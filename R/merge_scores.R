#' Clean TRT Scores Data
#'
#' This function cleans a TRT scores dataset, based on the standard data
#'   structure.
#'
#' @param data The TRT scores dataset.
#' @param state_filter A character vector identifying the state(s) of interest.
#'   Defaults to NULL.
#' @param clean_id TRUE or FALSE. Defaults to TRUE, when TRUE, rows where
#'   participant ID is missing are removed.
#' @param aggregate TRUE or FALSE. Defaults to TRUE, when TRUE, rows are
#'   aggregated to include only unique participants are kept.
#' @param id_col Differing variable naming structure for participant ID.
#'   (Eg. "X", or another name not similar to "participant" or "ID".)
#'   Defaults to NULL.
#'
#' @returns A cleaned data frame, restructured to a wide format, to help with
#'   merging process.
#'
#' @export
#' @import data.table
#' @import stats
#'
clean_scores <- function(data, state_filter = NULL, clean_id = TRUE,
                         aggregate = TRUE, id_col = NULL) {

  # Convert to data.table format
  setDT(data)

  # look for a state variable in the data
  state <- grep("(?i)^(?=.*state)|(?=.*\\bst\\b)(?!.*\\bst\\B)",
                names(data), value = TRUE, perl = TRUE)

  if (length(state) > 0) {
    names(data)[names(data) %in% state] <- "State"
  }

  # If user provides state_filter and there is a state variable in data,
  if (!is.null(state_filter) && length(state) > 0) {
    # then filter by state(s) provided by user
    data <- data[get(state) %in% state_filter]
    # Else if user identifies states to filter by but there is no state
    #   variable,
  } else if (!is.null(state_filter) && length(state) < 1) {
    # then return a warning, but continue with cleaning process
    message <- paste0("There is no state-identifying variable in this dataset.",
                      " Cleaning process will continue on.")
    warning(message)
  } else if (is.null(state_filter) && length(state) > 0) {
    unique_states <- unique(na.omit(data[[state]]))
    if (length(unique_states) > 1) {
      message <- paste0("There are multiple states with overlapping ",
                        "Participant IDs. State abbreviations will be ",
                        "appended to Participant ID")
      warning(message)
    }
    # Save the rest for AFTER we clean Participant ID. I don't want to put
    #   the cleaning of ID before this code, because if the user chooses to
    #   filter out by state, it will save time to filter by state first.
  }


  # Clean and check Participant ID
  if (!is.null(id_col)) {
    participant <- id_col
  } else {
    # do some necessary renaming of variables
    participant <- grep("(?i)^(?=.*participant)|(?=.*\\bid\\b)(?!.*\\bid\\B)",
                        names(data), value = TRUE, perl = TRUE)
  }

  if (length(participant) == 0) {
    message <- paste0("No participant ID column found. Data must include an ",
                      "ID column for this cleaning process.")
    stop(message)
  }

  # Process the Participant ID variable
  data[, (participant) := fifelse(
    # If value is mixed numeric and non-numeric
    grepl("\\d", get(participant)) & grepl("\\D", get(participant)),
    # then keep only numeric parts
    gsub("\\D", "", get(participant)),
    # If value is fully non-numeric (which includes NAs)
    fifelse(
      grepl("\\D", get(participant)),
      # then convert the value to NA
      NA_character_,
      # Ensure the numbers stay character, so that all output is in same style
      #.  this is necessary to avoid errors
      as.character(get(participant))
    )
  )]

  # Now, we can convert participant to numeric, in order to sort -- perhaps
  #   unnecessary
  data[, (participant) := as.numeric(get(participant))]

  # Remove rows where participant ID is NA if clean_id = TRUE
  if (clean_id) {
    data <- data[!is.na(get(participant))]
  }

  # Rename variable for easier referencing
  names(data)[names(data) %in% participant] <- "Participant_ID"

  # If we found unique_states > 0, now is when we will append the state abbrev.
  #   to the IDs.
  if (is.null(state_filter) && length(state) > 0) {
    unique_states <- unique(na.omit(data[[state]]))
    if (length(unique_states) > 1) {
      state_abbrevs <- setNames(state.abb, state.name)
      # Function to get state abbreviation or use the first two letters if
      #    missing
      abbreviate_state <- function(state) {
        ifelse(state %in% names(state_abbrevs), state_abbrevs[state],
               substr(state, 1, 4))
      }
      # Apply the abbreviation function and prepend it to Participant_ID
      data[, State_Abbrev := abbreviate_state(State)]
      data[, Participant_ID := paste0(State_Abbrev, "_", Participant_ID)]
      data[, State_Abbrev := NULL]  # Remove temporary abbreviation column
    }
  }

  # sort by Participant ID
  data[order(Participant_ID)]

  # Identify variables in the dataset
  pre_post <- grep("(?i)^(?=.*pre)(?=.*post)",
                   names(data), value = TRUE, perl = TRUE)
  provider <- grep("(?i)provider", names(data), value = TRUE, perl = TRUE)
  service <- grep("(?i)service", names(data), value = TRUE, perl = TRUE)
  completed <- grep("(?i)complete|(?i)date", names(data), value = TRUE,
                    perl = TRUE)
  score <- grep("(?i)((?=.*\\bscore\\b))|(?i)test_score", names(data),
                value = TRUE, perl = TRUE)
  difference <- grep("(?i)^difference$|^difference_score$|^diff_score$",
                     names(data), value = TRUE, perl = TRUE)

  # newly accessible variable
  mode <- grep("(?i)mode", names(data), value = TRUE, perl = TRUE)

  # Rename other variables as desired:
  names(data)[names(data) %in% pre_post] <- "Pre_Post"
  names(data)[names(data) %in% provider] <- "Provider"
  names(data)[names(data) %in% service] <- "Service"
  names(data)[names(data) %in% completed] <- "Completed"
  names(data)[names(data) %in% score] <- "Score"
  names(data)[names(data) %in% difference] <- "Difference"
  # other variables
  names(data)[names(data) %in% state] <- "State"
  names(data)[names(data) %in% mode] <- "Mode"

  # Clean Provider names--
  #   (Remove special characters, abbreviate unless already abbreviated)
  data[, Provider := handle_abbrev(Provider)]

  # Remove "(MST)" and convert 'Completed' to POSIXct
  #   (and maintain correct MST time zone)
  data[, Completed := as.POSIXct(gsub(" \\(MST\\)", "", Completed),
                                 format = "%m/%d/%Y %H:%M:%S",
                                 tz = "America/Denver")]

  # Group by Participant_ID, Service, Pre_Post and calculate the count--
  #   this counts how many records for each pre and post score per service each
  #   participant has (we expect it to be 1 per person for most participants)
  data[, count := .N, by = .(Participant_ID, Service, Pre_Post)]

  # Create Has_Multiple_Scores binary column, based on count variable
  #. This allows us to record which participants took tests multiple times.
  data[, Has_Multiple_Scores := as.integer(count > 1)]

  # Remove the count column
  data[, count := NULL]

  # Calculate overall Has_Multiple_Scores per participant, to append after
  #   the rest of cleaning, merge and reformatting steps
  overall_scores <- data[, .(Has_Multiple_Scores = max(Has_Multiple_Scores)),
                         by = Participant_ID]

  # Convert certain variables to factors
  factor_cols <- c("Participant_ID", "Provider", "Service",
                   "Has_Multiple_Scores")

  data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]


  # Filter Pre and Post data
  pre_data <- data[Pre_Post == "Pre"]
  post_data <- data[Pre_Post == "Post"]

  # Rename variables in pre_data for less confusing merge later
  setnames(pre_data, "Score", "Pre_Score")
  setnames(pre_data, "Completed", "Pre_Date")

  # Add "State" and "Mode" if they exist in the dataset
  if ("State" %in% names(pre_data)) {
    setnames(pre_data, "State", "Pre_State")
  }
  if ("Mode" %in% names(pre_data)) {
    setnames(pre_data, "Mode", "Pre_Mode")
  }


  # Rename variables in post_data for less confusing merge later
  setnames(post_data, "Score", "Post_Score")
  setnames(post_data, "Completed", "Post_Date")

  # Add "State" and "Mode" if they exist in the dataset
  if ("State" %in% names(post_data)) {
    setnames(post_data, "State", "Post_State")
  }
  if ("Mode" %in% names(post_data)) {
    setnames(post_data, "Mode", "Post_Mode")
  }


  if (aggregate) {
    # Aggregate Pre and Post data
    pre_data <- pre_data[order(Pre_Date), .SD[1],
                         by = .(Participant_ID, Service)]

    # post_data <- post_data[order(-Completed), .SD[1],
    #                        by = .(Participant_ID, Provider, Service)]
    post_data <- post_data[order(-Post_Date), .SD[1],
                           by = .(Participant_ID, Service)]
  }

  # Specify the required columns
  required_cols_pre <- c("Participant_ID", "Service", "Pre_Date", "Provider",
                         "Pre_Score")

  required_cols_post <- c("Participant_ID", "Service", "Post_Date", "Provider",
                          "Post_Score")

  # Add "State" and "Mode" if they exist in the dataset
  if ("Pre_State" %in% names(pre_data) && "Post_State" %in% names(post_data)) {
    required_cols_pre <- c(required_cols_pre, "Pre_State")
    required_cols_post <- c(required_cols_post, "Post_State")
  }
  if ("Pre_Mode" %in% names(pre_data) && "Post_Mode" %in% names(post_data)) {
    required_cols_pre <- c(required_cols_pre, "Pre_Mode")
    required_cols_post <- c(required_cols_post, "Post_Mode")
  }

  # Extract needed columns from pre_data
  pre_selected <- pre_data[, ..required_cols_pre]
  # (Difference has no values for pre-score rows, so we don't include it here)

  # Extract needed columns from post_data
  post_selected <- post_data[, c(required_cols_post, "Difference"),
                             with = FALSE]
  # with=FALSE handles the format of the function argument for columns that
  #  is weird in data.table


  # Merge the pre and post data

  # Merge using a full outer join (It ensures that all rows from both
  #   pre_selected and post_selected will be included in the merged result,
  #   even if they do not have matching rows in the other data table.)
  merged_data <- merge(pre_selected, post_selected,
                       by = c("Participant_ID", "Service"),
                       all = TRUE)
  # Because we expect each participant to have the same provider per service,
  #   we only care about one provider per unique combo of participant and
  #   service. This merge creates two columns, Provider.x and Provider.y (one
  #   from pre_selected and one from post_selected)

  # Identify the most common non-NA provider for each participant -- this
  #. should result in a dataframe with two columns, Participant_ID and
  #  Most_Common_Provider, with just one row per unique participant
  common_provider <- merged_data[!is.na(Provider.x) | !is.na(Provider.y),
                                 .(Most_Common_Provider =
                                     names(sort(table(c(Provider.x,
                                                        Provider.y)),
                                                decreasing = TRUE)[1])),
                                 by = Participant_ID]

  # Do the same thing for State and Mode
  # Most common State
  common_state <- merged_data[!is.na(Pre_State) | !is.na(Post_State),
                              .(Most_Common_State = names(sort(table(c(Pre_State, Post_State)),
                                                               decreasing = TRUE)[1])),
                              by = Participant_ID
  ]

  # Most common Mode
  common_mode <- merged_data[!is.na(Pre_Mode) | !is.na(Post_Mode),
                             .(Most_Common_Mode = names(sort(table(c(Pre_Mode, Post_Mode)),
                                                             decreasing = TRUE)[1])),
                             by = Participant_ID
  ]

  # Merge all back into main dataset
  merged_data <- merge(merged_data, common_provider, by = "Participant_ID",
                       all.x = TRUE)
  merged_data <- merge(merged_data, common_mode, by = "Participant_ID",
                       all.x = TRUE)
  merged_data <- merge(merged_data, common_state, by = "Participant_ID",
                       all.x = TRUE)

  # Drop old columns
  merged_data[, c("Provider.x", "Provider.y", "Pre_Mode", "Post_Mode",
                  "Pre_State", "Post_State") := NULL]

  # Rename new columns
  setnames(merged_data, c("Most_Common_Provider", "Most_Common_Mode",
                          "Most_Common_State"),
           c("Provider", "Mode", "State"))


  ## This is the new code to try to prevent an error in merge ##
  # Convert Participant_ID to factor in both data.tables to ensure the
  #.  final_data merge works
  merged_data[, Participant_ID := as.factor(Participant_ID)]
  overall_scores[, Participant_ID := as.factor(Participant_ID)]

  # Merge with overall_scores to get correct Has_Multiple_Scores
  final_data <- merge(merged_data, overall_scores, by = "Participant_ID",
                      all.x = TRUE)


  # We should now only have 1 row per unique combination of Participant_ID and
  #   Service


  # Calculate Time_Passed_Days, the number of days between the Pre_Date and
  #   Post_Date for each participant's tests.
  final_data[, Time_Passed_Days := as.numeric(difftime(Post_Date,
                                                       Pre_Date,
                                                       units = "days")),
             by = .(Participant_ID, Service)]
  # round to the nearest day
  final_data[, Time_Passed_Days := round(Time_Passed_Days)]

  # (Note that some Time_Passed_Days values are negative--this is an issue with
  #   collected data, not with cleaning process.)

  ## RESHAPE
  # First, identify columns to keep long, instead of wide.
  cols_to_include <- c("Participant_ID", "Provider", "Has_Multiple_Scores")

  # Check if 'State' and 'Mode' columns exist in the dataset
  # Conditionally add 'State' and 'Mode' if they exist in the dataset
  if ("State" %in% names(final_data)) {
    cols_to_include <- c(cols_to_include, "State")
  }
  if ("Mode" %in% names(final_data)) {
    cols_to_include <- c(cols_to_include, "Mode")
  }

  # Reshape the data from long to wide format
  scores_final <- dcast(
    final_data,
    formula = paste(paste(cols_to_include, collapse = " + "), "~ Service"),
    value.var = c("Pre_Score", "Post_Score", "Difference", "Time_Passed_Days",
                  "Pre_Date", "Post_Date"),
    sep = "_"
  )

  # Now, we should finally have ONE row per participant.

  pre_score_cols <- grep("^Pre_Score", names(scores_final), value = TRUE)
  post_score_cols <- grep("^Post_Score", names(scores_final), value = TRUE)
  difference_cols <- grep("^Difference_", names(scores_final), value = TRUE)
  time_cols <- grep("^Time_Passed_Days", names(scores_final), value = TRUE)

  # Calculate Differences_Available -- the count of difference scores available
  #   per participant
  scores_final[, Differences_Available := rowSums(!is.na(.SD)),
               .SDcols = difference_cols, by = Participant_ID]

  # Calculate Median columns:

  # Median_Pre_Score -- the median per participant across all of their pre
  #   scores
  if (length(pre_score_cols) > 0) {
    scores_final[, Median_Pre_Score := median(unlist(.SD), na.rm = TRUE),
                 .SDcols = pre_score_cols, by = Participant_ID]
  } else {
    scores_final[, Median_Pre_Score := NA_real_]
  }

  # Median_Post_Score -- the median per participant across all of their post
  #   scores
  if (length(post_score_cols) > 0) {
    scores_final[, Median_Post_Score := median(unlist(.SD), na.rm = TRUE),
                 .SDcols = post_score_cols, by = Participant_ID]
  } else {
    scores_final[, Median_Post_Score := NA_real_]
  }

  # Median_Difference_Score -- the median per participant across all of their
  #   difference values.
  if (length(difference_cols) > 0) {
    scores_final[, Median_Difference_Score := median(unlist(.SD), na.rm = TRUE),
                 .SDcols = difference_cols, by = Participant_ID]
  } else {
    scores_final[, Median_Difference_Score := NA_real_]
  }

  # Median_Time_Passed_Days -- the median per participant across all of their
  #   time passed values.
  if (length(time_cols) > 0) {
    scores_final[, Median_Time_Passed_Days := median(unlist(.SD), na.rm = TRUE),
                 .SDcols = time_cols, by = Participant_ID]
  } else {
    scores_final[, Median_Time_Passed_Days := NA_real_]
  }

  return(scores_final)
}
