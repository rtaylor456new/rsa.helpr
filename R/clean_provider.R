#' Clean TRT Scores Data, with Provider Variables Focus
#'
#' This function cleans a scores dataset, based on the standard data
#'   structure, returning a dataset focused on provider-variable analysis.
#'
#' @param data The TRT scores dataset.
#' @param state_filter A character vector identifying the state(s) of interest.
#'   Defaults to NULL.
#' @param id_col Differing variable naming structure for participant ID.
#'   (Eg. "X", or another name not similar to "participant" or "ID".)
#'   Defaults to NULL.
#' @param remove_questions Defaults to TRUE. When TRUE, removes question item
#'   variables.
#' @param condense Defaults to FALSE. When true, take medians across
#'  participants in order to keep only unique combinations of provider and
#'  service.
#' @param clean_id TRUE or FALSE. Defaults to TRUE, when TRUE, rows where
#'   participant ID is missing are removed.
#'
#' @returns A cleaned data frame, including only provider-relevant variables,
#'   restructured.
#'
#' @export
#' @import data.table
clean_provider <- function(data, state_filter = NULL, id_col = NULL,
                           remove_questions = TRUE, condense = FALSE,
                           clean_id = TRUE) {
  # Convert to data.table
  setDT(data)

  state <- grep("(?i)^(?=.*state)|(?=.*\\bst\\b)(?!.*\\bst\\B)",
                names(data), value = TRUE, perl = TRUE)

  # this code is a bit redundant, but I'll leave it in for now..
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
    message <- paste0("There is no state-identifying variable in this ",
                      "dataset. Cleaning process will continue on.")
    warning(message)
  } # Else carry on (without warning message)


  if (!is.null(id_col)) {
    participant <- id_col
  } else {
    # do some necessary renaming of variables
    participant <- grep("(?i)^(?=.*participant)|(?=.*\\bid\\b)(?!.*\\bid\\B)",
                        names(data), value = TRUE, perl = TRUE)
  }

  if (length(participant) < 1) {
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

  # Now, we can convert participant to numeric, in order to sort
  data[, (participant) := as.numeric(get(participant))]

  # Remove rows where participant ID is NA if clean_id = TRUE
  if (clean_id) {
    data <- data[!is.na(get(participant))]
  }

  # Next, order by participant
  data <- data[order(get(participant))]

  # Identify other column names
  provider <- grep("(?i)provider", names(data), value = TRUE, perl = TRUE)
  service <- grep("(?i)serv", names(data), value = TRUE, perl = TRUE)
  proctor <- grep("(?i)proctor", names(data), value = TRUE, perl = TRUE)
  mode <- grep("(?i)mode", names(data), value = TRUE, perl = TRUE)
  pre_post <- grep("(?i)^(?=.*pre)(?=.*post)",
                   names(data), value = TRUE, perl = TRUE)
  completed <- grep("(?i)^(?=.*complete)|(?=.*date)",
                    names(data), value = TRUE, perl = TRUE)
  caseload <- grep("(?i)^(?=.*case)|(?=.*caseload)|(?=.*workload)",
                   names(data), value = TRUE, perl = TRUE)
  group_freq <- grep("(?i)^(?=.*group)|(?=.*grp)(?=.*freq)",
                     names(data), value = TRUE, perl = TRUE)
  online_freq <- grep("(?i)^(?=.*online)(?=.*freq)",
                      names(data), value = TRUE, perl = TRUE)
  rural_freq <- grep("(?i)^(?=.*rural)(?=.*freq)",
                     names(data), value = TRUE, perl = TRUE)

  question_cols <- grep("(?i)(q_|question)", names(data), value = TRUE,
                        perl = TRUE)


  # Remove question item columns if specified
  if (remove_questions && length(question_cols) > 0) {
    # then remove question item columns
    data[, (question_cols) := NULL]
    # Else if user identifies to remove question items and there are none,
  } else if (remove_questions && length(question_cols) < 1) {
    # then return a warning, but continue with cleaning process
    message <- paste0("There are no question item variables in this dataset. ",
                      "Cleaning process will continue on.")
    warning(message)
  } # Else carry on (without warning message)


  # Rename columns for consistency
  names(data)[names(data) %in% participant] <- "Participant_ID"
  names(data)[names(data) %in% state] <- "State"
  names(data)[names(data) %in% provider] <- "Provider"
  names(data)[names(data) %in% service] <- "Service"
  names(data)[names(data) %in% proctor] <- "Proctor"
  names(data)[names(data) %in% mode] <- "Mode"
  names(data)[names(data) %in% pre_post] <- "Pre_Post"
  names(data)[names(data) %in% completed] <- "Completed"
  names(data)[names(data) %in% caseload] <- "Caseload"
  names(data)[names(data) %in% group_freq] <- "Group_freq"
  names(data)[names(data) %in% online_freq] <- "Online_freq"
  names(data)[names(data) %in% rural_freq] <- "Rural_freq"


  ## CONVERT VARIABLES

  # Clean up Provider (remove stuff in parentheses)
  data[, Provider := sub("\\s*\\([^\\)]+\\)", "", Provider)]

  # Remove "(MST)" and convert 'Completed' to POSIXct
  # and maintain correct MST time zone
  data[, Completed := as.POSIXct(gsub(" \\(MST\\)", "", Completed),
                                 format = "%m/%d/%Y %H:%M:%S",
                                 tz = "America/Denver")]

  ## NOMINAL FACTORS
  # Convert certain variables to basic/nominal factors
  factor_cols <- c("Participant_ID", "Provider", "Service")

  # Add "State" if it exists in the dataset
  if ("State" %in% names(data)) {
    factor_cols <- c(factor_cols, "State")
  }

  if ("Proctor" %in% names(data)) {
    factor_cols <- c(factor_cols, "Proctor")
  }

  data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

  # Clean and convert Caseload
  # Nominal: general, specialized, NULL
  # Clean and convert Caseload (Nominal)
  data[, Caseload := tolower(Caseload)]
  data[, Caseload := ifelse(Caseload %in% c("general", "specialized"),
                            Caseload, NA_character_)]
  data[, Caseload := factor(Caseload, levels = c("general", "specialized"))]

  ## ORDINAL FACTORS

  # Clean and convert frequency variables
  # Ordinal: never > rarely > sometimes > often > always, NULL
  freq_levels <- c("never", "rarely", "sometimes", "often", "always")
  frequency_vars <- c("Group_freq", "Online_freq", "Rural_freq")
  for (var in frequency_vars) {
    data[[var]] <- tolower(data[[var]])
    data[[var]] <- factor(data[[var]], levels = freq_levels, ordered = TRUE)
  }

  # Clean and convert Mode
  # Ordinal: no help > observer > with help, NULL
  mode_levels <- c("no help", "observer", "with help")
  data[, Mode := tolower(Mode)]
  data[, Mode := factor(Mode, levels = mode_levels, ordered = TRUE)]

  # Clean and convert Pre_Post
  # Ordinal pre > post
  pre_post_levels <- c("pre", "post")
  data[, Pre_Post := tolower(Pre_Post)]
  data[, Pre_Post := factor(Pre_Post, levels = pre_post_levels, ordered = TRUE)]


  # Keep only the most recent score per Participant, Provider, Service, Pre and
  #   Post score
  data <- data[order(Participant_ID, Provider, Service, Pre_Post, -Completed)]
  data <- data[, .SD[1], by = .(Participant_ID, Provider, Service, Pre_Post)]

  data[, c("Completed", "Participant_ID") := NULL]


  # Condense the data if 'condense = TRUE'
  if (condense) {
    data <- data[, .(Score = median(Score, na.rm = TRUE),
                     Difference = median(Difference, na.rm = TRUE),
                     Mode = names(sort(table(Mode), decreasing = TRUE))[1],
                     Caseload = names(sort(table(Caseload),
                                           decreasing = TRUE))[1],
                     Group_freq = names(sort(table(Group_freq),
                                             decreasing = TRUE))[1],
                     Online_freq = names(sort(table(Online_freq),
                                              decreasing = TRUE))[1],
                     Rural_freq = names(sort(table(Rural_freq),
                                             decreasing = TRUE))[1]),
                 by = .(Provider, Service, Pre_Post)]
  }

  # Sort by Provider, Service, and Pre_Post
  setorder(data, Provider, Service, Pre_Post)

  return(data)
}
