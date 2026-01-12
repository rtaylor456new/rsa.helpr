#' Clean Utah RSA-911 Data
#'
#' This function cleans a Utah RSA-911 dataset, based on the standard data
#'   structure.
#'
#' @param data The Utah RSA-911 dataset.
#' @param aggregate TRUE or FALSE. Defaults to TRUE, when TRUE, rows are
#'   aggregated to include only unique combinations of participants, year,
#'   and quarter.
#' @param unidentified_to_0 TRUE or FALSE. Defaults to TRUE, when TRUE,
#'   variables where unidentified is represented by 9,
#'   values are converted to 0.
#' @param remove_desc TRUE or FALSE. Defaults to TRUE, when TRUE, description
#'   variables are removed to minimize redundancy.
#' @param remove_strictly_na TRUE or FALSE. Defaults to TRUE, when TRUE,
#'   variables that contain only NA values are removed.
#' @param clean_specials A character vector of name(s) of variables with special
#'   characters to be cleaned. New, separate variables for each value space will
#'   be appended to end of dataset with the the following naming convention:
#'   original_variable_name_Place1, original_variable_name_Place2, etc.
#'   Defaults to NULL. Note that if many are listed, cleaning process will be
#'   very slow.
#'
#' @returns A cleaned data frame, with incorrect, blank, messy values replaced,
#'   additional, helpful variables created, and unnecessary variables removed.
#'
#' @export
#' @import data.table

clean_utah <- function(data,
                       aggregate = TRUE,
                       unidentified_to_0 = TRUE,
                       remove_desc = TRUE,
                       remove_strictly_na = TRUE,
                       clean_specials = NULL) {

  # Ensure data is a data.table
  setDT(data)

  ##############################################################################
  ######################
  ## REMOVABLE        ##
  ######################
  # Rows where Participant_ID is missing
  participant_col <- grep(
    paste0("(?i)^(?=.*participant|.*\\bid\\b)(?!.*\\bid\\B)(?!.*ssn)"),
    names(data),
    value = TRUE,
    perl = TRUE
  )


  # Clean rows where participant ID is missing, NULL, "NA", "NULL",
  #   blank, or whitespace
  data <- data[
    !(
      is.na(get(participant_col)) |
        get(participant_col) %in% c("NA", "NULL") |
        trimws(get(participant_col)) == ""
    )
  ]


  # DESCRIPTION columns - unnecessary for analysis
  if (remove_desc) {
    desc_cols <- grep("(?i)_desc", names(data), value = TRUE, perl = TRUE)
    if (length(desc_cols) > 0) {
      data[, (desc_cols) := NULL]
      gc()  # Trigger garbage collection after removing description columns
    } else {
      warning("No description columns to remove.")
    }
  }

  # ADMIN columns
  # remove the extra administrative columns, not needed for analysis
  extra_cols <- grep("(?i)_data_", names(data), value = TRUE, perl = TRUE)

  if (length(extra_cols) > 0) {
    data[, (extra_cols) := NULL]
    gc() # Trigger garbage collection after removing admin columns
  }

  # Additional miscellaneous columns to remove
  columns_to_remove <- c("X", "x", "V1")

  # Check if the columns exist in the data before removing them
  existing_columns <- columns_to_remove[columns_to_remove %in% names(data)]

  # Remove only the columns that exist
  if (length(existing_columns) > 0) {
    data[, (existing_columns) := NULL]
  }


  ##############################################################################
  ######################
  ## DATE             ##
  ######################
  # DATE columns - in excel date format
  # columns with start, extension, end dates
  date_cols <- grep(paste0("(?i)_date|(?i)_skill_gain|(?i)_start|(?i)_end_|",
                           "(?i)_extension(?!.*(?i)_desc)"),
                    names(data), value = TRUE,
                    perl = TRUE)

  data[, (date_cols) := lapply(.SD, handle_mixed_date), .SDcols = date_cols]

  # When handling date transformations (like Excel dates), you might generate
  #  intermediate results that can be garbage collected:
  gc()


  ##############################################################################
  ######################
  ## NUMERIC          ##
  ######################

  # E1_Year_911
  year_col <- grep("(?i)_year|(?i)_yr_(?!.*(?i)_desc)", names(data),
                   value = TRUE, perl = TRUE)


  # E2_Quarter_911
  quarter_col <- grep("(?i)_quarter|(?i)_qt_(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

  # AGE column
  age_cols <- grep("(?i)^(?=.*age)(?=.*app)(?!.*(desc|amt))", names(data),
                   value = TRUE, perl = TRUE)
  # Convert to numeric
  data[, (age_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
       .SDcols = age_cols]

  # Combine the age columns into one, call it "Age_At_Application"
  data[, Age_At_Application := round(do.call(fcoalesce, .SD)),
       .SDcols = age_cols]

  # Drop the original column names
  data[, (age_cols) := NULL]


  # AMT and TITLE columns (TITLEI columns are funds expended for different
  #   services)
  # WAGE columns
  amt_cols <- grep(paste0("(?i)_amt|(?i)_title|(?i)_wage|(?i)_amount|",
                          "(?i)_amnt(?!.*(?i)_desc)"),
                   names(data), value = TRUE, perl = TRUE)

  # HOURS columns
  hours_cols <- grep("(?i)_hours_|(?i)_hrs_(?!.*(?i)_desc)", names(data),
                     value = TRUE, perl = TRUE)

  numeric_cols <- c(year_col, quarter_col, "Age_At_Application",
                    amt_cols, hours_cols)

  # Convert to numeric, ignoring warnings about NAs--this is what we want.
  data[, (numeric_cols) :=
         lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
       .SDcols = numeric_cols]


  ### Calculate Birth Year and create Has_App_Date
  app_date_col <- grep("(?i)(?=.*app)(?=.*date)(?!.*_desc)", names(data),
                       value = TRUE, perl = TRUE)

  if (length(app_date_col) == 1) {
    data[, Birth_Year := year(data[[app_date_col]]) -
           data[["Age_At_Application"]]]
  }

  # Create Has_App_Date as 1 if not missing, 0 if missing
  if (length(app_date_col) == 1) {
    data[, Has_App_Date := fifelse(!is.na(get(app_date_col)), 1, 0)]
  } else {
    warning("Application date column not uniquely identified.")
  }

  # add to numeric columns list, so that it doesn't get counted as a factor
  #   later
  numeric_cols <- c(numeric_cols, "Birth_Year")

  # Converting columns to numeric can generate intermediate objects, which may
  #   linger in memory.
  gc()


  ##############################################################################
  ######################
  ## FACTORS: nominal ##
  ######################
  # CODE columns
  # different kinds of values--zip code, agency code, etc. -- all factors
  code_cols <- grep(paste0("((?i)_code_|(?i)_plan_occ|(?i)_referral_source|",
                           "(?i)_exit_occ)(?!.*(?i)_desc)"),
                    names(data), value = TRUE, perl = TRUE)

  data[, (code_cols) := lapply(.SD, handle_blanks),
       .SDcols = code_cols]


  # VENDOR columns - should be values 1,2,3,4 or blank
  vendor_cols <- grep("(?i)_vendor(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

  data[, (vendor_cols) := lapply(.SD, function(x) {
    numeric_x <- as.numeric(as.character(x))
    handle_values(numeric_x, values = c(1, 2, 3, 4))
  }), .SDcols = vendor_cols]

  #######################################
  ### 1, 0, 9 columns: ###
  # PROVIDER and PURCHASE columns
  prov_purch_cols <- grep("((?i)_provide|(?i)_purchase)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)

  # DEMOGRAPHIC columns - 0, 1, 9
  race_cols <- grep(paste0("(?i)(_indian|_asian|_black|_hawaiian|_islander|",
                           "_white|hispanic)(?!.*(?i)_desc)"),
                    names(data),
                    value = TRUE, perl = TRUE)
  # [1] "E10_Indian_Alaskan_911"            "E11_Asian_911"
  # [3] "E12_Black_African_911"             "E13_Hawaiian_Pacific_Islander_911"
  # [5] "E14_White_911"                     "E15_Hispanic_Latino_911"

  veteran_col <- grep(paste0("(?i)veteran(?!.*(_desc|description|_amt|amount|",
                             "amnt|vendor|title|comp))"),
                      names(data), value = TRUE, perl = TRUE)
  # [1] "E16_Veteran_Status_911"

  has_disability_col <- grep(paste0("(?i)(has_disability)(?!.*(_desc|",
                                    "description|_amt|amount|amnt|vendor|title",
                                    "|comp|hours|date|ext|wage))"),
                             names(data), value = TRUE, perl = TRUE)
  # [1] "E42_Has_Disability_911"

  q1_q1_employ_col <- grep(paste0("(?i)(employer_match|q1_q2_match|",
                                  "q1_q2_employ)(?!.*(_desc|description|_amt|",
                                  "amount|amnt|vendor|title|comp|hours|date|",
                                  "ext|wage))"),
                           names(data), value = TRUE, perl = TRUE)
  # [1] "E392_Q2_Q4_Employer_Match_911"

  adult_cols <- grep(paste0("(?i)adult(?!.*(_desc|description|_amt|amount|amnt",
                            "|vendor|title|comp|hours|date|ext|wage))"),
                     names(data), value = TRUE, perl = TRUE)
  # [1] "E54_Adult_911"           "E55_Adult_Education_911"

  service_work_cols <- grep(paste0("(?i)(dislocated_worker|job_corps|voc_rehab",
                                   "|wagner_peyser|youth|equivalent|se_goal)",
                                   "(?!.*(_desc|description|_amt|amount|amnt|",
                                   "vendor|title|comp|hours|date|ext|wage))"),
                            names(data), value = TRUE, perl = TRUE)
  # [1] "E49_SE_Goal_911"
  # [2] "E56_Dislocated_Worker_911"
  # [3] "E57_Job_Corps_911"
  # [4] "E58_Voc_Rehab_911"
  # [5] "E59_Wagner_Peyser_911"
  # [6] "E60_Youth_911"
  # [7] "E61_YouthBuild_911"
  # [8] "E400_Secondary_Equivalent_Enrollment_911"

  plan_cols <- grep(paste0("(?i)plan_(?!.*(_desc|description|_amt|amount|amnt|",
                           "vendor|title|comp|hours|date|ext|wage|status|occ|",
                           "grade|farm))"),
                    names(data), value = TRUE, perl = TRUE)
  # [1] "E62_Plan_Long_Term_Unemployment_911" "E63_Plan_Exhaust_TANF_911"
  # [3] "E64_Plan_Foster_Care_911"            "E65_Plan_Homeless_911"
  # [5] "E66_Plan_Offender_911"               "E67_Plan_Low_Income_911"
  # [7] "E68_Plan_English_Learner_911"        "E69_Plan_Skills_Deficient_911"
  # [9] "E70_Plan_Cultural_Barriers_911"      "E71_Plan_Single_Parent_911"
  # [11] "E72_Plan_Displaced_Homemaker_911"


  post_sec_complete_col <- grep(paste0("(?i)(?=.*postsecondary)(?=.*complet)",
                                       "(?!.*(_desc|_date))"),
                                names(data), value = TRUE, perl = TRUE)


  demographic_cols <- c(race_cols, veteran_col, has_disability_col,
                        q1_q1_employ_col, adult_cols, service_work_cols,
                        plan_cols)

  binary_cols <- c(prov_purch_cols, demographic_cols, post_sec_complete_col)

  data[, (binary_cols) :=
         lapply(.SD, function(x) handle_nines(x, unidentified_to_0)),
       .SDcols = binary_cols]


  ## FARMWORKER --> BINARY
  farm_work_col <- grep(paste0("(?i)plan_*(?i)_farm(?!.*(_desc|description|",
                               "_amt|amount|amnt|vendor|title|comp|hours|date",
                               "|ext|wage|status|occ|",
                               "grade))"),
                        names(data), value = TRUE, perl = TRUE)

  data[, (farm_work_col) := lapply(.SD, function(x) {
    numeric_x <- as.numeric(as.character(x))
    handle_values(numeric_x, c(1:3, 9), blank_value = 0)
  }),
  .SDcols = farm_work_col]

  # Convert farmworker to binary variable for analysis
  data[, (farm_work_col) := lapply(.SD, function(x) ifelse(x %in% 1:3, 1, 0)),
       .SDcols = farm_work_col]


  ##############################################################################
  #############################
  ## BINARY GROUPING COLUMNS ##
  ############################

  income_cols <- grep(paste0("(?i)(unemployment|TANF|homeless|low_income|",
                             "single_parent|displaced_homemaker|farmworker)",
                             "(?!.*(_desc|_amt|amount|_title|_date))"),
                      names(data), value = TRUE, perl = TRUE)

  cultural_cols <- grep(paste0("(?i)(english_learner|skills_deficient|",
                               "cultural_barriers|offender)",
                               "(?!.*(_desc|_amt|amount|_title|_date))"),
                        names(data), value = TRUE, perl = TRUE)

  support_cols <- grep(paste0("(?i)(foster_care|low_income|single_parent|",
                              "displaced_homemaker)",
                              "(?!.*(_desc|_amt|amount|_title|_date))"),
                       names(data), value = TRUE, perl = TRUE)

  housing_cols <- grep(paste0("(?i)(foster_care|homeless)",
                              "(?!.*(_desc|_amt|amount|_title|_date))"),
                       names(data), value = TRUE, perl = TRUE)


  data[, Income_Struggle := as.integer(rowSums(.SD, na.rm = TRUE) > 0),
       .SDcols = income_cols]

  data[, Cultural_Struggle := as.integer(rowSums(.SD, na.rm = TRUE) > 0),
       .SDcols = cultural_cols]

  data[, Support_Struggle := as.integer(rowSums(.SD, na.rm = TRUE) > 0),
       .SDcols = support_cols]

  data[, Housing_Struggle := as.integer(rowSums(.SD, na.rm = TRUE) > 0),
       .SDcols = housing_cols]


  data[, Facing_Struggle := as.integer(
    rowSums(.SD, na.rm = TRUE) > 0),
    .SDcols = c("Income_Struggle", "Cultural_Struggle", "Support_Struggle",
                "Housing_Struggle")]


  ####################################

  # SEX column
  sex_cols <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                   value = TRUE, perl = TRUE)

  data[, (sex_cols) := lapply(.SD, function(x) {
    numeric_x <- as.numeric(as.character(x))
    handle_values(numeric_x, c(1, 2, 3, 9), blank_value = 9)
  }), .SDcols = sex_cols]



  # E355_Exit_Reason_911 - 2-19, NULL -- 02, 03, 04, 06, 07, 08, 13-22

  exit_reason_cols <- grep("(?i)_exit_reason(?!.*(?i)_desc)", names(data),
                           value = TRUE, perl = TRUE)

  data[, (exit_reason_cols) := lapply(.SD, function(x) {
    numeric_x <- as.numeric(as.character(x))
    handle_values(numeric_x, c(02, 03, 04, 06, 07, 08, 13:22))
  }),
  .SDcols = exit_reason_cols]


  # EMPLOYMENT columns
  # 1-4, 9, 0 --factors--potentially ordinal...
  employ_cols <- grep("(?i)_employ", names(data), value = TRUE, perl = TRUE)
  employ_cols <- employ_cols[!grepl("wage|match|desc", employ_cols,
                                    ignore.case = TRUE)]

  data[, (employ_cols) := lapply(.SD, function(x) {
    handle_values(x, c(0, 1, 2, 3, 4, 9), blank_value = 0)
  }),
  .SDcols = employ_cols]

  # Convert 9s to 0s as well, if user chooses to set unidentified_to_0 = TRUE
  data[, (employ_cols) := lapply(.SD, function(x) {
    numeric_x <- as.numeric(as.character(x))
    handle_nines(numeric_x, unidentified_to_0)
  }),
  .SDcols = employ_cols]


  # E356_Exit_Work_Status_911
  # this is our exit work status--this is the important variable
  exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                        names(data), value = TRUE, perl = TRUE)

  data[, (exit_work_col) := lapply(.SD, function(x) {
    numeric_x <- as.numeric(as.character(x))
    handle_values(numeric_x, c(1:5, 7), blank_value = 0)
  }),
  .SDcols = exit_work_col]


  # NEW VARIABLE: Final_Employment
  # Create a new variable for employment status, binary for analysis
  #  1: competitively employed
  #  0: non-competitively employed (other values)
  data[, Final_Employment := lapply(.SD, function(x) ifelse(x == 1, 1, 0)),
       .SDcols = exit_work_col]


  ##############################################################################
  ########################
  ## SPECIAL CHARACTERS ##
  ########################
  # E394_App_Public_Support_911 - 0, 1-4
  # E395_App_Medical_911 - 0, 1-7 - limit of 3 types
  # E396_Exit_Public_Support_911 - 0, 1-4
  # E397_Exit_Medical_911 - 0, 1-7 - limit of 3 types
  # E74_SWD_Age_911 - two values, ages

  special_cols <- grep(paste0("((?i)_app_pub|(?i)_app_med|(?i)_exit_pub|",
                              "(?i)_exit_med|(?i)_swd_age_)(?!.*(?i)_Desc)"),
                       names(data), value = TRUE, perl = TRUE)

  # COMP columns - can enter a max of 3 values
  comp_cols <- grep("(?i)_comp_(?!.*(?i)_desc)", names(data), value = TRUE,
                    perl = TRUE)
  comp_cols <- comp_cols[!grepl("provide|amt|date", comp_cols,
                                ignore.case = TRUE)]

  all_special_cols <- c(special_cols, comp_cols)

  # There are too many special character variables, so it's best to let the
  #   user decide which variables they want to clean
  if (!is.null(clean_specials) && length(all_special_cols) > 0) {
    data <- apply_handle_splits(data, clean_specials, sep = ";")
  }


  # NEW VARIABLE: DISABILITY columns
  # Separate disability columns into impairment and cause columns
  data <- separate_disability(data)

  # Now, create a variable for impairment groupings
  impairment_vars <- c("Primary_Impairment", "Secondary_Impairment")
  group_vars <- paste0(impairment_vars, "_Group")

  # Apply a grouping function to both columns at once using lapply
  data[, (group_vars) := lapply(.SD, handle_impairment_group),
       .SDcols = impairment_vars]

  ##############################################################################
  ########################
  ## Type conversion    ##
  ########################

  # for now, convert everything to factors except numeric_cols

  # Convert all other columns to factors
  factor_cols <- unique(setdiff(names(data), c(numeric_cols, date_cols)))
  data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

  ##############################################################################
  ######################
  ## FACTORS: ordinal ##
  ######################

  ### NEW: these seem ORDINAL!
  # E45_Disability_Priority_911 - 0, 1, 2, NULL (null means something different
  #                                               than 0)
  # E78_Secondary_Enrollment_911 - 0, 1,2, NULL (Nulls --> 0)

  other_factor_cols <- grep(paste0("((?i)_disability_priority|(?i)_secondary_",
                                   "enrollment)(?!.*(?i)_desc)"),
                            names(data),
                            value = TRUE, perl = TRUE)

  # Apply the handle_values function and convert to ordered factor

  data[, (other_factor_cols) := lapply(.SD, function(x) {
    numeric_x <- as.numeric(as.character(x))
    factor(handle_values(numeric_x, c(0, 1, 2)))
  }), .SDcols = other_factor_cols]



  # E84_PostSecondary_Enrollment_911
  post_sec_cols <- grep("(?i)_postsecondary_enroll(?!.*(?i)_desc|(?i)_date)",
                        names(data), value = TRUE, perl = TRUE)

  data[, (post_sec_cols) := lapply(.SD, function(x) {
    numeric_x <- as.numeric(as.character(x))  # convert "0" → 0, "1" → 1, etc.
    as.factor(handle_values(numeric_x, 0:3, blank_value = 0))
  }), .SDcols = post_sec_cols]

  # E354_Exit_Type_911
  exit_var_cols <- grep("((?i)_exit_type)(?!.*(?i)_desc|(?i)_date)",
                        names(data), value = TRUE, perl = TRUE)

  # (order is a little weird for this one)
  data[, (exit_var_cols) := lapply(.SD, function(x) {
    numeric_x <- as.numeric(as.character(x))
    factor(handle_values(numeric_x, 0:7, blank_value = 0),
           levels = c(0, 7, 1, 2, 3, 4, 5, 6),
           ordered = TRUE)
  }), .SDcols = exit_var_cols]



  # E378_PostExit_Credential_911
  exit_cred_cols <- grep("(?i)_postexit_credential(?!.*(?i)_desc|(?i)_date)",
                         names(data), value = TRUE, perl = TRUE)

  # (order is a little weird for this one too)
  data[, (exit_cred_cols) := lapply(.SD, function(x) {
    numeric_x <- as.numeric(as.character(x))
    factor(handle_values(numeric_x, 1:8, blank_value = 0),
           levels = c(8, 6, 7, 5, 1, 2, 3, 4),
           ordered = TRUE)
  }), .SDcols = exit_cred_cols]



  # NEW VARIABLE: Age_Group
  # Apply a grouping function to create a new column (includes factor
  #   conversion step)
  data[, Age_Group := handle_age_group(Age_At_Application)]


  ##############################################################################
  #######################
  ## DATA AGGREGATION  ##
  #######################

  if (aggregate) {

    ## FIRST, run some checks
    # Make sure we have necessary columns for cleaning processes

    participant_col <- grep(
      paste0("(?i)^(?=.*participant|.*\\bid\\b)(?!.*\\bid\\B)(?!.*ssn)"),
      names(data),
      value = TRUE,
      perl = TRUE
    )

    year_col <- grep("(?i)(?<!birth)_year|_yr_(?!.*_desc)", names(data),
                     value = TRUE, perl = TRUE)

    quarter_col <- grep("(?i)_quarter|(?i)_qt_(?!.*(?i)_desc)", names(data),
                        value = TRUE, perl = TRUE)

    app_date_col <- grep("(?i)_app.*?(date)(?!.*(?i)_desc)", names(data),
                         value = TRUE, perl = TRUE)

    required_cols <- c(participant_col, year_col, quarter_col, app_date_col)


    # Set a flag to determine if we should proceed with aggregation
    skip_aggregation <- FALSE


    ## CHECK 1: Ensure all required columns were found
    if (length(participant_col) < 1 ||
        length(year_col) < 1 ||
        length(quarter_col) < 1 ||
        length(app_date_col) < 1) {

      warning("Skipping aggregation. Missing required columns: ",
              paste(c(
                if (length(participant_col) < 1) "Participant ID",
                if (length(year_col) < 1) "Year",
                if (length(quarter_col) < 1) "Quarter",
                if (length(app_date_col) < 1) "Application Date"
              ), collapse = ", "))

      # Raise flag to skip aggregation
      skip_aggregation <- TRUE
    }

    ## CHECK 2
    # Check if essential columns have only NA values
    empty_cols <- sapply(data[, required_cols, with = FALSE],
                         function(x) all(is.na(x)))

    if (any(empty_cols)) {
      warning("Skipping aggregation. Some required columns have no data: ",
              paste(names(empty_cols[empty_cols]), collapse = ", "))

      # Raise flag to skip aggregation
      skip_aggregation <- TRUE
    }

    if (!skip_aggregation) {
      # Rename for consistency
      names(data)[names(data) %in% participant_col] <- "Participant_ID"
      names(data)[names(data) %in% year_col] <- "E1_Year_911"
      names(data)[names(data) %in% quarter_col] <- "E2_Quarter_911"
      names(data)[names(data) %in% app_date_col] <- "E7_Application_Date_911"

      # Create a non-missing count per row
      data[, NonMissingCount := rowSums(!is.na(.SD)),
           .SDcols = setdiff(names(data), c("Participant_ID", "E1_Year_911",
                                            "E2_Quarter_911",
                                            "E7_Application_Date_911"))]

      # Add a temporary column for ordering: application date if available, else
      #   NA
      data[, AppDateRank := fifelse(!is.na(E7_Application_Date_911),
                                    as.numeric(E7_Application_Date_911),
                                    NA_real_)]

      # Apply ordering logic:
      setorder(data, Participant_ID, E1_Year_911, E2_Quarter_911,
               -AppDateRank, -NonMissingCount)

      # Keep only the top row per group
      data <- data[, .SD[1], by = .(Participant_ID, E1_Year_911,
                                    E2_Quarter_911)]

      # Final sort
      setorder(data, E1_Year_911, E2_Quarter_911)

      # Clean up helper columns
      data[, c("NonMissingCount", "AppDateRank") := NULL]
    }

  }

  # REMOVE COLUMNS WITH ONLY NAs when remove_strictly_na = TRUE
  if (remove_strictly_na) {
    data <- data[, which(unlist(lapply(data, function(x) !all(is.na(x))))),
                 with = FALSE]
  }

  # Return the cleaned dataset
  return(data)

}
