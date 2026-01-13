#' Merge Scores Data with RSA-911 Data
#'
#' This function merges cleaned Utah scores data with cleaned Utah RSA-911 Data.
#'
#' @param quarterly_data The cleaned RSA-911 dataset.
#' @param scores_data The cleaned scores dataset.
#' @param quarterly_id A string for the variable name corresponding to
#'   participant ID in the cleaned RSA-911 dataset. Defaults to
#'   "Participant_ID".
#' @param scores_id A string for the variable name corresponding to
#'   participant ID in the cleaned scores dataset. Defaults to "Participant_ID".
#'
#' @returns A data frame with one row per participant.
#'
#' @export

merge_scores <- function(quarterly_data, scores_data,
                         quarterly_id = "Participant_ID",
                         scores_id = "Participant_ID") {

  # add in these two copy lines to fix the data.table-specific error about
  #    duplicate column names
  quarterly_data  <- copy(quarterly_data)
  scores_data <- copy(scores_data)

  setDT(quarterly_data)
  setDT(scores_data)

  # Ensure ID columns exist
  if (!(quarterly_id %in% colnames(quarterly_data))) {
    stop(paste("Error: Column", quarterly_id, "not found in quarterly_data."))
  }
  if (!(scores_id %in% colnames(scores_data))) {
    stop(paste("Error: Column", scores_id, "not found in scores_data."))
  }

  # Check for duplicate IDs
  if (anyDuplicated(quarterly_data[[quarterly_id]]) > 0) {
    message <- paste0("Warning: Duplicate IDs found in quarterly data. ",
                      "Try applying create_metadata next to condense.")
    warning(message)
  }

  if (anyDuplicated(scores_data[[scores_id]]) > 0) {
    message <- paste0("Warning: Duplicate IDs found in scores data. ",
                      "Try applying create_metadata next to condense.")
    warning(message)
  }

  # Perform inner join
  merged_data <- merge(quarterly_data, scores_data,
                       by.x = quarterly_id,
                       by.y = scores_id,
                       all = FALSE)

  setDT(merged_data)

  # Verify that only common IDs remain
  common_ids <- intersect(quarterly_data[[quarterly_id]],
                          scores_data[[scores_id]])
  merged_ids <- merged_data[[quarterly_id]]

  if (!setequal(merged_ids, common_ids)) {
    message <- paste0("Error: Inner join failed. The merged dataset does not ",
                      "contain only the overlapping IDs.")
    stop(message)
  }


  # # ---- Compute Age Variables ----
  # # Create a Birth Year column
  # if (!("Birth_Year" %in% colnames(merged_data))) {
  #   warning(paste0("Birth_Year column not found in merged data. ",
  #                  "Age variables will not be created."))
  #   return(merged_data)
  # } else {
  #   # ---- Compute Age Variables Using Year Only ----
  #   date_cols <- grep("^(Pre|Post)_Date_", names(merged_data), value = TRUE)
  #
  #   # for (col in date_cols) {
  #   #   age_col <- paste0("Age_At_", col)
  #   #   year_vals <- as.numeric(format(as.Date(merged_data[[col]]), "%Y"))
  #   #   merged_data[[age_col]] <- year_vals - merged_data$Birth_Year
  #   # }
  #   #
  #   for (col in date_cols) {
  #     age_col <- paste0("Age_At_", col)
  #     merged_data[, (age_col) :=
  #                   suppressWarnings(as.numeric(format(as.Date(get(col)), "%Y")) - Birth_Year)
  #     ]
  #   }
  #
  #
  #   ## GET AGE DIFF PER SERVICE FOR EACH PARTICIPANT
  #   services <- c("CPSO", "CSS", "FL", "ILOM", "ISA", "JOBEX", "JS", "QWEX",
  #                 "WBLE", "WSS")
  #
  #   for (service in services) {
  #     pre_col <- paste0("Age_At_Pre_Date_", service)
  #     post_col <- paste0("Age_At_Post_Date_", service)
  #     diff_col <- paste0("Age_Diff_", service)
  #
  #     median_age_col <- paste0("Median_Age_", service)
  #
  #     merged_data[, (diff_col) := get(post_col) - get(pre_col)]
  #
  #     merged_data[, (median_age_col) :=
  #                   apply(.SD, 1, function(x) median(x, na.rm = TRUE)),
  #                 .SDcols = c(pre_col, post_col)]
  #   }
  #
  #   merged_data[, (diff_col) := fifelse(!is.na(get(post_col)) &
  #                                         !is.na(get(pre_col)),
  #                                       get(post_col) - get(pre_col),
  #                                       NA_real_)]
  #
  #   ## GET MEDIAN AGE DIFF (ACROSS ALL SERVICES) FOR EACH PARTICIPANT
  #   diff_cols <- paste0("Age_Diff_", c("CPSO", "CSS", "FL", "ILOM", "ISA",
  #                                      "JOBEX", "JS", "QWEX", "WBLE", "WSS"))
  #
  #   # Calculate per-row median of the Age_Diff_* columns, ignoring NA
  #   merged_data[, Median_Age_Diff_TRT := apply(
  #     .SD, 1, function(x) {
  #       median(x, na.rm = TRUE)
  #     }
  #   ), .SDcols = diff_cols]
  #
  #
  #   ## GET MIN AND MAX AGE FOR EACH PARTICIPANT
  #   # Define pre and post column sets
  #   pre_cols <- paste0("Age_At_Pre_Date_",
  #                      c("CPSO", "CSS", "FL", "ILOM", "ISA", "JOBEX", "JS",
  #                        "QWEX", "WBLE", "WSS"))
  #   post_cols <- paste0("Age_At_Post_Date_",
  #                       c("CPSO", "CSS", "FL", "ILOM", "ISA", "JOBEX", "JS",
  #                         "QWEX", "WBLE", "WSS"))
  #
  #   # Calculate per-row min and max (ignoring NAs)
  #   merged_data[, Min_Pre_Age := do.call(pmin, c(.SD, na.rm = TRUE)),
  #               .SDcols = pre_cols]
  #   merged_data[, Max_Post_Age := do.call(pmax, c(.SD, na.rm = TRUE)),
  #               .SDcols = post_cols]
  #
  #   ## GET THE OVERALL TOTAL TIME BETWEEN EARLIEST PRE AND LATEST POST
  #   merged_data[, Total_Age_Diff_TRT := Max_Post_Age - Min_Pre_Age]
  #
  #
  #   ## GET OVERALL MEDIAN AGE ACROSS ALL SERVICES AND BOTH PRE & POST
  #   all_age_cols <- unlist(lapply(services, function(service) {
  #     c(paste0("Age_At_Pre_Date_", service), paste0("Age_At_Post_Date_",
  #                                                   service))
  #   }))
  #
  #   merged_data[, Median_Age := apply(
  #     .SD, 1, function(x) {
  #       median(x, na.rm = TRUE)
  #     }
  #   ),
  #   .SDcols = all_age_cols]
  #
  #   age_cols <- names(merged_data)[grepl("Age", names(merged_data)) &
  #                                    names(merged_data) != "E74_SWD_Age_911" &
  #                                    names(merged_data) != "Age_Group" &
  #                                    names(merged_data) != "E4_Agency_Code_911" &
  #                                    names(merged_data) != "E394_App_SSI_Aged_Amt" &
  #                                    names(merged_data) != "E396_Exit_SSI_Aged_Amt"]
  #
  #   merged_data[, (age_cols) := lapply(.SD, round), .SDcols = age_cols]
  #
  #   return(merged_data)
  # }

  return(merged_data)

}
