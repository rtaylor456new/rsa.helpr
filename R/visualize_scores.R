#' Visualize TRT Scores Data
#'
#' This function produces tailored visualizations for a TRT scores dataset in
#'   one step.
#'
#' @param data A cleaned TRT scores dataset. Apply clean_scores function first.
#' @param option The selected visual analysis option. The options are
#'   "overview":
#'   "across_service":
#'   "across_provider":
#' @param one_window Whether or not the user wants the visuals to be displayed
#'   in one plotting window, or spread across multiple plot windows. Defaults to
#'   FALSE.
#'
#' @returns The appropriate plots for the chosen visual analysis.
#'
#' @export
#' @import graphics
#'
visualize_scores <- function(data, option = c("overview", "across_service",
                                              "across_provider"),
                             one_window = FALSE) {

  option <- match.arg(option) # this produces an automatic check

  if (option == "overview") {

    # Check if the required columns are found

    # Find the column for Median_Difference_Score
    median_diff_col <- grep("(?i)(med|median).*?(diff|difference)",
                            names(data), value = TRUE, perl = TRUE)

    # Find the column for Median_Time_Passed_Days
    median_time_col <- grep("(?i)(med|median).*?(time|days)", names(data),
                            value = TRUE, perl = TRUE)

    # Find the column for Differences_Available
    diff_available_col <- grep("(?i)(diff|difference).*?(available)",
                               names(data), value = TRUE, perl = TRUE)

    # Check if the required columns are found
    if (length(median_diff_col) == 0 || length(median_time_col) == 0 ||
        length(diff_available_col) == 0) {
      stop("Missing required columns for 'overview' visualization.")
    }

    # Proceed with visualization using the identified columns
    message("Columns identified: ", paste(median_diff_col, collapse = ", "),
            "; ",
            paste(median_time_col, collapse = ", "), "; ",
            paste(diff_available_col, collapse = ", "))

    # Set adjusted plotting window if necessary
    if (one_window == TRUE) {
      par(mfrow = c(3, 1))
    }

    ## PLOT 1
    hist(data[[median_diff_col]],
         col = "lightsteelblue",
         main = "Distribution of Median Difference Scores",
         xlab = "Median Difference Scores")

    ## PLOT 2
    hist(data[[median_time_col]],
         col = "lightsteelblue",
         main = "Distribution of Time in Programs",
         xlab = "Median Time in Program (per individual)")

    ## PLOT 3
    hist(data[[diff_available_col]],
         col = "lightsteelblue",
         main = "Distribution of Counts of Difference Scores",
         xlab = "Number of Program Difference Scores per Individual")

    # Reset plotting window
    if (one_window == TRUE) {
      par(mfrow = c(1, 1))
    }
  } else if (option == "across_service") {

    # Check if the required columns are found

    ## DIFFERENCE SCORES
    difference_cols <- grep("(?i)difference", names(data),
                            value = TRUE, perl = TRUE)
    # Exclude columns that contain "med" or "avail"
    exclude_patterns <- "(?i)med|avail"
    filtered_columns <- difference_cols[!grepl(exclude_patterns,
                                               difference_cols,
                                               perl = TRUE)]

    ## PRE SCORES
    pre_cols <- grep("(?i)pre", names(data),
                     value = TRUE, perl = TRUE)
    # Exclude columns with "median" (can't get it to work in one step)
    pre_cols <- pre_cols[!grepl("(?i)median", pre_cols)]

    ## POST SCORES
    post_cols <- grep("(?i)post", names(data),
                      value = TRUE, perl = TRUE)
    # Exclude columns with "median" (can't get it to work in one step)
    post_cols <- post_cols[!grepl("(?i)median", post_cols)]


    if (length(filtered_columns) == 0 || length(pre_cols) == 0 ||
        length(post_cols) == 0) {
      stop("Missing required columns for 'across_services' visualization.")
    }

    # Proceed with visualization using the identified columns
    message("Columns identified: ", paste(filtered_columns, collapse = ", "),
            "; ",
            paste(pre_cols, collapse = ", "), "; ",
            paste(post_cols, collapse = ", "))


    # Set adjusted plotting window if necessary
    if (one_window == TRUE) {
      par(mfrow = c(3, 1))
    }

    ## PLOT 1

    # Find the overall median for all median difference scores -- to plot as
    #   horizontal reference line
    differences <- data[, .SD, .SDcols = filtered_columns]
    differences_scores_vector <- as.vector(unlist(differences))
    differences_median <- median(differences_scores_vector, na.rm = TRUE)

    par(mar = c(3, 4, 4, 2) + 0.1)  # Increase bottom margin for labels
    boxplot(differences,
            names = NA,  # Suppress default labels
            main = "Distributions of Difference Scores Across Services",
            ylab = "Difference Scores",
            ylim = range(differences_scores_vector, na.rm = TRUE) * c(0.95,
                                                                      1.05),
            col = "lightsteelblue",
            xaxt = "n")

    # Get shortened labels
    short_labels <- abbreviate(sub(".*Difference_", "",
                                   names(differences)), minlength = 6)

    # Add rotated labels manually
    axis(1,
         at = seq_along(short_labels),
         labels = FALSE, # Prevent overlapping
         tck = 0)
    text(x = seq_along(short_labels),
         y = par("usr")[3] - 10,  # Adjust y position
         labels = short_labels,
         srt = 45,  # Rotate labels
         adj = 1,
         xpd = TRUE)  # Allow text outside plot bounds

    abline(h = differences_median, lty = 1, lwd = 2, col = "steelblue",
           xpd = FALSE)


    ## PLOT 2
    # Find the overall median for all median pre scores -- to plot as
    #   horizontal reference line
    pre_scores <- data[, .SD, .SDcols = pre_cols]
    pre_scores_vector <- as.vector(unlist(pre_scores))
    pre_scores_median <- median(pre_scores_vector, na.rm = TRUE)

    par(mar = c(3, 4, 4, 2) + 0.1)  # Increase bottom margin for labels
    boxplot(pre_scores,
            names = NA,  # Suppress default labels
            main = "Distributions of Pre Scores Across Services",
            ylab = "Pre Scores",
            col = "lightsteelblue",
            xaxt = "n")  # Suppress x-axis labels

    # Get shortened labels
    short_labels <- sub("Pre_Score_", "", pre_cols)

    # Add rotated labels manually
    axis(1,
         at = seq_along(short_labels),
         labels = FALSE, # Prevent overlapping
         tck = 0)
    text(x = seq_along(short_labels),
         y = par("usr")[3] - 10,  # Adjust y position
         labels = short_labels,
         srt = 45,  # Rotate labels
         adj = 1,
         xpd = TRUE)  # Allow text outside plot bounds

    abline(h = pre_scores_median, lty = 1, lwd = 2, col = "steelblue",
           xpd = FALSE)


    ## PLOT 3
    # Find the overall median for all median post scores -- to plot as
    #   horizontal reference line
    post_scores <- data[, .SD, .SDcols = post_cols]
    post_scores_vector <- as.vector(unlist(post_scores))
    post_scores_median <- median(post_scores_vector, na.rm = TRUE)

    par(mar = c(3, 4, 4, 2) + 0.1)  # Increase bottom margin for labels
    boxplot(post_scores,
            names = NA,  # Suppress default labels
            main = "Distributions of Post Scores Across Services",
            ylab = "Post Scores",
            col = "lightsteelblue",
            xaxt = "n")  # Suppress x-axis labels

    # Get shortened labels
    short_labels <- sub("Post_Score_", "", post_cols)

    # Add rotated labels manually
    axis(1,
         at = seq_along(short_labels),
         labels = FALSE, # Prevent overlapping
         tck = 0)
    text(x = seq_along(short_labels),
         y = par("usr")[3] - 10,  # Adjust y position
         labels = short_labels,
         srt = 45,  # Rotate labels
         adj = 1,
         xpd = TRUE)  # Allow text outside plot bounds
    abline(h = post_scores_median, lty = 1, lwd = 2, col = "steelblue",
           xpd = FALSE)


    # Return plotting window to normal
    if (one_window == TRUE) {
      par(mfrow = c(1, 1))
    }
  } else if (option == "across_provider") {

    # Check if the required columns are found

    # Median_Difference_Score column
    median_diff_col <- grep("(?i)(med|median).*?(diff|difference)",
                            names(data), value = TRUE, perl = TRUE)

    # Provider column
    provider_col <- grep("(?i)provider", names(data),
                         value = TRUE, perl = TRUE)


    if (length(median_diff_col) == 0 || length(provider_col) == 0) {
      stop("Missing required columns for 'across_services' visualization.")
    }

    # Proceed with visualization using the identified columns
    message("Columns identified: ", paste(median_diff_col, collapse = ", "),
            "; ",
            paste(provider_col, collapse = ", "))

    # Alter this if I add in more plots
    if (one_window == TRUE) {
      par(mfrow = c(1, 1))
    }

    ## PLOT

    # Find the overall median for all median differences scores -- to plot as
    #   horizontal reference line
    median_diff_scores <- data[, .SD, .SDcols = median_diff_col]
    median_diff_scores_vector <- as.vector(unlist(median_diff_scores))
    overall_median <- median(median_diff_scores_vector, na.rm = TRUE)

    # Apply cleaning function to get abbreviated Provider names (will work even
    #   if the values are already abbreviated)
    provider_col_clean <- factor(handle_abbrev(data[[provider_col]]))

    par(las = 2)
    boxplot(data[[median_diff_col]] ~ provider_col_clean,
            main = "Median Difference Scores Across Providers",
            ylab = "Median Difference Score",
            cex.axis = 0.7,
            col = "lightsteelblue")
    abline(h = overall_median, lty = 1, lwd = 2, col = "steelblue",
           xpd = FALSE)
    par(las = 1)

    # Return plotting window to normal -- not necessary for this option yet
    if (one_window == TRUE) {
      par(mfrow = c(1, 1))
    }

  }

}
