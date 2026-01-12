#' Clean date variable.
#'
#' This function converts a RSA-911 date variable written in order YYYYMMDD as
#'    one number to the appropriate date.
#'
#' @param x A date variable, written as a numeric YYYYMMDD.
#' @returns The converted date variable.
#' @export
handle_date <- function(x) {
  date <- suppressWarnings(as.Date(as.character(x), format = "%Y%m%d"))
  date
}


#' Clean Excel-based date variable.
#'
#' This function converts a RSA-911 Excel-based date variable written as one
#'    number with origin date 1899-12-30 to the appropriate date.
#'
#' @param x An Excel date variable, origin 1899-12-30.
#' @returns The converted date variable.
#' @export
handle_excel_date <- function(x) {
  date <- suppressWarnings(as.Date(as.numeric(x), origin = "1899-12-30"))
  date
}

#' Clean mixed-format date variable.
#'
#' This function converts a RSA-911 date variable with different entered
#'   formats.
#'
#' @param x An date variable (if Excel date, origin 1899-12-30).
#' @returns The converted date variable.
#' @export

handle_mixed_date <- function(x) {
  x <- as.character(x)
  cleaned <- rep(as.Date(NA), length(x))

  # Identify types of entries
  is_slash <- grepl("/", x)
  is_dash <- grepl("-", x)
  is_numeric <- grepl("^\\d+$", x)

  # Apply parsing
  cleaned[is_slash] <- suppressWarnings(as.IDate(x[is_slash],
                                                 format = "%m/%d/%Y"))
  cleaned[is_dash]  <- suppressWarnings(as.IDate(x[is_dash],
                                                 format = "%Y-%m-%d"))
  cleaned[is_numeric] <- suppressWarnings(as.IDate(as.numeric(x[is_numeric]),
                                                   origin = "1899-12-30"))

  cleaned
}


#' Clean year variable.
#'
#' This function cleans a RSA-911 year variable containing a 4-digit year to
#'   the extracted 4-digit year.
#'
#' @param x A year variable, containing a 4-digit year somewhere in string.
#' @returns The converted year variable, just the digits.
#' @export
handle_year <- function(x) {
  year <- suppressWarnings(as.numeric(gsub("\\D*(\\d{4})\\D*", "\\1", x)))
  year
}


#' Convert missing values to 9s or 0s
#'
#' This function cleans a RSA-911 date variable containing missing values,
#'   converting missings to 9s. If specified by the user, it additionally
#'   converts 9s to 0s.
#'
#' @param x An RSA-911 variable to be cleaned, containing missings and/or 9s.
#' @param unidentified_to_0 TRUE or FALSE. Defaults to TRUE, when TRUE,
#'   variables where unidentified is represented by 9,
#'   values are converted to 0.
#' @returns The converted date variable.
#' @export
handle_nines <- function(x, unidentified_to_0 = TRUE) {
  x <- suppressWarnings(as.numeric(x))
  # Replace NA values with 9
  x[is.na(x)] <- 9

  # If convert_9s is TRUE, convert 9s to 0s
  if (unidentified_to_0) {
    x[x == 9] <- 0
  }

  return(x)
}


#' Clean a variable with blank values.
#'
#' This function cleans a RSA-911 variable with blanks and NULL values.
#'
#' @param x An RSA-911 variable.
#' @returns The cleaned variable, where blanks and NULLs are replaced with 0s.
#' @export
handle_blanks <- function(x) {
  # Identify rows with values equal to " " or "NULL" in the specified column
  x[x %in% c(" ", "NULL", NA, "NA")] <- 0
  x
}


#' Convert character coded variables.
#'
#' This function converts a RSA-911 date variable written in order YYYYMMDD as
#'    one number to the appropriate date.
#'
#' @param x A variable containing code values, so values that have little
#'   meaning in numeric or factor form. Simply represent recorded ids.
#' @returns The cleaned code variable.
#' @export
handle_code <- function(x) {
  x[x %in% c(" ", "NULL", NA, "NA", "")] <- NA
  x
}


#' Clean incorrect values.
#'
#' This function cleans incorrect values for a RSA-911 variable.
#'
#' @param x An RSA-911 variable with specific allowed values.
#' @param values A vector of permitted values for the variable.
#' @param blank_value The preferred replacement value for incorrect values.
#'   Default is to replace incorrect values to NA.
#' @returns The converted date variable.
#' @export
handle_values <- function(x, values, blank_value = NA) {
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x) | !(x %in% values)] <- blank_value
  return(x)
}


#' CLean/abbreviate name variables.
#'
#' This function cleans and shortens name variables, for example, to applied on
#'   a variable such as Provider.
#'
#' @param x The name variable.
#' @returns The cleaned name variable, with abbreviated values.
#' @export
handle_abbrev <- function(x) {

  filler_words <- c("to", "of", "for", "and", "the", "in", "on", "at", "with",
                    "by", "from")

  sapply(x, function(name) {
    # Step 1: Remove special characters (e.g., (30), punctuation)
    name <- gsub("\\(.*?\\)|[^a-zA-Z\\s]", " ", name)

    # Step 2: Split name into words
    words <- unlist(strsplit(name, " "))

    # Step 3: Filter out filler words and keep only capitalized words
    # Remove filler words
    words <- words[!(tolower(words) %in% filler_words)]
    # Keep only capitalized words
    capitalized_words <- words[grepl("^[A-Z]", words)]

    # Step 4: Generate the acronym
    if (length(capitalized_words) > 1) {
      # Acronym from initials
      paste0(substr(capitalized_words, 1, 1), collapse = "")
    } else {
      # Return full name if there's only one capitalized word
      trimws(name)
    }
  })
}


#' Clean variables in RSA-911 dataset with special characters.
#'
#' This function cleans variables with special characters by splitting values
#'   into separate variables that are separated by special characters.
#'
#' @param var An RSA-911 special character variable (vector) to be cleaned.
#' @param var_name A character vector of the name of the variable to be cleaned.
#' @param sep The special character that is separating values within the
#'   variable.
#' @returns The new variables derived from the inputted variable. They contain
#'   the cleaned, separated values without special characters.
#' @export
handle_splits <- function(var, var_name, sep = ";") {
  # Split the vector into a list
  split_values <- strsplit(as.character(var), split = sep, perl = TRUE)

  # Replace "NULL" strings and blanks with NA
  split_values <- lapply(split_values,
                         function(x) ifelse(x %in% c("NULL", ""), NA, x))

  # Determine the max number of splits
  max_splits <- max(lengths(split_values))

  # Generate new vector names
  new_var_names <- paste0(var_name, "_Place", seq_len(max_splits))

  # Create a matrix with values or NAs
  result_matrix <- t(sapply(split_values, function(x) {
    # Ensure uniform length with NA padding
    length(x) <- max_splits
    x
  }))

  # Convert matrix to named list of vectors
  result_list <- setNames(as.list(as.data.frame(result_matrix,
                                                stringsAsFactors = FALSE)),
                          new_var_names)

  return(result_list)
}



#' Apply handle_splits() to RSA-911 dataset.
#'
#' This function cleans variables with special characters by splitting values
#'   into separate variables that are separated by special characters by
#'   applying the function within a dataset.
#'
#' @param data An RSA-911 dataset.
#' @param special_cols A character vector of the names of variables with special
#'   character values in the dataset.
#' @param sep The special character that is separating values within the
#'   variable.
#' @returns The cleaned RSA-911 dataset, with new variables that contain the
#'   cleaned, separated values without special characters.
#' @export
apply_handle_splits <- function(data, special_cols, sep = ";") {
  for (col in special_cols) {
    if (col %in% names(data)) {
      split_results <- handle_splits(data[[col]], col, sep)
      data <- cbind(data, as.data.frame(split_results,
                                        stringsAsFactors = FALSE))
      # Remove the original column
      data[[col]] <- NULL
    }
  }
  return(data)
}


#' Clean primary and secondary disability variables in RSA-911 dataset.
#'
#' This function cleans primary and secondary disability variables with special
#'   characters by splitting impairment and cause values into separate variables
#'   that are separated by special characters.
#'
#' @param df An RSA-911 dataset to be cleaned.
#' @returns The cleaned RSA-911 dataset, with new, separate variables for
#'   primary and secondary cause and impairment, without special characters.
#' @export

separate_disability <- function(df) {
  # Convert to data.table if not already
  setDT(df)

  prim_disability <- grep("(?i)^(?=.*prim)(?=.*disab)(?!.*(desc))",
                          names(df), value = TRUE, perl = TRUE)

  second_disability <- grep("(?i)^(?=.*second)(?=.*disab)(?!.*(desc))",
                            names(df), value = TRUE, perl = TRUE)

  # Separate E43_Primary_Disability_911 into E43_Primary_Impairment_911 and
  #   E43_Primary_Cause_911
  df[, c("Primary_Impairment",
         "Primary_Cause") := tstrsplit(df[[prim_disability]],
                                       ";", fixed = TRUE)]

  # Separate E44_Secondary_Disability_911 into E44_Secondary_Impairment_911 and
  #   E44_Secondary_Cause_911
  df[, c("Secondary_Impairment",
         "Secondary_Cause") := tstrsplit(df[[second_disability]],
                                         ";", fixed = TRUE)]
  # remove original columns
  df[, (prim_disability) := NULL]
  df[, (second_disability) := NULL]

  df
}


#' Create Primary Impairment and Secondary Impairment grouping columns in
#'  RSA-911 dataset.
#'
#' This function groups impairments (values 0-19) by the codebook definitions
#'   (visual, auditory/communicative, physical, intellectual/learning,
#'   psychological). This process
#'
#' @param x An RSA-911 impairment (or cause) variable to be grouped.
#' @returns The new variable with grouped character values Visual, Aud_Comm,
#'   Physical, Intell_Learn, and Psych, rather than values 0-19.
#' @export
handle_impairment_group <- function(x) {
  factor(
    fifelse(x == 0, "None",
            fifelse(x %in% c(1, 2, 8), "Visual",
                    fifelse(x %in% c(3, 4, 5, 6, 7, 9), "Aud_Comm",
                            fifelse(x %in% c(10, 11, 12, 13, 14, 15, 16), "Physical",
                                    fifelse(x == 17, "Intell_Learn",
                                            fifelse(x %in% c(18, 19), "Psych",
                                                    NA_character_)
                                    )
                            )
                    )
            )
    ),
    levels = c("None", "Visual", "Aud_Comm", "Physical", "Intell_Learn",
               "Psych")
  )
}


#' Create age group column in RSA-911 dataset.
#'
#' This function groups variable Age at Application into ordered levels:
#'   <5
#'   5-7
#'   8-10
#'   11-13
#'   14-16
#'   17-19
#'   20-22
#'   23-25
#'   26-30
#'   31-40
#'   41+
#'
#' @param x An RSA-911 age at application variable to be grouped.
#' @returns The new age group values, see above list.
#' @export
handle_age_group <- function(x) {
  factor(
    fifelse(x < 5, "<5",
            fifelse(x >= 5 & x < 8, "5-7",
                    fifelse(x >= 8 & x < 11, "8-10",
                            fifelse(x >= 11 & x < 14, "11-13",
                                    fifelse(x >= 14 & x < 17, "14-16",
                                            fifelse(x >= 17 & x < 20, "17-19",
                                                    fifelse(x >= 20 & x < 23, "20-22",
                                                            fifelse(x >= 23 & x < 26, "23-25",
                                                                    fifelse(x >= 26 & x < 31, "26-30",
                                                                            fifelse(x >= 31 & x <= 40, "31-40",
                                                                                    fifelse(x > 40, "41+", NA_character_)
                                                                            )
                                                                    )
                                                            )
                                                    )
                                            )
                                    )
                            )
                    )
            )
    ),
    levels = c("<5", "5-7", "8-10", "11-13", "14-16", "17-19", "20-22", "23-25",
               "26-30", "31-40", "41+")
  )
}
