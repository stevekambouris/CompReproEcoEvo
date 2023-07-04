#' Compare a set of repro character values to original character values to check for matches
#'
#' This function takes two data frames, an original result and a reproduced result, and creates a data frame containing the results of string matches of a set of variables.
#' This function uses the base R function \code{identical} to compare character strings.
#'
#' @param df_orig The data frame containing the original character values
#' @param df_repro The data frame containing the reproduced character values
#' @param checkvars A character vector containing variable names to compare
#' @param name_orig A character string containing the column name for the original value. Default \code{"original"}.
#' @param name_repro A character string containing the column name for the reproduced value. Default \code{"repro"}.
#' @param name_diff A character string containing the column name for the string comparison. Default \code{"values_identical"}.
#'
#' @return A data frame containing five named variables: \code{compared}, the value of \code{name_orig}, the value of \code{name_repro}, the value of \code{name_diff}, and \code{note}. The values in the \code{compared} column will be the values of \code{checkvars}.
#' @export
#'
#' @examples
compare_char_vars <- function(df_orig,
                         df_repro,
                         checkvars,
                         name_orig = "original",
                         name_repro = "repro",
                         name_diff = "values_identical") {
  
  # Check that checkvars and the column name arguments are character vectors.
  stopifnot(is.character(checkvars),
            is.character(name_orig),
            is.character(name_repro),
            is.character(name_diff))
  
  # Check that the column name arguments only have length 1.
  stopifnot(length(name_orig) == 1,
            length(name_repro) == 1,
            length(name_diff) == 1)
  
  # Check that df_orig and df_repro are data frames.
  stopifnot(is.data.frame(df_orig),
            is.data.frame(df_repro))
  
  # Check that the variables specified in checkvars exist in both df_orig and
  # df_repro. Stop if not met.
  stopifnot(all(checkvars %in% names(df_orig)),
            all(checkvars %in% names(df_repro)))
  
  # Warn if df_orig and df_repro have more than one row - only data from the
  # first row will be used.
  if (nrow(df_orig) > 1) {
    warning("df_orig has multiple rows. Only data from the first row will be read.")
  }
  if (nrow(df_repro) > 1) {
    warning("df_repro has multiple rows. Only data from the first row will be read.")
  }
  
  # Specify the number of columns to have in total.
  n_cols <- 5
  
  # Determine the number of rows to have in total.
  n_vars <- length(checkvars)
  
  # Create an empty data frame with the number of required rows and columns.
  df_out <- as.data.frame(matrix(nrow = n_vars, ncol = n_cols))
  
  # Rename the variables in the data frame.
  names(df_out) <- c("compared",
                     name_orig,
                     name_repro,
                     name_diff,
                     "note")
  
  # Populate the "compared" variable - this is just the names of the variables
  # to be compared between the original and repro data frames.
  df_out$compared <- checkvars
  
  # Loop through the variables to be compared, and populate each row of the
  # output data frame.
  for (i in 1:n_vars) {
    # Get the original and repro values. Coerce to character.
    val_orig <- as.character(df_orig[1, checkvars[i]])
    val_repro <- as.character(df_repro[1, checkvars[i]])
    
    # Set the original and repro values.
    df_out[i, name_orig] <- val_orig
    df_out[i, name_repro] <- val_repro
    
    # Compare the two strings.
    if (identical(val_orig, val_repro) == TRUE) {
      val_match <- "Yes"
      val_match_note <- "-"
    } else {
      val_match <- "No"
      val_match_note <- "-"
    }
    
    # Set the string comparison results.
    df_out[i, name_diff] <- val_match
    df_out[i, "note"] <- val_match_note
  }
  
  # Return the data frame object.
  return(df_out)
}