#' Calculate the percent error between two values
#'
#' This function calculates the relative error between two values, where one value is deemed to be the original value, and the other is the recalculated/reproduced value. See definition on \href{https://en.wikipedia.org/wiki/Approximation_error#Formal_Definition}{Wikipedia} for reference.
#' The relative error is expressed as a percentage of the original value.
#' The function returns a percent error of NA if the original value is 0.
#' The function returns a percent error of NA if either the original value or recalculated value is \code{NA}, \code{NULL}, \code{NaN}, or non-numeric.
#'
#' @param origval The original numeric value to compare against
#' @param repval The recalculated or reproduced numeric value to be compared
#'
#' @return A list containing the percent error (\code{pcterror}) and an informative text description (\code{note})
#' @export
#'
#' @examples
#' pcterr(1.0, 1.0)
#' 
#' pcterr(1.0, 1.09)
#' 
#' pcterr(1.0, 1.10)
pcterr <- function(origval, repval) {
  
  # Check if either value is not numeric.
  if (!is.numeric(origval) || !is.numeric(repval)) {
    
    x <- NA_real_
    xnote <- "Cannot calculate result, invalid value(s)"
    
    # Check if either value is NA or NULL or NAN.
  } else if (is.na(origval) || is.na(repval)
      || is.null(origval) || is.null(repval)
      || is.nan(origval) || is.nan(repval)) {
    
    x <- NA_real_
    xnote <- "Cannot calculate result, invalid value(s)"
    
    # Check if the original value is 0.
  } else if (origval == 0) {
    
    x <- NA_real_
    xnote <- "Cannot calculate result, original value is zero"
    
    # In all other cases, the calculation can proceed as normal.
  } else {
    
    x <- 100*abs(repval - origval)/abs(origval)
    if (x == 0) {
      xnote <- "Original and reproduced values match exactly"
    } else if (x < 10) {
      xnote <- "Original and reproduced values differ by less than 10%"
    } else {
      xnote <- "Original and reproduced values differ by at least 10%"
    }
    
  }
  
  return(list(pcterror = x,
              note = xnote))
}