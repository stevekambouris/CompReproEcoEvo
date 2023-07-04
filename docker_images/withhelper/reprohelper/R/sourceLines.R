#' Run specific lines of a source file
#'
#' This is a variation of the solution found on \href{https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file}{Stack Overflow}.
#' The function will take a range of lines within a source file and execute only those lines, via the \code{source()} function.
#'
#' @param sourcefile The path and file name of the source file.
#' @param sline The line to start reading from the source file. Default is 1.
#' @param fline The final line to be read in from the source file. Default is 1.
#' @param secho logical; this is the argument used to pass to the \code{echo} argument of the \code{source} function. Default is \code{TRUE}.
#'
#' @return None
#' @export
#'
#' @examples
sourceLines <- function (sourcefile, sline = 1, fline = 1, secho = TRUE) {
  # Check that the secho argument is a logical.
  stopifnot(is.logical(secho))
  
  # Determine the value of skip argument for scan function.
  stopifnot(sline > 0)
  this_skip <- sline - 1

  # Determine the value of n for scan function.
  stopifnot(fline >= sline)
  this_n <- fline - sline + 1

  lines <- scan(sourcefile, what = character(), sep = "\n", skip = this_skip,
                n = this_n, quiet = TRUE, blank.lines.skip = FALSE)

  tc <- textConnection(lines)
  source(tc, echo = secho)
  close(tc)
}
