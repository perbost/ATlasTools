#' @title Function to wrap any text at the nth character.
#' @name WrapStrings
#' @description We define a maximum number of character (n) and we transform
#' the text with a line crossing every n character.
#' @param strings (str) text to transform.
#' @param n (int) number of character before the line crossing (default=10).
#' @export


WrapStrings <- function(strings, n=10) {
  # function to wrap string label in multiple lines with maximum n characters
  # per line
  .WrapSingleString <- function(str) {
    chunks <- ceiling(nchar(str) / n)
    wrapped_string <- sapply(1:chunks, function(i) substr(str, (i-1)*n + 1,
                                                          min(i*n, nchar(str))))
    paste(wrapped_string, collapse = "\n")
  }

  lapply(strings, .WrapSingleString)
}
