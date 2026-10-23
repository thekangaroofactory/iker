
#' Random String
#'
#' @param n an integer, the length of the string.
#'
#' @details
#' The function produces character strings based on sampling
#' alphanumerical characters.
#'
#' @returns a character string.
#' @export
#'
#' @examples
#' rstring()
#' rstring(n = 10)

rstring <- function(n = 30){

  paste(replicate(n, sample(c(letters, LETTERS, 0:9), 1, replace = TRUE)), collapse = "")

}
