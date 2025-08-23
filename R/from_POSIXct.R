

#' Convert POSIXct Columns to ISO-8601 Character
#'
#' @param data a data.frame (expected to contain POSIXct columns)
#'
#' @returns a data.frame, whose POSIXct columns have been converted to ISO-8601 character columns
#' @export
#'
#' @examples
#' from_POSIXct(data.frame(a = Sys.time()))

from_POSIXct <- function(data){

  # -- get POSIXct index
  classes <- lapply(data, function(x) class(x)[1])
  idx <- which(classes == "POSIXct")

  # -- convert to ISO 8601 character format
  if(length(idx) > 0){
    ktools::catl("[Iker] Convert datetime attribute(s) to ISO-8601 =", names(data[idx]), "\n")
    data[idx] <- format(data[idx], "%FT%H:%M:%S%z")}

  # -- return
  data

}
