

#' Convert ISO-8601 Character Columns to POSIXct
#'
#' @param data a data.frame
#' @param names_ct the names of the columns to convert
#'
#' @returns a data.frame with converted columns
#' @export
#'
#' @examples
#' to_POSIXct(data.frame(a = "2025-05-13T12:15:30+0200"), "a")


to_POSIXct <- function(data, names_ct){

  # -- check if data has any of the expected attributes
  if(any(names_ct %in% names(data))){

    # -- because any is used, clean potential missing attribute(s)
    names_ct <- names_ct[names_ct %in% names(data)]

    ktools::catl("[Iker] Converting attribute(s) to POSIXct =", names_ct, "\n", level = 1)
    data[names_ct] <- lapply(data[names_ct], function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S%z", tz = Sys.timezone()))}

  # -- return
  data

}
