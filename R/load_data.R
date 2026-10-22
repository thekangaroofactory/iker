

#' Load Data From Connector
#'
#' @param ... arguments to pass to the connector function (see details).
#' @param verbose a logical (default = FALSE) to indicate if additional traces should be sent to the console.
#' @param data.frame a logical (default = FALSE) if a data.frame should be returned (instead of a tibble).
#' @param force_empty a logical (default = FALSE) if NULL should be returned when data has no row.
#'
#' @details
#' The function is a wrapper around connector functions.
#' It offers a single point of access to the package data persistence features.
#'
#' Arguments passed to `...` depend on the desired connector.
#' Vignettes are available to describe the connectors.
#'
#' @seealso [read_data()][supabase_select()]
#'
#' @returns the output of the connector function.
#' @export
#'
#' @examples
#' \dontrun{
#' # -- file connector
#' load_data(path = getwd(), file = "my_data.csv")
#' }


load_data <- function(..., verbose = FALSE, data.frame = FALSE, force_empty = FALSE) {

  # -- get ellipsis
  args <- list(...)

  # -- unwrap connector
  if("connector" %in% names(args))
    args <- append(args[-which(names(args) == "connector")], args$connector)

  # -- check allowed connectors
  connectors <- c("file")
  if(!any(connectors %in% names(args))){
    warning("[iker] one of the connector ", paste0("[", connectors, "]"), " argument must be set")
    return(NULL)}

  # -- file connector
  if("file" %in% names(args))
    x <- do.call(read_data, c(args, list(verbose = verbose)))

  # -- force empty
  # return NULL if x has no rows/observations
  if(force_empty)
    if(nrow(x) == 0)
      return(NULL)

  # -- return
  if(data.frame)
    as.data.frame(x)
  else
    x

}
