

#' Save Data To Connector
#'
#' @param ... arguments to pass to the connector function (see details).
#' @param verbose a logical (default = FALSE) to indicate if additional traces
#' should be sent to the console.
#'
#' @details
#' The function is a wrapper around connector functions.
#' It offers a single point of access to the package data persistence features.
#'
#' Arguments passed to `...` depend on the desired connector:
#' Vignettes are available to describe the connectors.
#'
#' @seealso [write_data()][supabase_upsert()][supabase_delete()]
#'
#' @returns the output of the connector function.
#' @export
#'
#' @examples
#' \dontrun{
#' # -- file connector
#' save_data(file = "my_data.csv", path = getwd())
#' }


save_data <- function(..., verbose = FALSE) {

  # -- get ellipsis arguments
  args <- list(...)

  # -- unwrap connector
  if("connector" %in% names(args))
    args <- append(args[-which(names(args) == "connector")], args$connector)

  # -- allowed connectors
  connectors <- c("file")

  # -- check connector
  if(!any(connectors %in% names(args))){
    warning("[iker] one of the connector ", paste0("[", connectors, "]"), " argument must be set")
    return(FALSE)}

  # -- init
  rc <- FALSE

  # -- file connector
  if("file" %in% names(args))
    rc <- do.call(write_data, c(args, list(verbose = verbose)))

  # -- return
  rc

}
