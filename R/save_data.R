

#' Save Data To Various Connectors
#'
#' @param ... arguments to pass to connector (see details)
#' @param verbose a logical (default = FALSE) to indicate if additional traces should be sent to the console
#'
#' @details
#' The save_data function is a wrapper around connector functions.
#' It offers a single point of access to the package data persistence features.
#'
#' Arguments passed to ... depend on the desired connector:
#' - file: \link[iker]{write_data}
#'
#' @returns the output of the connector function
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
    rc <- write_data(..., verbose = verbose)

  # -- return
  rc

}
