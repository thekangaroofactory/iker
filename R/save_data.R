

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
  connectors <- c("file", "api")

  # -- check connector
  if(!any(connectors %in% names(args))){
    warning("[iker] one of the connector ", paste0("[", connectors, "]"), " argument must be set")
    return(FALSE)}

  # -- file
  # only .csv is supported
  if("file" %in% names(args))
    f <- "write_data"

  # -- API
  else if("api" %in% names(args))

    # -- Supabase
    if(args$api == "supabase"){

      # -- upsert
      if("data" %in% names(args))
        f <- "supabase_upsert"

      # -- delete
      else if("values" %in% names(args))
        f <- "supabase_delete"}

  # -- call connector function & return
  do.call(f, c(args, list(verbose = verbose)))

}
