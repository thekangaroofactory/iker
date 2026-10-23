
#' Call API
#'
#' @description
#' High level implementation of 'curl' fetch to memory.
#'
#' @param url character, the API url.
#' @param header a list of character headers.
#' @param options optional list (see details).
#' @param raw a logical (default FALSE) if the raw content should be returned.
#' @param verbose a logical (default FALSE) if logs should be printed to the console.
#'
#' @details
#' `url` must start with "https://".
#'
#' The values in `header` must be character strings (see examples).
#'
#' The list of options can be accessed with `curl::curl_options()`.
#'
#' When `raw` is FALSE, the content (& headers) of the response will be converted
#' into character strings.
#'
#' @returns a list (the full response).
#' @export
#'
#' @examples
#' \dontrun{
#' call_api(url = "https://some/url", header = list(Accept = "application/json"))
#' }

call_api <- function(url, header, options = NULL, raw = FALSE, verbose = FALSE){

  # -- check params
  stopifnot("url must be a character string" = is.character(url))
  stopifnot("url must start with https://" = startsWith(url, "https://"))
  stopifnot("header must be a list" = is.list(header))
  if(!is.null(options))
    stopifnot("options must be a list" = is.list(options))

  # -- build handle
  h <- curl::new_handle() |>
    curl::handle_setheaders(.list = header)

  if(!is.null(options))
    h <- h |> curl::handle_setopt(.list = options)

  # -- do API call
  if(verbose)
    message("Calling url: ", url)
  res <- curl::curl_fetch_memory(url = url, handle = h)

  # -- check response
  # Informational responses (100 – 199)
  # Successful responses (200 – 299)
  # Redirection messages (300 – 399)
  # Client error responses (400 – 499)
  # Server error responses (500 – 599)
  if(res$status_code >= 400)
    warning("API response contains an error code: ", res$status_code, call. = FALSE)
  else if(verbose)
    message("API response status code: ", res$status_code)

  # -- convert raw
  if(!raw){
    res$headers <- rawToChar(res$headers)
    res$content <- rawToChar(res$content)}

  # -- return
  # full response, to be handled within specific connector functions
  res

}
