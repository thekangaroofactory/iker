
#' Supabase API
#'
#' @description
#' Helpers to communicate with the Supabase API.
#'
#' @param table the name of the database table.
#' @param query an optional character string (for select).
#' @param data a data.frame (for upsert).
#' @param key the label of the key (for delete).
#' @param values the key value(s) to delete.
#' @param url the Supabase API url.
#' @param api_key the Supabase API key.
#' @param verbose if logs should be printed.
#'
#' @returns a data.frame or NULL (if call fails).
#' @export
#'
#' @examples
#' \dontrun{
#' # select all rows
#' supabase_select(table = "foo")
#'
#' # upsert (assuming foo table has attributes id, value, comment)
#' supabase_upsert(table = "foo", data = data.frame(id = 1, value = 12, comment = "update"))
#'
#' # delete row with id=1
#' supabase_delete(table = "foo", key = "id", values = 1)
#' }

# //////////////////////////////////////////////////////////////////////////////
# Get the content of the table
# query <- "select=*"

supabase_select <- function(table = Sys.getenv("SUPABASE_TABLE"), query = NULL,
                            url = Sys.getenv("SUPABASE_API_URL"), api_key = Sys.getenv("SUPABASE_API_KEY"),
                            verbose = FALSE){

  # compute url
  url <- paste0(url, table)
  if(!is.null(query))
    url <- paste(url, query, sep = "?")

  # prepare
  headers <- list(apikey = api_key,
                  verify_jwt = "FALSE",
                  Accept = "application/json")


  # call API
  res <- call_api(url, headers, verbose = verbose)

  # check & return
  if(res$status_code == 200){
    x <- jsonlite::fromJSON(res$content)
    if(verbose)
      cat("Output dim =", dim(x), "\n")
    x
  } else if(res$status_code >= 400) {
    warning("Supabase select failed, returning NULL!", call. = FALSE)
    NULL
  }

}


# //////////////////////////////////////////////////////////////////////////////
# Update
# We send the full data.frame or just those to update

#' @export
#' @rdname supabase_select

supabase_upsert <- function(table = Sys.getenv("SUPABASE_TABLE"), data,
                            url = Sys.getenv("SUPABASE_API_URL"), api_key = Sys.getenv("SUPABASE_API_KEY"),
                            verbose = FALSE){

  # check param
  stopifnot("data must be a data.frame" = is.data.frame(data))

  # compute url
  url <- paste0(url, table)

  # prepare payload
  payload <- jsonlite::toJSON(data)

  # headers
  headers <- list("apikey" = api_key,
                  "verify_jwt" = "FALSE",
                  "Content-Type" = "application/json",
                  "Prefer" = "resolution=merge-duplicates")

  # options (body)
  options <- list("copypostfields" = payload)

  # call API
  res <- call_api(url, headers, options, verbose = TRUE)

  # check & return
  # 201 for created
  if(res$status_code == 201){
    if(verbose)
      cat("Supabase upsert OK \n")
    TRUE
  } else {
    if(verbose){
      warning("Supabase upsert KO!", call. = FALSE)
      print(res$content)}
    FALSE
  }

}


# //////////////////////////////////////////////////////////////////////////////
# DELETE
# We just send the id to delete

#' @export
#' @rdname supabase_select

supabase_delete <- function(table = Sys.getenv("SUPABASE_TABLE"), key = "id", values,
                            url = Sys.getenv("SUPABASE_API_URL"), api_key = Sys.getenv("SUPABASE_API_KEY"),
                            verbose = FALSE){

  # check param
  stopifnot("key must be a character string" = is.character(key))
  stopifnot("values must have length > 0" = length(values) != 0)

  # prepare query
  expr <- if(length(values) > 1)
    paste0("in.", paste0("(", paste(values, collapse = ","), ")"))
  else
    paste0("eq.", values)
  query <- paste0("?", key, "=", expr)

  # compute url
  url <- paste0(url, table, query)

  # headers
  headers <- list("apikey" = api_key,
                  "verify_jwt" = "FALSE",
                  "Content-Type" = "application/json")

  # options
  options <- list(customrequest = "DELETE")

  # call API
  res <- call_api(url, headers, options, verbose = TRUE)

  # check
  # 204 for successful delete
  if(res$status_code == 204){
    if(verbose)
      cat("Supabase delete OK \n")
    TRUE
  } else {
    if(verbose){
      warning("Supabase delete KO!", call. = FALSE)
      print(res$content)}
    FALSE
  }

}
