
#' Reset Cache
#'
#' @description
#' Clear all files from cache folder.
#'
#' @param path the path to the cache folder.
#' @param force a logical, if confirmation should be skipped.
#'
#' @details
#' When a single location is used to store the cache content, the "R_IKER_CACHE_PATH"
#' is recommended to define `path`.
#'
#' @returns NULL or a character string when folder does not exist.
#' @export
#'
#' @examples
#' \dontrun{
#' reset_cache(path = "./cache")
#' }

reset_cache <- function(path = Sys.getenv("R_IKER_CACHE_PATH"), force = FALSE){

  stopifnot("empty path is not allowed" = nchar(path) > 0)

  if(dir.exists(path)){

    content <- list.files(path)

    if(!force){
      cat("Cache contains", length(content), "file(s).")
      confirm <- readline("Do you confirm deleting all files? (Y or anything)")
      confirm <- ifelse(confirm == "Y", TRUE, FALSE)
    } else confirm <- TRUE

    if(confirm){
      unlink(paste0(path, "/*"))
      message("Reset cache done.")}

  } else warning("Path does not exist.", call. = F)

}
