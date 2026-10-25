
#' With Cache
#'
#' @description
#' Runs an expression with cache.
#'
#' @param expr an expression to evaluate.
#' @param path the path to the cache folder.
#' @param timeout a numeric (in hours) to indicate when cache content is expired.
#' @param force a logical, if cache content should be updated.
#' @param limit a numeric, the cache size limit (in Mb).
#' @param cleanup a character string or a numeric value (see details).
#' @param encrypt a logical, if cache content should be encrypted.
#' @param key a character string, to be used as a secret.
#' @param verbose a logical, if logs should be printed.
#'
#' @details
#' Even though the function is designed to work with call_api(), it could in principle
#' work with any expression.
#'
#' When a single location is used to store the cache content, the "R_IKER_CACHE_PATH"
#' is recommended to define `path`.
#'
#' When `force = TRUE`, the expression will be run even if the cache content is
#' not expired.
#'
#' By default `cleanup` is set to "auto". The function will check whether the files
#' in cache are older than the timeout. Obsolete files will be deleted.
#' This behavior can be turned off with `cleanup = "off"` or tuned with a numeric
#' value `cleanup = 24` in hours.
#'
#' By default, `encrypt` is set to `TRUE` so that cache content is encrypted.
#' This is because many APIs require a key/token as an authentication method to
#' access their resources.
#'
#' Setting `encrypt = FALSE` exposes the content of the cache files. It should
#' only be used for APIs that do not require an authentication (an open source
#' geocoding service for example).
#'
#' When `encrypt = TRUE`, a `key` is mandatory to encode/decode the content of
#' the cache. The expected value is a character string that does NOT need to be
#' hashed. This value will be used to generate the actual encryption key.
#' That being said, this key should not be hard coded or an easy one to find.
#' The [rstring()] function can be used to generate one, but the generated value
#' needs to be saved and passed to the "R_IKER_CACHE_KEY" environment variable.
#' If the key is lost, the cache content is lost.
#'
#' @seealso [rstring()]
#'
#' @returns whatever the evaluation of expr produces.
#' @export
#'
#' @examples
#' \dontrun{
#' # set R_IKER_CACHE_PATH environment variable
#' # set R_IKER_CACHE_KEY environment variable
#'
#' # basic example:
#' # first call will evaluate & return the current time
#' with_cache(Sys.time(), verbose = T)
#' # next calls will return cache content (for 8 hours)
#' with_cache(Sys.time(), verbose = T)
#' }

with_cache <- function(expr, path = Sys.getenv("R_IKER_CACHE_PATH"),
                       timeout = 8, force = FALSE, limit = 1, cleanup = c("auto", "off"),
                       encrypt = TRUE, key = Sys.getenv("R_IKER_CACHE_KEY"),
                       verbose = FALSE){

  # ////////////////////////////////////////////////////////////////////////////
  # check params

  stopifnot("empty path is not allowed" = nchar(path) > 0)

  if(encrypt){
    stopifnot("key must be a character string" = is.character(key))
    stopifnot("empty key is not allowed" = nchar(key) > 0)
    key <- openssl::sha256(charToRaw(key))}

  if(is.character(cleanup))
    cleanup <- match.arg(cleanup)
  else
    stopifnot("cleanup must be one of auto, off or a numeric value" = is.numeric(cleanup))


  # ////////////////////////////////////////////////////////////////////////////
  # cache content & cleanup

  # get content
  files <- list.files(path, full.names = TRUE)
  cache_files <- data.frame(file = files,
                            date = file.mtime(files),
                            size = file.size(files))

  # cleanup obsolete files
  if(cleanup != "off"){

    # reference & tag obsolete files
    ref <- if(is.numeric(cleanup)) cleanup else timeout
    cache_files$obsolete <- as.numeric(difftime(Sys.time(), cache_files$date, units = "hours")) > ref

    # cleanup files
    if(sum(cache_files$obsolete) > 0){

      if(verbose)
        cat("Cleanup obsolete files \n")

      unlink(cache_files[cache_files$obsolete, ]$file)}

  }


  # ////////////////////////////////////////////////////////////////////////////
  # Compute hash
  # ! expr needs its arguments to be evaluated first #46

  if(verbose)
    cat("Computing cache label: \n")

  # 1. Capture the expression as a raw defused expression
  d_expr <- rlang::enquo(expr)

  # 2. Get the raw arguments from the defused expression
  u_args <- rlang::call_args(d_expr)

  # 2. Evaluate the arguments in the caller's environment
  e_args <- lapply(u_args, rlang::eval_tidy, env = rlang::quo_get_env(d_expr))

  # 3. Rebuild the call with the evaluated arguments
  n_expr <- rlang::call2(rlang::call_name(d_expr), !!!e_args)

  # 4. Convert to string
  s_expr <- rlang::as_label(n_expr)
  if(verbose)
    cat("Cache label =", s_expr, "\n")

  # 5. compute hash on expression with evaluated arguments
  hash <- openssl::sha256(s_expr)


  # ////////////////////////////////////////////////////////////////////////////
  # check cache status

  # check content
  if(!force)
    if(file.path(path, hash) %in% cache_files$file){

      if(verbose)
        cat("Found in cache, file =", file.path(path, hash), "\n")

      ts <- cache_files[cache_files$file == file.path(path, hash), ]$date

      if(as.numeric(difftime(Sys.time(), ts, units = "hours")) < timeout){

        if(verbose)
          cat("Returning response from cache \n")

        return(
          if(encrypt)
            unserialize(openssl::aes_cbc_decrypt(readRDS(file.path(path, hash)), key = key))
          else
            readRDS(file.path(path, hash)))

      } else if(verbose)
        cat("Cache content expired \n")}

  # (otherwise) eval expression
  if(verbose)
    cat("Evaluating expr \n")
  #x <- eval(expr)
  x <- rlang::eval_tidy(d_expr)

  # update cache
  if(verbose)
    cat("Update cache content \n")
  saveRDS(
    if(encrypt)
      openssl::aes_cbc_encrypt(serialize(x, NULL), key = key)
    else x,
    file.path(path, hash))


  # ////////////////////////////////////////////////////////////////////////////
  # size limit

  cache_size <- sum(cache_files$size) / 1024^2

  if(cache_size > limit){
    if(verbose)
      cat("Size limit reached / cleaning older files. \n")
    cache_files <- cache_files[order(cache_files$date, decreasing = T), ]
    cache_files$cumsize <- cumsum(cache_files$size) / 1024^2
    cleanup <- cache_files[cache_files$cumsize <= cache_size - limit, ]
    unlink(cleanup$file)}

  # return
  x

}
