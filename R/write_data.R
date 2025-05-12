

#' Write Data
#'
#' @param x A data frame or tibble to write to disk.
#' @param path A path where to write the data (see details).
#' @param resource An optional resource name (used as sub folder inside path).
#' @param file The name of the file to write (with extension)
#' @param verbose If TRUE, additional messages will be sent to the console
#'
#' @details
#' By default, path takes its value from the DATA_HOME environment variable.
#' If it's not set in the environment and no value has been provided, function will be stopped (and return FALSE).
#' If the corresponding path does not exist, it won't be created - to avoid creating data inside a container if the mount has not been done for example.
#' When NULL, no check will be done.
#'
#' Resource is an optional parameter to allow the use of sub folder inside path.
#' If not NULL, it will be concatenated to path (and created inside path if it does not exist)
#' When path is NULL, it will be ignored.
#'
#' @returns returns the input x invisibly, or FALSE when it fails
#' @export
#'
#' @examples
#' \dontrun{
#' write_data(foo, file = "my_data.csv")
#' }


write_data <- function(x, path = Sys.getenv("DATA_HOME"), resource = NULL, file, delim = ",", verbose = FALSE){

  # ----------------------------------------------------------------------------
  # check parameters
  # ----------------------------------------------------------------------------

  # -- check path (skip if NULL)
  if(!is.null(path))
    if(path == ""){
      message("[Warning] Path is empty, check DATA_HOME environment variable or provide a value for path argument")
      return(FALSE)}

  # -- get extension
  extension <- tools::file_ext(file)
  if(extension == ""){
    message("[Warning] Can't guess extension from file - csv will be applied by default")
    extension <- "csv"}


  # ----------------------------------------------------------------------------
  # check target path
  # ----------------------------------------------------------------------------

  # -- skip if NULL
  if(!is.null(path)){

    # -- check base path
    if(!dir.exists(path)){
      message("[Warning] Path does not exist - can't save the data")
      return(FALSE)}

    # -- check resource path
    if(!is.null(resource)){

      # -- concatenate
      path <- file.path(path, resource)

      if(verbose)
        cat("[write_data] Update path with resource folder \n")

      # -- check new path
      if(!dir.exists(path))
        dir.create(path)}

  }


  # ----------------------------------------------------------------------------
  # write the data
  # ----------------------------------------------------------------------------

  # -- init
  res <- FALSE

  # -- case csv (default)
  if(extension == "csv"){

    tryCatch({

      cat("Iker is writing your data... \n")

      # -- expression
      res <- readr::write_delim(x = x,
                                file = ifelse(is.null(path), file, file.path(path, file)),
                                delim = delim)},

      # -- error
      error = function(e)
        message(e$message))}

  # -- return
  res

}
