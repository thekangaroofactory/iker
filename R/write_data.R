

#' Write Data
#'
#' @param x A data frame to write to disk.
#' @param path A path where to write the data (see details).
#' @param resource An optional resource name (used as sub folder inside path).
#' @param file The name of the file to write (with extension)
#' @param delim Single character used to separate fields within a record.
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
#' Columns with POSIXct type will be converted to ISO-8601 character columns before writing the file
#'
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
      warning("[Iker] Empty path, check DATA_HOME environment variable or provide a value for the argument")
      return(FALSE)}

  # -- get extension
  extension <- tools::file_ext(file)
  if(extension == ""){
    message("[Iker] Can't guess extension from file - csv will be applied by default")
    extension <- "csv"}


  # ----------------------------------------------------------------------------
  # check target path
  # ----------------------------------------------------------------------------

  # -- skip if NULL
  if(!is.null(path)){

    # -- check base path
    if(!dir.exists(path)){
      warning("[Iker] Path does not exist - can't save the data!")
      return(FALSE)}

    # -- check resource path
    if(!is.null(resource)){

      # -- concatenate
      path <- file.path(path, resource)

      if(verbose)
        ktools::catl("[Iker] Update path with resource folder \n", level = 2)

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

      # -- Convert tibble to data.frame
      # (otherwise from_POSIXct will fail)
      if(tibble::is_tibble(x))
        x <- as.data.frame(x)

      # -- Ensure datetime continuity (ISO-8601)
      x <- from_POSIXct(x)

      ktools::catl("[Iker] Writing data to file... \n")

      # -- expression
      res <- readr::write_delim(x = x,
                                file = ifelse(is.null(path), file, file.path(path, file)),
                                delim = delim)},

      # -- error
      error = function(e)
        warning(e$message))}

  # -- return
  res

}
