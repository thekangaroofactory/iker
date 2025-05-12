

#' Read Data
#'
#' @param path A path where to read the data (see details).
#' @param resource An optional resource name (used as sub folder inside path).
#' @param file The name of the file to read (with extension).
#' @param col_types See read_delim function from readr package.
#' @param verbose A logical whether additional trace should be sent to the console.
#'
#' @details
#' By default, path takes its value from the DATA_HOME environment variable.
#' If it's not set in the environment and no value has been provided, function will be stopped (and return NULL).
#'
#' @returns A tibble, output value of the read_delim() call or NULL if data can't be read.
#' @export
#'
#' @examples
#' \dontrun{
#' foo <- read_data(file = "my_data.csv")
#' }


read_data <- function(path = Sys.getenv("DATA_HOME"), resource = NULL, file, delim = ",", col_types = NULL, verbose = FALSE){

  # ----------------------------------------------------------------------------
  # check parameters
  # ----------------------------------------------------------------------------

  # -- check path (skip if NULL)
  if(!is.null(path))
    if(path == ""){
      message("[Warning] Path is empty, check DATA_HOME environment variable or provide a value for path argument")
      return(NULL)}

  # -- get extension
  extension <- tools::file_ext(file)
  if(extension == ""){
    message("[Warning] Can't guess extension from file")
    return(NULL)}


  # ----------------------------------------------------------------------------
  # check target path
  # ----------------------------------------------------------------------------

  # -- skip when path is NULL
  if(!is.null(path)){

    # -- check resource
    if(!is.null(resource)){

      # -- concatenate
      path <- file.path(path, resource)

      if(verbose)
        cat("[write_data] Update path with resource folder \n")}

    # -- check dir
    if(!dir.exists(path)){
      message("[Warning] Path does not exist")
      return(NULL)}

  }

  # ----------------------------------------------------------------------------
  # read the data
  # ----------------------------------------------------------------------------

  # -- init the data
  x <- NULL

  # -- case csv (default)
  if(extension == "csv"){

    # -- read the data
    tryCatch({

      cat("Iker is reading your data... \n")

      # -- read data
      # skip path if NULL
      x <- readr::read_delim(file = ifelse(is.null(path), file, file.path(path, file)),
                             delim = delim,
                             col_types = col_types)

      # -- log
      cat("-- output dim =", nrow(x), "rows /", ncol(x), "columns \n")},

      # -- error
      error = function(e)
        message(e$message))}


  # -- return the data
  x

}
