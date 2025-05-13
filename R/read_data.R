

#' Read Data
#'
#' @param path A path where to read the data (see details).
#' @param resource An optional resource name (used as sub folder inside path).
#' @param file The name of the file to read (with extension).
#' @param delim Single character used to separate fields within a record.
#' @param col_types See read_delim function from readr package.
#' @param verbose A logical whether additional trace should be sent to the console.
#'
#' @details
#' By default, path takes its value from the DATA_HOME environment variable.
#' If it's not set in the environment and no value has been provided, function will be stopped (and return NULL).
#' When NULL, it will be ignored (assuming file contains the full path to the file)
#'
#' col_types will be used to detect if POSIXct columns are expected. In this case
#' they will be read as character first (expecting an ISO-8601 compatible string)
#' then converted to POSIXct.
#'
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
      warning("[Iker] Empty path, check DATA_HOME environment variable or provide a value for the argument")
      return(NULL)}

  # -- get extension
  extension <- tools::file_ext(file)
  if(extension == ""){
    warning("[Iker] Can't guess extension from file")
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
        cat("[Iker] Update path with resource folder \n")}

    # -- check dir
    if(!dir.exists(path)){
      warning("[Iker] Path does not exist")
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

      cat("[Iker] Reading data from file... \n")

      # ------------------------------------------------------------------------
      # Ensure timezone continuity (ISO-8601)
      # ------------------------------------------------------------------------
      # datetime will be read as character (and converted later)

      # -- get datetime index & name
      idx_ct <- which(col_types %in% "POSIXct")
      names_ct <- names(col_types[idx_ct])

      # -- convert POSIXct classes
      if(length(idx_ct) > 0)
        col_types[idx_ct] <- "character"

      # ------------------------------------------------------------------------

      # -- read data
      # skip path if NULL
      x <- readr::read_delim(file = ifelse(is.null(path), file, file.path(path, file)),
                             delim = delim,
                             col_types = col_types,
                             show_col_types = FALSE)


      # -- convert back to POSIXct (ISO-8601)
      if(length(idx_ct) > 0)
        x <- to_POSIXct(x, names_ct)

      # -- log
      cat("- output dim =", nrow(x), "x", ncol(x), "\n")

      },

      # -- error
      error = function(e)
        warning(e$message))}


  # -- return the data
  x

}
