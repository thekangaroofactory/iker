
# ------------------------------------------------------------------------------
# Declare objects
# ------------------------------------------------------------------------------

# -- test folder
testdata_path <- file.path(system.file("tests", "testthat", package = "iker"), "testdata")

# -- test file
filename <- "my_data.csv"

# -- template data
template_df <- data.frame(id = c(1, 2, 3),
                          name = c("john", "joe", "jayden"))


# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# -- helper: create data
create_test_data <- function(sub_folder = NULL){

  # -- check param
  target_dir <- ifelse(is.null(sub_folder), testdata_path, file.path(testdata_path, sub_folder))

  # -- create folder
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  # -- create test file
  readr::write_delim(template_df, file = file.path(target_dir, filename), delim = ";")

}

# -- helper: cleanup data
clean_test_data <- function(){

  unlink(testdata_path, recursive = TRUE)

}
