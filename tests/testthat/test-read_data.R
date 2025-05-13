
test_that("read_data works", {

  # ----------------------------------------------------------------------------
  # Negative tests
  # ----------------------------------------------------------------------------

  # -- path = "" (when env variable is not declared)
  expect_warning(x <- read_data(path = ""))
  expect_null(x)

  # -- path does not exist
  expect_warning(x <- read_data(path = "dummy", file = filename))
  expect_null(x)

  # -- file without extension
  expect_warning(x <- read_data(path = "dummy", file = "my_data"))
  expect_null(x)

  # -- missing file
  expect_warning(x <- read_data(path = getwd(), file = "missing_file.csv"))
  expect_null(x)


  # ----------------------------------------------------------------------------
  # Others
  # ----------------------------------------------------------------------------

  # -- create data
  create_test_data(sub_folder = "resource")

  # -- resource
  x <- read_data(path = testdata_path, file = filename, resource = "resource", verbose = TRUE)
  expect_type(x, "list")

  # -- POSIXct
  x <- read_data(path = testdata_path, file = filename, resource = "resource", col_types = template_col_types, verbose = TRUE)
  expect_true(inherits(x$datetime, "POSIXct"))

  # -- cleanup
  clean_test_data()

})
