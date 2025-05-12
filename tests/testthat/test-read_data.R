
test_that("read_data works", {

  # ----------------------------------------------------------------------------
  # Negative tests
  # ----------------------------------------------------------------------------

  # -- path = "" (when env variable is not declared)
  expect_null(read_data(path = ""))

  # -- path does not exist
  expect_null(read_data(path = "dummy", file = filename))

  # -- file without extension
  expect_null(read_data(path = "dummy", file = "my_data"))

  # -- missing file
  expect_null(read_data(path = getwd(), file = "missing_file.csv"))


  # ----------------------------------------------------------------------------
  # Others
  # ----------------------------------------------------------------------------

  # -- create data
  create_test_data(sub_folder = "resource")

  # -- resource
  x <- read_data(path = testdata_path, file = filename, resource = "resource", verbose = TRUE)

  # -- check
  expect_true(tibble::is_tibble(x))

  # -- cleanup
  clean_test_data()

})
