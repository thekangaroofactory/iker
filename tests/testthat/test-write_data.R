
test_that("write_data works", {

  # ----------------------------------------------------------------------------
  # Negative tests
  # ----------------------------------------------------------------------------

  # -- path = "" (when env variable is not declared)
  expect_false(write_data(path = ""))

  # -- path does not exist
  expect_false(write_data(path = "some/dummy/path", file = "test.csv"))

  # -- write_delim failure (when x is not a data.frame)
  expect_false(write_data(x = 123, path = getwd(), file = "test.csv"))


  # ----------------------------------------------------------------------------
  # test: basic save
  # ----------------------------------------------------------------------------

  # -- create folder
  dir.create(testdata_path, showWarnings = FALSE)

  # -- file without extension
  x <- write_data(template_df, path = testdata_path, file = "test")

  # -- test
  expect_type(x, "list")
  expect_true(file.exists(file.path(testdata_path, "test")))

  # -- cleanup
  clean_test_data()


  # ----------------------------------------------------------------------------
  # test: save with resource
  # ----------------------------------------------------------------------------

  # -- create folder
  dir.create(testdata_path, showWarnings = FALSE)

  # -- file with resource
  x <- write_data(template_df, path = testdata_path, resource = "resource", file = "test.csv", verbose = TRUE)

  # -- test
  expect_type(x, "list")
  expect_true(file.exists(file.path(testdata_path, "resource", "test.csv")))

  # -- cleanup
  clean_test_data()

})
