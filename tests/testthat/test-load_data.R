
test_that("load_data works", {

  # ----------------------------------------------------------------------------
  # Negative tests
  # ----------------------------------------------------------------------------

  # -- no connector
  expect_warning(x <- load_data())
  expect_null(x)


  # ----------------------------------------------------------------------------
  # Others
  # ----------------------------------------------------------------------------

  # -- create data
  create_test_data(sub_folder = "resource")

  # -- file connector
  x <- load_data(file = filename, path = testdata_path, resource = "resource")
  expect_type(x, "list")

  # -- cleanup
  clean_test_data()

})
