
test_that("save_data works", {

  # ----------------------------------------------------------------------------
  # Negative tests
  # ----------------------------------------------------------------------------

  # -- no connector
  expect_warning(x <- save_data())
  expect_false(x)


  # ----------------------------------------------------------------------------
  # file connector
  # ----------------------------------------------------------------------------

  # -- create folder
  dir.create(testdata_path, showWarnings = FALSE)

  # -- file with resource
  x <- save_data(template_df, path = testdata_path, file = "test.csv")
  expect_type(x, "list")

  # -- cleanup
  clean_test_data()

})
