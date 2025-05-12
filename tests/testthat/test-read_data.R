
test_that("read_data works", {

  # -- path = "" (when env variable is not declared)
  expect_null(read_data(path = ""))

  # -- path does not exist
  expect_null(read_data(path = "dummy", file = "my_data.csv"))

  # -- file without extension
  expect_null(read_data(path = "dummy", file = "my_data"))

  # -- missing file
  expect_null(read_data(path = getwd(), file = "my_data.csv"))


})
