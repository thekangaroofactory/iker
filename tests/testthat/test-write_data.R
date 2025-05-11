
test_that("write_data works", {

  # -- path = "" (when env variable is not declared)
  expect_false(write_data(path = ""))

  # -- path does not exist
  expect_false(write_data(path = "some/dummy/path", file = "test.csv"))

  # -- write_delim failure (when x is not a data.frame)
  expect_error(write_data(x = 123, path = getwd(), file = "test.csv"))

})
