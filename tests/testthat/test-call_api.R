
test_that("call_api works", {

  # baseline
  res <- call_api(url = "https://postman-echo.com/get?foo1=bar1&foo2=bar2",
                  header = list())

  expect_equal(res$status_code, 200)

  # header
  res <- call_api(url = "https://postman-echo.com/headers",
                  header = list("my-sample-header" = "Lorem ipsum dolor sit amet"))

  expect_equal(res$status_code, 200)

  # options
  res <- call_api(url = "https://postman-echo.com/post",
                  header = list("Content-Type" = "application/json"),
                  options = list("copypostfields" = "{\"foo1\":\"bar1\",\"foo2\":\"bar2\"}"))

  expect_equal(res$status_code, 200)

  # error (+ verbose)
  expect_warning(res <- call_api(url = "https://postman-echo.com/dummy",
                                 header = list(),
                                 verbose = TRUE))

  expect_equal(res$status_code, 404)

  # check params
  expect_error(call_api(url = 123))
  expect_error(call_api(url = "123"))
  expect_error(call_api(url = "https://123", header = 1))
  expect_error(call_api(url = "https://123", header = list(1), options = 1))

})
