
test_that("to_POSIXct works", {

  # ----------------------------------------------------------------------------
  # tests
  # ----------------------------------------------------------------------------

  # -- convert POSIXct to ISO-8601
  x <- to_POSIXct(data.frame(a = "2025-05-13T12:15:30+0200"), "a")
  expect_equal(x$a, as.POSIXct("2025-05-13T12:15:30+0200", format = "%Y-%m-%dT%H:%M:%S%z", tz = Sys.timezone()))

})
