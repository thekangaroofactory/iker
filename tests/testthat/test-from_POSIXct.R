
test_that("from_POSIXct works", {

  # ----------------------------------------------------------------------------
  # tests
  # ----------------------------------------------------------------------------

  # -- convert POSIXct to ISO-8601
  x <- from_POSIXct(data.frame(a = as.POSIXct("2025-05-13 12:15:30")))
  expect_equal(as.character(x$a), "2025-05-13T12:15:30+0200")

  # -- test without POSIXct (should not alter Date)
  x <- from_POSIXct(data.frame(a = as.Date("2025-05-13")))
  expect_identical(x$a, as.Date("2025-05-13"))

})
