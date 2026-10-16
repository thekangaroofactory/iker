
test_that("rstring works", {

  # baseline
  x <- rstring()
  expect_type(x, "character")
  expect_equal(nchar(x), 30)

  # specific length
  x <- rstring(n = 10)
  expect_type(x, "character")
  expect_equal(nchar(x), 10)

})
