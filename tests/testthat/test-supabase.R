
test_that("multiplication works", {

  # baseline:
  # there is no way to test the function without Supabase token
  # > just going through the code

  expect_warning(
    expect_warning(
      x <- supabase_select(table = "dummy",
                      url = "https://postman-echo.com/",
                      query = "select=*",
                      verbose = TRUE)))

  expect_null(x)

  # upsert:
  # just going through the code

  expect_warning(
    expect_warning(
      x <- supabase_upsert(table = "dummy",
                           data = data.frame(foo = 1),
                           url = "https://postman-echo.com/",
                           verbose = TRUE)))

  expect_false(x)

  # delete:
  # just going through the code

  expect_warning(
    expect_warning(
      x <- supabase_delete(table = "dummy",
                           values = c(1, 2),
                           url = "https://postman-echo.com/",
                           verbose = TRUE)))

  expect_false(x)

})
