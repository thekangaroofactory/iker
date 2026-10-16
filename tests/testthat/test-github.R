
test_that("multiplication works", {

  # baseline: just going through the code
  # there is no way to perform real test without user_agent/token

  expect_warning(
    expect_warning(
      x <- github(url = "https://api.github.com",
                  owner = "thekangaroofactory",
                  user_agent = "dummy",
                  token = "dummy",
                  verbose = TRUE)))

  expect_null(x)


  # milestones:
  # just going through specific code

  expect_warning(
    expect_warning(
      x <- github(url = "https://api.github.com",
                  resource = "milestones",
                  repo = "foo",
                  owner = "thekangaroofactory",
                  user_agent = "dummy",
                  token = "dummy",
                  verbose = TRUE)))

  expect_null(x)

})
