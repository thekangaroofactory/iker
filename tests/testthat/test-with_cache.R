
# environment
cache_dir <- file.path(test_path(), "test_cache")
dir.create(cache_dir)
Sys.setenv("R_IKER_CACHE_PATH" = cache_dir)
Sys.setenv("R_IKER_CACHE_KEY" = rstring())

test_that("with_cache works", {

  # baseline
  x <- with_cache(Sys.time(), verbose = T)
  expect_type(x, "double")
  expect_length(list.files(path = cache_dir), 1)

  # recall
  y <- with_cache(Sys.time(), verbose = T)
  expect_identical(y, x)
  expect_length(list.files(path = cache_dir), 1)

  # limit
  # with 0, above file should be deleted so still 1 file after the test
  y <- with_cache(Sys.Date(), limit = 0, verbose = T)
  expect_length(list.files(path = cache_dir), 1)

})

# environment cleanup
unlink(cache_dir, recursive = TRUE)
Sys.unsetenv("R_IKER_CACHE_PATH")
Sys.unsetenv("R_IKER_CACHE_KEY")
