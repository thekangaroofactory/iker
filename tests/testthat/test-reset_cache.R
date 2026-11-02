
# -- set test env
cache_dir <- file.path(test_path(), "test_cache")
dir.create(cache_dir)
Sys.setenv("R_IKER_CACHE_PATH" = cache_dir)
file.create(paste(cache_dir, c("f1", "f2"), sep = "/"))

# -- test
test_that("reset_cache works", {

  expect_message(reset_cache(force = T))
  expect_length(list.files(cache_dir), 0L)

  # basic call
  expect_no_condition(reset_cache())

  # path does not exist
  expect_warning(reset_cache(path = "dummy/path"))

})


# environment cleanup
unlink(cache_dir, recursive = TRUE)
Sys.unsetenv("R_IKER_CACHE_PATH")
