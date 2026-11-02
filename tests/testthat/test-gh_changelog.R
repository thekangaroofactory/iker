
test_that("gh_changelog works", {

  expect_warning(
    expect_warning(
      expect_warning(gh_changelog(repo = "foo", milestone = 1))))

})
