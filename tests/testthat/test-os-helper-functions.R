test_that("OS tests", {
  libs = get_installed_libs()
  expect_gt(length(libs), 1000)
})
