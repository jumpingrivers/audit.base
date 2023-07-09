test_that("OS tests", {
  # Avoid errors on CI Windows testing
  if (Sys.info()["sysname"] != "Linux") testthat::skip()
  libs = get_installed_libs()
  expect_gt(length(libs), 1000)
})
