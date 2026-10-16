test_that("latest_patch_per_minor() keeps every minor when a new one ships", {
  versions = c("1.11.2", "1.10.19", "1.10.3", "1.11.0", "1.9.38", "1.12.0-rc1")
  expect_identical(latest_patch_per_minor(versions), c("1.11.2", "1.10.19", "1.9.38"))
})
