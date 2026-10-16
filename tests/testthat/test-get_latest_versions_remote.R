test_that("latest_patch_per_minor() keeps every minor when a new one ships", {
  versions = c("1.11.2", "1.10.19", "1.10.3", "1.11.0", "1.9.38", "1.12.0-rc1")
  expect_identical(latest_patch_per_minor(versions), c("1.11.2", "1.10.19", "1.9.38"))
})

test_that("get_latest_versions_remote() keeps the latest stable patch of each release", {
  local_version_json()
  versions = get_latest_versions_remote()
  # Quarto 1.0 to 1.6 come from the fixed list; prereleases, musl and macOS builds are dropped
  expected = tibble::tibble(
    software = rep(c("python", "quarto", "r"), c(2, 9, 2)),
    version = c(
      "3.14.8",
      "3.13.16",
      "1.10.19",
      "1.9.38",
      "1.6.43",
      "1.5.57",
      "1.4.557",
      "1.3.450",
      "1.2.475",
      "1.1.189",
      "1.0.38",
      "4.6.1",
      "4.5.3"
    )
  )
  expect_identical(versions, expected)
})
