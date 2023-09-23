test_that("Testing check server", {

  types = c("workbench", "connect", "drivers")
  for (type in types) {
    versions = get_posit_versions(type)
    expect_equal(ncol(versions), 3)
    expect_gt(nrow(versions), 2)
  }
  type = "connect"
  versions = get_posit_versions(type)
  # Up to date
  latest_version = versions[1, ]$version

  expect_message(audit_posit_version(latest_version, type),
                 regexp = "up to date")

  expect_message(audit_posit_version(paste0(latest_version, ".pro1"), type),
                 regexp = "up to date")

  # Version not in DB
  expect_equal(lookup_version(posit_version = "2029.01.01", type), 1)
  # Really old version not in the DB
  expect_equal(lookup_version(posit_version = "2010.01.01", type),
               NA_integer_)

  expect_equal(lookup_version(posit_version = "Not in DB", type),
               NA_integer_)

  # No version in DB
  v = lookup_version(posit_version = "2022.10.31", type)
  expect_true(is.na(v))

  expect_message(audit_posit_version("2022.10.0", type),
                 regexp = "out of date")
})
