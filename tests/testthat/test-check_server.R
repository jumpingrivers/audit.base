test_that("Testing check server", {
  versions = get_posit_versions("workbench")
  expect_equal(ncol(versions), 3)
  expect_gt(nrow(versions), 2)

  versions = get_posit_versions("connect")
  expect_equal(ncol(versions), 3)
  expect_gt(nrow(versions), 20)

  # Version not in DB
  expect_equal(lookup_version(server_version = "2029.01.01", type = "connect"), 1)
  # Really old version not in the DB
  expect_equal(lookup_version(server_version = "2010.01.01", type = "connect"),
               nrow(versions) + 1)

  expect_equal(lookup_version(server_version = "Not in DB", type = "connect"),
               nrow(versions) + 1)

  # No version in DB
  v = lookup_version(server_version = "2022.10.31", type = "connect")
  expect_true(is.na(v))

  expect_message(audit_server_version("2022.10.0", type = "connect"), regexp = "out of date")
})
