test_that("Testing check server", {
  types = c("workbench", "connect", "drivers")
  for (type in types) {
    versions = get_posit_versions(type)
    expect_equal(ncol(versions), 2)
    expect_gt(nrow(versions), 2)
  }
  type = "connect"
  versions = get_posit_versions(type)
  # Up to date
  latest_version = versions[1, ]$version

  expect_message(
    audit_posit_version(latest_version, type),
    regexp = "up to date"
  )

  expect_message(
    audit_posit_version(paste0(latest_version, ".pro1"), type),
    regexp = "up to date"
  )

  # Version not in DB
  expect_equal(lookup_version(posit_version = "2029.01.01", type), 1)
  # Really old version not in the DB
  expect_equal(
    lookup_version(posit_version = "2010.01.01", type),
    NA_integer_
  )

  expect_equal(
    lookup_version(posit_version = "Not in DB", type),
    NA_integer_
  )

  # No version in DB
  v = lookup_version(posit_version = "2022.10.31", type)
  expect_true(is.na(v))

  expect_message(audit_posit_version("2022.10.0", type), regexp = "out of date")
})

test_that("Testing check Posit versions", {
  type = "connect"
  remote = get_posit_remote_versions(type)
  local = get_posit_versions(type, remote = FALSE)
  expect_true(
    all(remote$version %in% local$version),
    info = "Try running update_all_versions()"
  )
  type = "workbench"
  remote = get_posit_remote_versions(type)
  local = get_posit_versions(type, remote = FALSE)
  expect_true(
    all(remote$version %in% local$version),
    info = "Try running update_all_versions()"
  )
})

test_that("get_posit_versions() keeps CVE IDs and marks versions without one as NA", {
  versions = get_posit_versions("connect", remote = FALSE)
  expect_type(versions$cve, "character")
  expect_false(any(versions$cve == "", na.rm = TRUE))
  expect_true("CVE-2021-38297" %in% versions$cve)
  expect_true(all(grepl("^CVE-", stats::na.omit(versions$cve))))
})

test_that("count_cves() counts each CVE once", {
  versions = tibble::tibble(
    version = c("2025.03.0", "2025.03.0", "2025.03.0", "2025.02.0"),
    cve = c("CVE-2025-0001", "CVE-2025-0002", "CVE-2025-0001", NA)
  )
  expect_equal(count_cves(versions), 2L)
  expect_equal(count_cves(versions[4, ]), 0L)
})
