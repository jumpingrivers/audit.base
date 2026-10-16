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

test_that("A version that couldn't be found is reported, not an error", {
  for (version in list(NA_character_, character(0))) {
    expect_message(audit_posit_version(version, "workbench"), "Couldn't find")
    msg = get_quarto_posit_version_msg(list(posit_version = version), "workbench")
    expect_match(msg, "couldn't find")
  }
})

test_that("lookup_version() places a version against the list", {
  versions = tibble::tibble(version = c("2026.10.0", "2026.09.0", "2026.08.2", "2026.08.0"))
  cases = tibble::tribble(
    ~posit_version, ~row,
    "2026.10.0", 1L,
    "2026.10.1", 1L, # patch after the newest entry
    "2026.10.0.pro1", 1L,
    "2030.01.0", 1L,
    "2026.08.3", 3L, # patch missing from the list
    "2026.08.0", 4L,
    "2025.01.0", NA, # older than the list
    "1.9.0", NA,
    "Not a version", NA
  )
  for (i in seq_len(nrow(cases))) {
    expect_identical(
      lookup_version(cases$posit_version[i], versions),
      cases$row[i],
      label = cases$posit_version[i]
    )
  }
})
