test_that("get_posit_versions() reads versions and CVEs from the NEWS page", {
  local_posit_news()
  versions = get_posit_versions("connect")
  expect_identical(unique(versions$version), c("2026.10.0", "2025.09.0", "2025.07.0"))
  cves = sort(unique(stats::na.omit(versions$cve)))
  expect_identical(cves, c("CVE-2025-47906", "CVE-2025-47907"))

  expect_message(audit_posit_version("2026.10.0", "connect"), "up to date")
  expect_message(audit_posit_version("2026.10.0.pro1", "connect"), "up to date")
  expect_message(audit_posit_version("2025.07.0", "connect"), "2 newer versions that fix 2 CVEs")
})

test_that("get_posit_versions() falls back to the cache when the NEWS page can't be read", {
  local_mocked_bindings(read_posit_news = function(url) stop("HTTP 503"))
  for (type in c("connect", "workbench")) {
    expect_message(get_posit_versions(type), "HTTP 503.*Falling back to cache")
    versions = suppressMessages(get_posit_versions(type))
    expect_identical(versions, get_posit_versions(type, remote = FALSE))
    expect_gt(nrow(versions), 2)
  }
  expect_gt(nrow(get_posit_versions("drivers")), 2)
})

test_that("The cached Posit versions are up to date", {
  skip_if_offline("docs.posit.co")
  for (type in c("connect", "workbench")) {
    remote = get_posit_remote_versions(type)
    local = get_posit_versions(type, remote = FALSE)
    expect_true(all(remote$version %in% local$version), info = "Try running update_all_versions()")
  }
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
