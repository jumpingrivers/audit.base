test_that("The Posit version message and status agree", {
  local_posit_news()
  cases = tibble::tribble(
    ~posit_version, ~status, ~msg,
    "2026.10.0", "current", "is up to date",
    "2025.07.0", "outdated", "fix 2 CVEs",
    "2012.10.0", "unknown", "in our database"
  )
  for (i in seq_len(nrow(cases))) {
    out = list(posit_version = cases$posit_version[i])
    msg = get_quarto_posit_version_msg(out, "connect")
    expect_match(msg, cases$msg[i], label = cases$posit_version[i])
    expect_identical(get_quarto_posit_version_status(out, "connect"), cases$status[i])
  }
})

test_that("Server headers are tabulated for the report", {
  skip_if_offline("google.com")
  out = list(server_headers = serverHeaders::check("google.com"))
  q = get_quarto_server_header(out)
  expect_equal(colnames(q), c("header", "status", "message", "value"))
  expect_true(all(is.list(q$message)))
  expect_true(nrow(q) >= 6)
})

test_that("version_status maps row numbers", {
  expect_equal(version_status(NA_integer_), "unknown")
  expect_equal(version_status(1L), "current")
  expect_equal(version_status(4L), "outdated")
})

test_that("Posit version message counts the CVEs fixed by newer versions", {
  local_mocked_bindings(get_posit_versions = function(type, remote = TRUE) {
    tibble::tibble(
      version = c("2025.03.0", "2025.03.0", "2025.03.0", "2025.02.0", "2025.01.0"),
      cve = c("CVE-2025-0001", "CVE-2025-0002", "CVE-2025-0001", NA, NA)
    )
  })
  msg = get_quarto_posit_version_msg(list(posit_version = "2025.01.0"), "connect")
  expect_match(msg, "There are 2 newer versions that fix 2 CVEs", fixed = TRUE)
})
