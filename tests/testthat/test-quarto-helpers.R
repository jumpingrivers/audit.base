test_that("Quarto Outputs", {
  out = list()
  out$posit_version = "2022.10.0"
  msg = get_quarto_posit_version_msg(out, type = "connect")
  expect_true(stringr::str_detect(msg, "CVEs"))

  out$posit_version = "2012.10.0"
  msg = get_quarto_posit_version_msg(out, "connect")
  expect_true(stringr::str_detect(msg, "in our database"))

  out$posit_version = get_posit_versions("connect")[1, ]$version
  msg = get_quarto_posit_version_msg(out, "connect")
  expect_true(stringr::str_detect(msg, "is up to date"))

  out = list(server_headers = serverHeaders::check("google.com"))
  q = get_quarto_server_header(out)
  expect_equal(colnames(q), c("header", "status", "message", "value"))
  expect_true(all(is.list(q$message)))
  expect_true(nrow(q) >= 6)
})

test_that("Posit version status matches the message", {
  out = list(posit_version = "2022.10.0")
  expect_equal(get_quarto_posit_version_status(out, "connect"), "outdated")

  out$posit_version = "2012.10.0"
  expect_equal(get_quarto_posit_version_status(out, "connect"), "unknown")

  out$posit_version = get_posit_versions("connect")[1, ]$version
  expect_equal(get_quarto_posit_version_status(out, "connect"), "current")
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
