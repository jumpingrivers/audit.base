test_that("Quarto Outputs", {
  out = list()
  out$posit_version = "2022.10.0"
  msg = get_quarto_posit_version_msg(out, "connect")
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
