test_that("P3M's reason for rejecting a system reaches the user", {
  local_mocked_bindings(read_p3m = function(url) list(error = "Unsupported system"))
  expect_warning(suppressMessages(get_pkg_requirements("rockylinux", "8")), "Unsupported system")
  expect_identical(suppressMessages(suppressWarnings(get_pkg_requirements("rockylinux", "8"))), NA)
})
