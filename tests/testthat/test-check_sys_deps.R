test_that("Testing sys deps", {
  testthat::skip_on_ci()
  if (Sys.info()["sysname"] != "Linux") testthat::skip()
  os_release = readLines("/etc/os-release")
  installed_libs = get_installed_libs()
  deps = check_sys_deps(os_release, installed_libs)
  expect_equal(colnames(deps), c("pkg", "sys_libs"))

  out = list(sys_deps = deps)
  q = get_quarto_sys_deps(out)
  expect_equal(colnames(q), c("sys_libs", "pkg", "n"))
  expect_true(all(is.integer(q$n)))

  # Empty state
  out = list(sys_deps = tibble::tibble(
    pkg = character(0),
    sys_libs = character(0)
  ))
  q = get_quarto_sys_deps(out)
  expect_equal(colnames(q), c("sys_libs", "pkg", "n"))
})
