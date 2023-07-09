test_that("Testing sys deps", {
  if (Sys.info()["sysname"] != "Linux") testthat::skip()
  os_release = readLines("/etc/os-release")
  installed_libs = get_installed_libs()
  deps = check_sys_deps(os_release, installed_libs)
  expect_equal(colnames(deps), c("pkg", "sys_libs"))

})
