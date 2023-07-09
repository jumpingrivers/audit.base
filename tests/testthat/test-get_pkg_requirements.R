test_that("Test pkg requirements", {
  pkgs = get_pkg_requirements(distribution = "ubuntu",
                              release = "22.04")
  expect_true(ncol(pkgs) == 2)
  expect_true(nrow(pkgs) > 750)
  expect_true("fftw" %in% pkgs$pkg)
})
