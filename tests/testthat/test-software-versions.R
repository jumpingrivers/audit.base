test_that("Testing software versions", {
  versions = get_latest_versions()
  v_colnames = c("software", "version", "major", "patch", "version_num")
  expect_equal(colnames(versions), v_colnames)
  expect_gte(nrow(versions), 16)


  installed = tibble::tibble(software = c("r", "r", "python"),
                             installed_version = c("3.4.3", "3.5.3", "3.7.1"))
  augmented = augment_installed(installed)
  expect_equal(colnames(augmented), c(v_colnames[1:4],
                                      paste0("installed_", c("version", "patch")),
                                      "upgrade"))
  expect_equal(sum(!augmented$upgrade), 2)

  # Check specific packages
  augmented = augmented[!is.na(augmented$installed_version), ]
  expect_equal(augmented$installed_patch, c(3, 3, 1))

  # Test really old versions
  installed = tibble::tibble(software = c("r"),
                             installed_version = c("1.5.3"))
  augmented = augment_installed(installed)
  expect_equal(sum(augmented$upgrade), 10)

  # Test version that's not
  installed = tibble::tibble(software = c("r"),
                             installed_version = c("99.4.3"))

  augmented = augment_installed(installed)
  expect_equal(sum(augmented$upgrade), 9)
})
