test_that("Testing software versions", {
  versions = get_latest_versions()
  v_colnames = c("software", "version", "major", "patch", "version_num")
  expect_equal(colnames(versions), v_colnames)
  expect_gte(nrow(versions), 16)

  installed = tibble::tibble(
    software = c("r", "r", "python"),
    installed_version = c("3.4.3", "3.5.3", "3.7.1")
  )
  augmented = augment_installed(installed)
  expect_equal(colnames(augmented), c(
    v_colnames[1:4],
    paste0("installed_", c("version", "patch")),
    "upgrade"
  ))
  expect_equal(sum(!augmented$upgrade), 1)

  # Check specific packages
  augmented = augmented[!is.na(augmented$installed_version), ]
  expect_equal(augmented$installed_patch, c(1, 3, 3))

  # Test really old versions: Upgrade old version
  installed = tibble::tibble(software = "r", installed_version = "1.5.3")
  augmented = augment_installed(installed)
  expect_equal(sum(augmented$upgrade), 10)

  # Test version not in DB - don't upgrade
  installed = tibble::tibble(software = "r", installed_version = "99.4.3")
  augmented = augment_installed(installed)
  expect_equal(sum(augmented$upgrade), 9)
})

test_that("Testing software versions quarto output", {
  installed = tibble::tibble(
    software = c("r", "r", "python"),
    installed_version = c("3.4.3", "3.5.3", "3.7.1")
  )
  augmented = suppressMessages(augment_installed(installed))
  out = list(versions = augmented)
  q = get_quarto_software_versions(out)
  expect_equal(colnames(q), c("software", "version", "installed_version", "upgrade"))
  expect_true(all(is.logical(q$upgrade)))
})

test_that("Ensure that software versions are up to date", {
  testthat::skip_on_ci()
  versions = get_latest_versions()
  latest = create_software_tibble()
  # If this test files, try running update_software_csv() first
  expect_true(all(latest$version %in% versions$version))
})
