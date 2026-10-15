test_that("Testing software versions", {
  versions = get_latest_versions(remote = FALSE)
  v_colnames = c("software", "version", "major", "patch", "version_num", "within_eol")
  expect_equal(colnames(versions), v_colnames)
  expect_gte(nrow(versions), 16)
})

test_that("Testing augment_installed (empty)", {
  installed = tibble::tibble(software = character(0), installed_version = character(0))
  augmented_empty = augment_installed(installed, remote = FALSE, verbose = FALSE)
  expect_equal(
    colnames(augmented_empty),
    c(
      "software",
      "version",
      "major",
      "patch",
      "within_eol",
      paste0("installed_", c("version", "patch")),
      "upgrade"
    )
  )
})

test_that("Testing augment_installed (old)", {
  augmented_empty =
    augment_installed(
      tibble::tibble(
        software = character(0),
        installed_version = character(0)
      ),
      remote = FALSE,
      verbose = FALSE
    )

  # All really old versions
  installed = tibble::tibble(
    software = c("r", "python"),
    installed_version = c("2.4.1", "3.2.1")
  )
  augmented = augment_installed(installed, remote = FALSE, verbose = FALSE)
  expect_equal(nrow(augmented) - nrow(augmented_empty), 2)
  expect_equal(sum(!augmented$upgrade), 0)

  installed_augmented_versions = dplyr::left_join(
    installed,
    augmented,
    by = dplyr::join_by(software, installed_version)
  )
  expect_equal(
    nrow(installed_augmented_versions),
    2
  )

  expect_equal(sum(installed_augmented_versions$within_eol), 0)
  expect_equal(sum(installed_augmented_versions$upgrade), 2)
})

test_that("Testing augment_installed (new)", {
  # Test really old versions: Upgrade old version
  installed = tibble::tibble(software = "r", installed_version = "4.4.3")
  augmented = augment_installed(installed, remote = FALSE, verbose = FALSE)
  augmented = augmented[augmented$version == "4.4.3", ]
  expect_equal(nrow(augmented), 1)
  expect_false(augmented$upgrade)
})

test_that("Testing augment_installed (future))", {
  # Test version not in DB - don't upgrade
  installed = tibble::tibble(software = "r", installed_version = "99.4.3")
  augmented = augment_installed(installed, remote = FALSE, verbose = FALSE)
  augmented = augmented[augmented$version == "99.4.3", ]
  expect_equal(nrow(augmented), 1)
  expect_false(augmented$upgrade)
})

test_that("Ensure that software versions are up to date", {
  testthat::skip_on_ci()
  versions = get_latest_versions(remote = FALSE)
  latest = get_latest_versions_remote()

  expect_true(
    all(latest$version %in% versions$version),
    info = "Try running update_all_versions()"
  )
})

test_that("get_latest_versions() falls back to cache when remote lookup fails", {
  local_mocked_bindings(
    get_latest_versions_remote = function() stop("HTTP 404")
  )
  expect_message(get_latest_versions(remote = TRUE), "Using cached version")
  versions = suppressMessages(get_latest_versions(remote = TRUE))
  expect_equal(versions, suppressMessages(get_latest_versions(remote = FALSE)))
})

test_that("Versions are compared as numbers, not text", {
  expect_equal(
    version_compare(c("1.10", "1.9", NA), c("1.9", "1.10", "1.9"), `>`),
    c(TRUE, FALSE, NA)
  )
  expect_equal(max_version(c("1.9", "1.10", "1.3")), "1.10")
  expect_equal(min_version(c("1.9", "1.10", "1.3")), "1.3")
})

test_that("A release newer than the version list isn't reported as past EOL", {
  local_mocked_bindings(
    get_latest_versions = function(remote = FALSE) {
      versions = tibble::tibble(
        software = factor("quarto"),
        version = c("1.8.27", "1.9.38")
      )
      versions$major = get_major(versions$version)
      versions$patch = get_patch(versions$version)
      versions$version_num = c(2L, 1L)
      versions$within_eol = TRUE
      dplyr::group_by(versions, .data$software)
    }
  )
  installed = tibble::tibble(software = "quarto", installed_version = "1.10.18")
  augmented = augment_installed(installed, remote = FALSE, verbose = FALSE)
  row = augmented[augmented$installed_version %in% "1.10.18", ]
  expect_true(row$within_eol)
  expect_false(row$upgrade)
  expect_message(print_colour_version(row), "newer than")
})
