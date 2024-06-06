#' Augments installed software columns
#' @param installed A tibble with columns software and installed_version
#' @param verbose Default TRUE.
#' @export
augment_installed = function(installed, verbose = TRUE) {
  installed$installed_major = get_major(installed$installed_version)
  installed$installed_patch = get_patch(installed$installed_version)
  installed = in_db(installed)
  installed = add_upgrade_column(installed)
  installed$major = package_version(installed$major)
  installed = dplyr::arrange(installed, .data$software, dplyr::desc(.data$major))
  if (verbose) print_colour_versions(installed)
  installed
}

#' @rdname augment_installed
#' @export
print_colour_versions = function(installed) {
  for (i in seq_len(nrow(installed))) {
    row = installed[i, ]
    print_colour_version(row)
  }
  return(invisible(NULL))
}

print_colour_version = function(row) {
  software_name = glue::glue("{stringr::str_to_title(row$software)} v{row$major}")
  latest_version = glue::glue("latest v{row$version}")
  if (is.na(row$installed_version)) {
    cli::cli_alert_danger("{software_name}: {latest_version} not installed")
    return(invisible(NULL))
  }

  if (isTRUE(row$upgrade)) {
    cli::cli_alert_danger("{software_name}: v{row$installed_version} installed, but {latest_version} available")
  } else {
    cli::cli_alert_success("{software_name}: Latest version installed v{row$installed_version}")
  }
  return(invisible(NULL))
}

# Returns X.Y from X.Y.Z
get_major = function(v) unlist(stringr::str_match(v, "^([0-9]*\\.[0-9]*)")[, 2])
# Returns Z - use numeric for comparisons
get_patch = function(v) as.numeric(unlist(stringr::str_match(v, "\\.([0-9]*)$")[, 2]))

# Checks if versions are in the DB
in_db = function(installed) {
  # latest/earliest versions stored in DB
  software_range = get_latest_versions()  %>%
    dplyr::group_by(.data$software) %>%
    dplyr::summarise(latest = max(.data$major), earliest = min(.data$major))

  # Check installed packages are in software range
  installed %>%
    dplyr::left_join(software_range, by = c("software" = "software")) %>%
    dplyr::mutate(to_old = .data$installed_major < .data$earliest,
                  to_new = .data$installed_major > .data$latest) %>%
    dplyr::select(-"latest", -"earliest")
}

add_upgrade_column = function(installed) {
  versions = versions_to_display(installed)
  versions = versions %>%
    dplyr::mutate(upgrade = .data$patch > .data$installed_patch | .data$to_old)  %>%
    dplyr::mutate(upgrade = is.na(.data$upgrade) | .data$upgrade) %>%
    dplyr::mutate(upgrade =
                    dplyr::if_else(!is.na(.data$to_new) & .data$to_new, FALSE, .data$upgrade))
  dplyr::select(versions, -"to_old", -"to_new")
}

versions_to_display = function(installed) {
  latest = get_latest_versions()

  min_installed = latest %>%
    dplyr::full_join(installed, by = c("software" = "software",
                                       "major" = "installed_major")) %>%
    dplyr::group_by(.data$software, .drop = FALSE) %>%
    dplyr::filter(!is.na(.data$installed_version)) %>%
    dplyr::summarise(installed_version_num = max(.data$version_num, 3, na.rm = TRUE))

  l = get_latest_versions() %>%
    dplyr::full_join(installed, by = c("software" = "software",
                                       "major" = "installed_major")) %>%
    dplyr::full_join(min_installed, by = c("software" = "software")) %>%
    dplyr::group_by(.data$software) %>%
    dplyr::filter(.data$version_num <= .data$installed_version_num | is.na(.data$version_num))
  dplyr::select(l, -"version_num", -"installed_version_num")
}

get_latest_versions = function() {
  versions_fname = system.file("extdata", "versions", "software.csv",
                               package = "audit.base",
                               mustWork = TRUE)
  versions = readr::read_csv(versions_fname, comment = "#", col_types = "fc")
  versions$major = get_major(versions$version)
  versions$patch = get_patch(versions$version)
  # Add a version number.
  # Latest version is always 1, oldest increases with new version
  versions = versions %>%
    dplyr::group_by(.data$software) %>%
    dplyr::mutate(version_num = length(.data$major) - seq_along(.data$major) + 1L)
  versions
}
