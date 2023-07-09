#' Augments installed software columns
#' @param installed A tibble with columns software and installed_version
#' @param verbose Default TRUE. Out a {cli} summary
#' @export
augment_installed = function(installed, verbose = TRUE) {
  installed$installed_major = get_major(installed$installed_version)
  installed$installed_patch = get_patch(installed$installed_version)
  installed = in_db(installed)
  installed = add_upgrade_column(installed)
  if (verbose) print_colour_versions(installed)
  installed
}

#' @rdname augment_installed
#' @export
print_colour_versions = function(installed) {
  for (i in seq_len(nrow(installed))) {
    row = installed[i, ]
    version = colour_version(row$upgrade, row$installed_version)
    software_name = glue::glue("{stringr::str_to_title(row$software)}") #nolint
    latest_version = glue::glue("latest (v{row$version})") #nolint
    cli::cli_alert_info("{software_name}: installed (v{version}) {latest_version}")
  }
  return(invisible(NULL))
}

# Returns X.Y from X.Y.Z
get_major = function(v) as.numeric(unlist(stringr::str_match(v, "^([0-9]*\\.[0-9]*)")[, 2]))
# Returns Z - use numeric for comparisons
get_patch = function(v) as.numeric(unlist(stringr::str_match(v, "\\.([0-9]*)$")[, 2]))

# Checks if versions are in the DB
in_db = function(installed) {
  # latest/earliest versions stored in DB
  software_range = get_latest_versions()  %>%
    dplyr::group_by(software) %>%
    dplyr::summarise(latest = max(major), earliest = min(major))

  # Check installed packages are in software range
  installed %>%
    dplyr::left_join(software_range, by = c("software" = "software")) %>%
    dplyr::mutate(to_old = installed_major < earliest,
                  to_new = installed_major > latest) %>%
    dplyr::select(-latest, -earliest)
}

add_upgrade_column = function(installed) {
  versions = versions_to_display(installed)
  versions = versions %>%
    dplyr::mutate(upgrade = .data$patch > .data$installed_patch | .data$to_old)  %>%
    dplyr::mutate(upgrade = is.na(.data$upgrade) | .data$upgrade) %>%
    dplyr::mutate(upgrade =
                    dplyr::if_else(!is.na(.data$to_new) & .data$to_new, FALSE, .data$upgrade))


  dplyr::select(versions, -to_old, -to_new)
}

versions_to_display = function(installed) {
  latest = get_latest_versions()

  min_installed = latest %>%
    dplyr::full_join(installed, by = c("software" = "software",
                                       "major" = "installed_major")) %>%
    dplyr::group_by(software, .drop = FALSE) %>%
    dplyr::filter(!is.na(.data$installed_version)) %>%
    dplyr::summarise(installed_version_num = max(version_num, 3, na.rm = TRUE))

  l = get_latest_versions() %>%
    dplyr::full_join(installed, by = c("software" = "software",
                                       "major" = "installed_major")) %>%
    dplyr::full_join(min_installed, by = c("software" = "software")) %>%
    dplyr::group_by(software) %>%
    dplyr::filter(version_num <= installed_version_num | is.na(version_num))  %>%
    dplyr::select(-version_num, -installed_version_num)
  l
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
    dplyr::group_by(software) %>%
    dplyr::mutate(version_num = length(major) - seq_along(major) + 1)
  versions
}
colour_version = function(upgrade, installed) {
  if (is.na(upgrade) || isTRUE(upgrade)) {
    cli::col_red(installed)
  } else {
    cli::col_green(installed)
  }
}
