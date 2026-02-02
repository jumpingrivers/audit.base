#' Augments installed software columns
#' @param installed A tibble with columns software and installed_version
#' @param remote Use remote server for obtain versions
#' @param verbose Default TRUE.
#' @export
augment_installed = function(installed, remote = FALSE, verbose = TRUE) {
  installed$installed_major = get_major(installed$installed_version)
  installed$installed_patch = get_patch(installed$installed_version)
  installed = in_db(installed, remote = remote)
  installed = add_upgrade_column(installed, remote = remote)
  installed$major = package_version(installed$major)
  installed = dplyr::arrange(
    installed,
    .data$software,
    dplyr::desc(.data$major)
  )
  if (verbose) {
    print_colour_versions(installed)
  }
  installed
}

#' @rdname augment_installed
#' @export
print_colour_versions = function(installed) {
  # Selected installed and max version
  installed = installed %>%
    dplyr::group_by(.data$software) %>%
    dplyr::filter(!is.na(.data$installed_version) | .data$major == max(.data$major))

  for (i in seq_len(nrow(installed))) {
    row = installed[i, ]
    print_colour_version(row)
  }
  invisible(NULL)
}

print_colour_version = function(row) {
  if (is.na(row$installed_version) && isFALSE(row$within_eol)) {
    return(NULL)
  }
  major = if ("major" %in% colnames(row)) paste0(" v", row$major) else ""
  software_name = glue::glue("{stringr::str_to_title(row$software)}{major}") # nolint
  latest_version = glue::glue("v{row$version}") # nolint
  if (is.na(row$installed_version)) {
    cli::cli_alert_danger(
      "{software_name}: latest {latest_version} not installed"
    )
    return(invisible(NULL))
  }

  if (isFALSE(row$within_eol)) {
    cli::cli_alert_danger(
      "{software_name}: v{row$installed_version} installed, \\
                          but EOL has past and should be removed."
    )
  } else if (isTRUE(row$upgrade)) {
    cli::cli_alert_danger(
      "{software_name}: v{row$installed_version} installed, \\
                          but {latest_version} available"
    )
  } else {
    cli::cli_alert_success(
      "{software_name}: Latest version installed"
    )
  }
  invisible(NULL)
}

# Returns X.Y from X.Y.Z
get_major = function(v) unlist(stringr::str_match(v, "^([0-9]*\\.[0-9]*)")[, 2])
# Returns Z - use numeric for comparisons
get_patch = function(v) {
  as.numeric(unlist(stringr::str_match(v, "\\.([0-9]*)$")[, 2]))
}

# Checks if versions are in the DB
in_db = function(installed, remote = FALSE) {
  # latest/earliest versions stored in DB
  software_range = get_latest_versions(remote = remote) %>%
    dplyr::group_by(.data$software) %>%
    dplyr::summarise(latest = max(.data$major), earliest = min(.data$major))

  # Check installed packages are in software range
  installed %>%
    dplyr::left_join(software_range, by = c("software" = "software")) %>%
    dplyr::mutate(
      to_old = .data$installed_major < .data$earliest,
      to_new = .data$installed_major > .data$latest
    ) %>%
    dplyr::select(-"latest", -"earliest")
}

add_upgrade_column = function(installed, remote = FALSE) {
  versions = versions_to_display(installed, remote = remote)
  versions = versions %>%
    dplyr::mutate(
      upgrade = .data$patch > .data$installed_patch | .data$to_old
    ) %>%
    dplyr::mutate(upgrade = is.na(.data$upgrade) | .data$upgrade) %>%
    dplyr::mutate(
      upgrade = dplyr::if_else(
        !is.na(.data$to_new) & .data$to_new,
        FALSE,
        .data$upgrade
      )
    )
  dplyr::select(versions, -"to_old", -"to_new")
}

versions_to_display = function(installed, remote = FALSE) {
  latest_versions = get_latest_versions(remote = remote)

  min_installed = latest_versions %>%
    dplyr::full_join(
      installed,
      by = c(
        "software" = "software",
        "major" = "installed_major"
      )
    ) %>%
    dplyr::group_by(.data$software, .drop = FALSE) %>%
    dplyr::filter(!is.na(.data$installed_version)) %>%
    dplyr::summarise(
      installed_version_num = ifelse(
        length(.data$version_num) == 0L | all(is.na(.data$version)),
        3,
        min(.data$version_num, na.rm = TRUE)
      )
    )

  installed_with_full = latest_versions %>%
    dplyr::full_join(
      installed,
      by = c(
        "software" = "software",
        "major" = "installed_major"
      )
    )

  all = installed_with_full %>%
    dplyr::full_join(min_installed, by = c("software" = "software")) %>%
    dplyr::group_by(.data$software) %>%
    dplyr::filter(
      .data$version_num >= .data$installed_version_num |
        is.na(.data$version_num)
    ) %>%
    dplyr::arrange(.data$software, .data$version)

  # Old versions don't appear - so just tidy up some NAs
  l = dplyr::mutate(
    all,
    within_eol = ifelse(is.na(.data$within_eol), FALSE, .data$within_eol),
    version = ifelse(is.na(.data$version), .data$installed_version, .data$version)
  )

  dplyr::select(l, -"version_num", -"installed_version_num")
}

get_latest_versions = function(remote = TRUE) {
  versions = if (isTRUE(remote)) {
    get_latest_versions_remote()
  } else {
    NULL
  }
  if ("try-error" %in% class(versions) || is.null(versions)) {
    cli::cli_alert_warning("Using cached version for software versions")
    versions_fname = system.file(
      "extdata",
      "versions",
      "software.csv",
      package = "audit.base",
      mustWork = TRUE
    )
    versions = readr::read_csv(versions_fname, comment = "#", col_types = "fc")
  }
  versions$major = get_major(versions$version)
  versions$patch = get_patch(versions$version)
  # Add a version number.
  # Latest version is always 1, oldest increases with new version
  versions = versions %>%
    dplyr::group_by(.data$software) %>%
    dplyr::mutate(
      version_num = length(.data$major) - seq_along(.data$major) + 1L,
      within_eol = FALSE
    )
  versions %>%
    add_python_eol() %>%
    add_quarto_eol() %>%
    add_r_eol()
}

add_python_eol = function(versions) {
  eol_fname = system.file(
    "extdata",
    "versions",
    "eol.csv",
    package = "audit.base",
    mustWork = TRUE
  )
  eol = readr::read_csv(eol_fname, col_types = "cD", skip = 1)
  eol = eol[eol$end_of_life > Sys.Date(), ]
  versions[versions$software == "python", ]$within_eol = FALSE
  versions[versions$software == "python" & versions$major %in% eol$version, ]$within_eol = TRUE
  versions
}

add_quarto_eol = function(versions) {
  versions[versions$software == "quarto", ]$within_eol = TRUE
  versions
}
add_r_eol = function(versions) {
  # Versions of R 3.0 past EOL
  versions[versions$software == "r", ]$within_eol = FALSE
  versions[versions$software == "r" & startsWith(versions$major, "4"), ]$within_eol = TRUE
  versions
}
