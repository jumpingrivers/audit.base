#' Augments installed software columns
#' @param installed A tibble with columns software and installed_version
#' @param verbose Default TRUE. Out a {cli} summary
#' @export
augment_installed = function(installed, verbose = TRUE) {
  installed$installed_major = get_major(installed$installed_version)
  installed$installed_point = get_point(installed$installed_version)
  installed = add_upgrade_column(installed)
  if (verbose) print_colour_versions(installed)
  installed
}

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
get_major = function(v) as.character(unlist(stringr::str_match(v, "^([0-9]+\\.[0-9]*)")[, 2]))
# Returns Z - use numeric for comparisons
get_point = function(v) as.numeric(unlist(stringr::str_match(v, "\\.([0-9]*)$")[, 2]))

add_upgrade_column = function(installed) {
  latest = get_latest_versions()
  latest = truncate_versions(latest, installed)

  dplyr::left_join(latest, installed,
                   by = c("software" = "software", "major" = "installed_major")) |>
    dplyr::mutate(upgrade = .data$point > .data$installed_point) |>
    dplyr::mutate(upgrade = is.na(.data$upgrade) | .data$upgrade)
}

get_latest_versions = function() {
  versions = system.file("extdata", "versions", "versions.csv",
                         package = "uatBase",
                         mustWork = TRUE)
  versions = dplyr::as_tibble(utils::read.csv(versions))
  versions$major = get_major(versions$version)
  versions$point = get_point(versions$version)
  return(versions)
}

# Determine the earliest version installed
# Then discard all previous version from latest
truncate_versions = function(latest, installed) {
  min_installed = installed |>
    dplyr::group_by(.data$software) |>
    dplyr::summarise("min_major" = min(.data$installed_major))

  truncated_latest = latest |>
    dplyr::left_join(min_installed, by = c("software" = "software")) |>
    dplyr::filter(.data$major >= .data$min_major | is.na(.data$min_major))

  dplyr::select(truncated_latest, -"min_major")
}

colour_version = function(upgrade, installed) {
  if (is.na(upgrade) || isTRUE(upgrade)) {
    cli::col_red(installed)
  } else {
    cli::col_green(installed)
  }
}
