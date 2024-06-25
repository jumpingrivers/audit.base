#' Posit Versions
#'
#' Returns a tibble containing the Posit version, date of release,
#' and any associated CVEs
#' @param type Posit product of interest
#' @param remote Try scrapping NEWS first, but fall back to local if that fails
#' @export
#' @examples
#' get_posit_versions(type = "connect")
#'
get_posit_versions = function(
    type = c("connect", "workbench", "drivers"),
    remote = TRUE) {
  type = match.arg(type)
  versions = if (isTRUE(remote)) {
    try(get_posit_remote_versions(type), silent = TRUE)
  } else {
    NULL
  }
  if ("try-error" %in% class(versions) || is.null(versions)) {
    cli::cli_alert_warning("Unable to scrape NEWS page for {type}. Falling back to cache")
    fname = system.file("extdata", "versions", paste0(type, ".csv"),
      mustWork = TRUE, package = "audit.base"
    )
    versions = readr::read_csv(fname, comment = "#", col_types = c("c", "c"))
  }
  versions = dplyr::arrange(versions, dplyr::desc(.data$version))
  return(versions)
}

#' Audit Posit Server
#'
#' Used for side effect, i.e. printing to console
#' @inheritParams get_posit_versions
#' @param posit_version Current Posit version
#' @export
#' @examples
#' audit_posit_version("2023.03.0", type = "connect")
audit_posit_version = function(posit_version, type = c("connect", "workbench", "drivers")) {
  type = match.arg(type)
  versions = get_posit_versions(type = type)
  row_number = lookup_version(posit_version, type = type)

  if (is.na(row_number)) {
    cli::cli_alert_info("The version {posit_version}, of Posit {type} isn't in the database")
  } else if (row_number > 1L) {
    newer_versions = versions[seq_len(row_number - 1), ]
    no_of_versions = length(unique(newer_versions$version)) # nolint
    no_of_cves = sum(!is.na(newer_versions$cve)) # nolint
    cli::cli_alert_info("Posit {type} is {cli::col_red('out of date')}")
    cli::cli_alert_info("There are {cli::col_red(no_of_versions)} newer versions that fix \\
                      {cli::col_red(no_of_cves)} CVEs")
  } else {
    cli::cli_alert_info("Posit {type} is up to date")
  }
  return(invisible(NULL))
}



lookup_version = function(posit_version, type) {
  versions = get_posit_versions(type = type)
  version_as_date = version_to_date(posit_version)
  all_dates = unlist(purrr::map(versions$version, version_to_date))

  if (is.na(version_as_date) || version_as_date < min(all_dates)) {
    # Older than DB
    row_number = NA_integer_
  } else if (version_as_date > max(all_dates)) {
    # Newer than DB
    row_number = 1L
  } else {
    # Return matching version; multiple CVEs, so just pick 1
    # Rtns an NA when not in the DB
    # Some versions add on additional info, .e.g. 2023.06.0.pro1 - delete
    posit_version = stringr::str_extract(posit_version, "^(202[0-9])\\.([01][0-9])\\.[0-9]")
    row_number = which(versions$version == posit_version)[1]
  }
  return(row_number)
}

version_to_date = function(version) {
  # Old style version
  if (!is_new_version(version)) {
    return(NA)
  }
  as_date = stringr::str_match_all(version, "^(202[0-9])\\.([01][0-9])")[[1]]
  as.Date(paste(as_date[1, 2], as_date[1, 3], "01", sep = "-"))
}

# Old style versions are 1.9.0 (2021-07)
is_new_version = function(version) {
  stringr::str_detect(version, pattern = "^202[0-9]\\.[01][0-9].[0-9]")
}
