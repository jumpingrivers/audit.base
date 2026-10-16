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
  remote = TRUE
) {
  type = match.arg(type)
  versions = NULL
  if (isTRUE(remote) && type != "drivers") {
    versions = tryCatch(
      get_posit_remote_versions(type),
      error = function(e) {
        cli::cli_alert_warning(
          "Unable to scrape NEWS page for {type}: {conditionMessage(e)}. Falling back to cache"
        )
        NULL
      }
    )
  }
  if (is.null(versions)) {
    fname = system.file(
      "extdata",
      "versions",
      paste0(type, ".csv"),
      mustWork = TRUE,
      package = "audit.base"
    )
    # Can't figure out how to do character missing values in read_csv when entire column is missing
    versions = utils::read.csv(fname, header = TRUE, comment.char = "#", colClasses = "character")
    tibble::as_tibble(versions)
  }
  # Newest first, compared as numbers so 2024.12.10 sorts above 2024.12.2
  versions = versions[order(numeric_version(versions$version), decreasing = TRUE), ]
  # Versions without a CVE have an empty string; make these NA so they aren't counted
  versions$cve = dplyr::na_if(trimws(as.character(versions$cve)), "")
  versions
}

# Number of distinct CVEs; the release notes can list the same CVE more than once
count_cves = function(versions) {
  dplyr::n_distinct(versions$cve, na.rm = TRUE)
}

#' Audit Posit Server
#'
#' Used for side effect, i.e. printing to console
#' @inheritParams get_posit_versions
#' @param posit_version Current Posit version
#' @export
#' @examples
#' audit_posit_version("2023.03.0", type = "connect")
audit_posit_version = function(
  posit_version,
  type = c("connect", "workbench", "drivers")
) {
  type = match.arg(type)
  if (is_missing_version(posit_version)) {
    cli::cli_alert_info("Couldn't find which version of Posit {type} is installed")
    return(invisible(NULL))
  }
  versions = get_posit_versions(type = type)
  row_number = lookup_version(posit_version, versions)

  if (is.na(row_number)) {
    cli::cli_alert_info(
      "The version {posit_version}, of Posit {type} isn't in the database"
    )
  } else if (row_number > 1L) {
    newer_versions = versions[seq_len(row_number - 1), ]
    no_of_versions = length(unique(newer_versions$version)) # nolint
    no_of_cves = count_cves(newer_versions) # nolint
    cli::cli_alert_info("Posit {type} is {cli::col_red('out of date')}.")
    cli::cli_alert_info(
      "There are {cli::col_red(no_of_versions)} newer versions that fix \\
                      {cli::col_red(no_of_cves)} CVEs."
    )
  } else {
    cli::cli_alert_info("Posit {type} is up to date")
  }
  invisible(NULL)
}

# Row of `versions` (sorted newest first) that `posit_version` sits at:
# 1 when it's at or above the newest entry, otherwise one more than the number of
# strictly newer versions. NA when it can't be parsed or predates the list.
lookup_version = function(posit_version, versions) {
  version = parse_posit_version(posit_version)
  all_versions = numeric_version(versions$version)
  if (is.na(version) || version < min(all_versions)) {
    return(NA_integer_)
  }
  sum(all_versions > version) + 1L
}

# Year.month.patch, ignoring suffixes such as .pro1; NA for old style versions (1.9.0)
parse_posit_version = function(version) {
  if (is_missing_version(version)) {
    return(NA)
  }
  version = stringr::str_extract(version, "^20[0-9]{2}\\.[01][0-9]\\.[0-9]+")
  if (is.na(version)) NA else numeric_version(version)
}

# NA or character(0) when the version couldn't be found
is_missing_version = function(version) {
  length(version) != 1L || is.na(version)
}
