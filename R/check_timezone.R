#' Check Timezone Correctly Set
#'
#' Checks that the system timezone can be determined via `Sys.timezone()`.
#' @return Invisibly, `TRUE` if the timezone is set, `FALSE` otherwise.
#' @export
check_timezone = function() {
  cli::cli_h1("Checking timezone")
  # Sys.timezone() warns when timedatectl fails, but can still find the timezone
  tz = suppressWarnings(Sys.timezone())
  check = !is.na(tz)
  if (isTRUE(check)) {
    cli::cli_alert_success(paste0("Timezone set: ", tz))
  } else {
    cli::cli_alert_danger("Timezone not set")
  }
  invisible(check)
}
