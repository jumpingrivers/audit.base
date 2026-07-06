#' Check Timezone Correctly Set
#'
#' Checks that the system timezone can be determined via `Sys.timezone()`.
#' @return Invisibly, `TRUE` if the timezone is set, `FALSE` otherwise.
#' @export
check_timezone = function() {
  cli::cli_h1("Checking timezone")
  tz = tryCatch(
    Sys.timezone(),
    warning = function(w) {
      NA
    }
  )
  check = !is.na(tz)
  if (isTRUE(check)) {
    cli::cli_alert_success(paste0("Timezone set: ", tz))
  } else {
    cli::cli_alert_success(paste0("Timezone seems incorrect: ", tz))
  }
  invisible(check)
}
