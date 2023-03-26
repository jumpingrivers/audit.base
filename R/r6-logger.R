#' R6 logger class
#'
#' Not called directly, but is inherited by base_check.
#' @import R6
#' @export
logger = R6::R6Class(
  "logger",
  public = list(
    #' @description Starts the logger
    start_logger = function() {
      cli::cli_alert_info("Starting check: {private$group}({private$short}): {private$context}")

      private$start_time = Sys.time()
    },
    #' @description Stores the results of the check
    #' @param passed Logical, if skipped, the NA
    stop_logger = function(passed) {
      time_taken = Sys.time() - private$start_time
      private$log = dplyr::tibble(group = private$group,
                                  short = private$short,
                                  context = private$context,
                                  passed = passed,
                                  time_taken = round(time_taken, 2))

      msg_function(passed)

      return(invisible(self))
    },
    #' @description Retrieve the log
    #' @return Check log
    get_log = function() private$log
  ),
  private = list(
    start_time = NULL,
    context = NA,
    group = NA,
    log = NULL
  )
)

msg_function = function(has_passed, msg) {
  if (is.na(has_passed)) {
    cli::cli_alert_info("Check skipped")
  } else if (has_passed) {
    cli::cli_alert_success("Check finished")
  } else {
    cli::cli_alert_danger("Check finished")
  }
  return(invisible(NULL))
}
