#' Base Check Class
#'
#' Inherited underlying base class.
#' Shouldn't be used directly
#' @docType class
#' @field group Field for grouping tests
#' @field short Use in config.yml
#' @field context Human readable string
#' @field long A long description of the test
#' @field account Account name
#' @export
base_check = R6::R6Class(
  "base_check",
  inherit = logger,
  public = list(
    context = NA,
    group = NA,
    short = NA,
    long = NA_character_,
    account = NA,
    #' @description Set parameters for config file location
    #' @param dir directory location of the the config file
    #' @param file config file name
    #' @param debug_level Value of 0, 1, or 2
    initialize = function(dir = ".", file = "config-uat.yml", debug_level = 0) {
      private$dir = dir
      private$file = file
      private$debug_level = debug_level
    },
    #' @description Summarise key class attributes
    info = function() {
      if (is.na(private$group)) {
        stop("Missing group")
      }
      if (is.na(private$context)) {
        stop("Missing context")
      }
      if (is.na(private$short)) {
        stop("Missing short")
      }
      if (is.na(private$long)) {
        stop("Missing long description")
      }

      c(
        "group" = private$group,
        "short" = private$short,
        "context" = private$context,
        "long" = private$long
      )
    }
  ),
  private = list(
    dir = "-",
    file = "-",
    debug_level = "-",
    # Assume TRUE, unless explicitly FALSE
    skip_test = function() {
      config_path = file.path(private$dir, private$file)
      if (!file.exists(config_path)) {
        return(FALSE)
      }
      config = yaml::read_yaml(config_path)

      if (!(private$group %in% names(config))) {
        return(FALSE)
      }
      group = config[[private$group]]
      return(isFALSE(group[[private$short]]))
    },
    checker = function(check) {
      self$start_logger()
      if (private$skip_test()) {
        has_passed = NA
      } else {
        res = try(check, silent = TRUE)
        has_passed = !inherits(res, "try-error") && isTRUE(res)
      }
      self$stop_logger(has_passed)
      return(invisible(self))
    }
  )
)
