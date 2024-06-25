#' Helper functions for updating R/Py/Quarto/Posit versions
#'
#' Currently this function would only be called by the package author, as it
#' updates inst/extdata/versions/*.csv
#' @export
update_all_versions = function() {
  readr::write_csv(
    get_posit_remote_versions(type = "connect"),
    file = "inst/extdata/versions/connect.csv"
  )

  readr::write_csv(
    get_posit_remote_versions(type = "workbench"),
    file = "inst/extdata/versions/workbench.csv"
  )

  software = create_software_tibble()
  readr::write_csv(software, file = "inst/extdata/versions/software.csv")
  return(invisible(NULL))
}
