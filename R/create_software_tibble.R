#' Helper function for updating R/Py/Quarto versions
#'
#' Currently this function would only be called by the package author, as it
#' updates inst/extdata/versions/software.csv
#' @export
update_software_csv = function() {
  software = create_software_tibble()
  readr::write_csv(software, file = "inst/extdata/versions/software.csv")
  return(invisible(software))
}

create_software_tibble = function() {
  r = get_latest_versions_from_posit("r")
  py = get_latest_versions_from_posit("python")
  q = jsonlite::read_json("https://api.github.com/repos/quarto-dev/quarto-cli/releases/latest")
  quarto = c("1.0.38", "1.1.189", "1.2.475", stringr::str_remove(q$name, "^v"))

  tibble::tibble(software = rep(c("r", "python", "quarto"),
                                c(length(r), length(py), length(quarto))),
                 version = c(r, py, quarto))
}

get_latest_versions_from_posit = function(type = c("r", "python")) {
  if (!requireNamespace("jsonlite")) {
    cli::cli_alert_info("Missing package - jsonlite")
    return(invisible(NULL))
  }
  type = match.arg(type)
  url = glue::glue("https://cdn.posit.co/{type}/versions.json")
  versions = unlist(jsonlite::read_json(url))
  versions = unname(versions)
  tibble::tibble(patch = get_patch(versions), major = get_major(versions), versions = versions) |>
    dplyr::filter(!is.na(patch)) |>
    dplyr::arrange(major, -patch) |>
    dplyr::group_by(major) |>
    dplyr::mutate(patch = max(patch))  |>
    dplyr::slice(1) |>
    dplyr::pull(versions)
}
