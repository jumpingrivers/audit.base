fname = system.file("extdata", "versions", "software.csv",
                    package = "audit.base",
                    mustWork = TRUE)
software = readr::read_csv(fname, col_types = "cc", comment = "#")

lastest_r = get_latest_versions_from_posit("r")
expect_true(all(lastest_r %in% software$version))


lastest_py = get_latest_versions_from_posit("python")
expect_true(all(lastest_py %in% software$version))

py_versions_site = "https://cdn.posit.co/quarto/versions.json"
versions = unlist(jsonlite::read_json(py_versions_site))
versions = unname(versions)
x
library(dplyr)
get_latest_versions_from_posit = function(type = c("r", "python")) {
  type = match.arg(type)
  url = glue::glue("https://cdn.posit.co/{type}/versions.json")
  versions = unlist(jsonlite::read_json(url))
  versions = unname(versions)
  tibble::tibble(patch = get_patch(versions), major = get_major(versions), versions = versions) |>
    dplyr::filter(!is.na(patch)) |>
    dplyr::arrange(major, -patch) |>
    group_by(major) |>
    mutate(patch = max(patch))  |>
    slice(1) |>
    pull(versions)
}

url = "https://api.github.com/repos/quarto-dev/quarto-cli/releases/latest"
jsonlite::read_json(url)

library(testthat)

x = x[stringr::str_starts(x, "[1-9]")]
x = unname(x)
x
