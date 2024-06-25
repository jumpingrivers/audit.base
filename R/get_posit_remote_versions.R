get_posit_remote_versions = function(type = c("connect", "workbench")) {
  if (!requireNamespace("rvest")) {
    cli::cli_alert_danger("Missing package - rvest")
  }
  type = match.arg(type)
  url = if (type == "connect") {
    "https://docs.posit.co/connect/news/"
  } else {
    "https://docs.posit.co/ide/news/"
  }
  page = rvest::read_html(url)
  sections = rvest::html_elements(page, "section")
  v_tibbles = purrr::map_df(sections, extract_posit_cves)
  all_v = get_all_remote_versions(page) %>%
    dplyr::filter(!.data$version %in% v_tibbles$version) %>%
    dplyr::bind_rows(v_tibbles) %>%
    dplyr::arrange(dplyr::desc(.data$version))
  all_v
}

extract_posit_cves = function(section) {
  posit_name = rvest::html_attrs(section)
  posit_name = as.vector(posit_name["id"])
  posit_version = stringr::str_extract(posit_name, "[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}")

  li = section %>%
    rvest::html_elements("li") %>%
    rvest::html_text2()

  cves = stringr::str_extract(li, "^CVE-[0-9]{4}-[0-9]*")
  cves = cves[!is.na(cves)]

  if (length(cves) == 0L || is.na(posit_version)) {
    tibble::tibble(version = character(0), cve = character(0))
  } else {
    tibble::tibble(version = posit_version, cve = cves)
  }
}

get_all_remote_versions = function(page) {
  versions = page %>%
    rvest::html_nodes("h2") %>%
    rvest::html_text()
  v = stringr::str_extract(versions, "202[0-9]\\.[0-9]{2}\\.[0-9]{1,2}")
  tibble::tibble(version = v[!is.na(v)], cve = "")
}
