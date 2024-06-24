extract_cves = function(url) {
  page = rvest::read_html(url)
  sections = rvest::html_elements(page, "section")
  v_tibbles = purrr::map_df(sections, extract_components)
  all_v = get_all_versions(page) |>
    dplyr::filter(!.data$name %in% v_tibbles$name) %>%
    dplyr::bind_rows(v_tibbles) %>%
    dplyr::arrange(dplyr::desc(name))
  all_v
}

extract_components = function(section) {
  posit_name = rvest::html_attrs(section)
  posit_name = as.vector(posit_name["id"])
  posit_id = stringr::str_extract(posit_name, "[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}")

  li = section %>%
    rvest::html_elements("li") %>%
    rvest::html_text2()

  cves = stringr::str_extract(li, "^CVE-[0-9]{4}-[0-9]*")
  cves = cves[!is.na(cves)]

  if (length(cves) == 0L || is.na(posit_id)) {
    NULL
  } else {
    tibble::tibble(name = posit_id, cve = cves)
  }
 }

get_all_versions = function(page) {
  versions = page %>%
    rvest::html_nodes('h2') %>%
      rvest::html_text()
  v = stringr::str_extract(versions, "202[0-9]\\.[0-9]{2}\\.[0-9]{1,2}")
  tibble::tibble(name = v[!is.na(v)], cve = "")
}

update_posit_versions = function() {
  readr::write_csv(extract_cves("https://docs.posit.co/connect/news/"), 
    file = "inst/extdata/versions/connect.csv")

  readr::write_csv(extract_cves("https://docs.posit.co/ide/news/"), 
    file = "inst/extdata/versions/workbench.csv")
}