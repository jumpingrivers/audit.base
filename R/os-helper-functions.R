is_ubuntu = function(os_release_df) {
  os_name = os_release_df[os_release_df$name == "name", ]$value
  stringr::str_detect(os_name, pattern = "ubuntu")
}

is_redhat = function(os_release_df) {
  os_name = os_release_df[os_release_df$name == "name", ]$value
  stringr::str_detect(os_name, pattern = "red hat")
}

get_os_release = function() {
  os_release = readr::read_lines("/etc/os-release")
  os_release
}

os_release_to_df = function(os_release) {
  os_release = os_release[nchar(os_release) > 0]
  os_release_df = stringr::str_match(os_release, "(.*)=(.*)")

  colnames(os_release_df) = c("full", "name", "value")
  os_release_df = tibble::as_tibble(os_release_df)
  os_release_df$name = stringr::str_to_lower(os_release_df$name)
  os_release_df$value = stringr::str_to_lower(os_release_df$value)
  os_release_df$value = stringr::str_remove_all(os_release_df$value, "\"")
  os_release_df
}
