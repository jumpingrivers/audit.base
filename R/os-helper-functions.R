# Posit Package Manager's distribution and release, from os-release ID, then ID_LIKE.
# Rocky Linux and AlmaLinux list rhel in ID_LIKE, so they use the Red Hat requirements.
get_p3m_system = function(os_release_df) {
  p3m_names = c(
    ubuntu = "ubuntu",
    debian = "debian",
    centos = "centos",
    rhel = "redhat",
    "opensuse-leap" = "opensuse",
    sles = "sle"
  )
  ids = c(
    os_release_value(os_release_df, "id"),
    stringr::str_split(os_release_value(os_release_df, "id_like"), " ")[[1]]
  )
  distribution = unname(p3m_names[ids[ids %in% names(p3m_names)]])[1L]
  release = os_release_value(os_release_df, "version_id")
  # P3M uses the major version for these: 9, not 9.4
  if (distribution %in% c("debian", "redhat", "centos")) {
    release = stringr::str_remove(release, "\\..*")
  }
  list(distribution = distribution, release = release)
}

# apt or yum, which list installed packages in different formats; NA for any other OS
get_package_tool = function(os_release_df) {
  distribution = get_p3m_system(os_release_df)$distribution
  if (distribution %in% c("ubuntu", "debian")) {
    "apt"
  } else if (distribution %in% c("redhat", "centos")) {
    "yum"
  } else {
    NA_character_
  }
}

os_release_value = function(os_release_df, name) {
  value = os_release_df$value[os_release_df$name == name]
  if (length(value) == 0L) "" else value[1L]
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
