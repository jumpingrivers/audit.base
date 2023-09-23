#' Create test config
#'
#' This function creates an example config. By default all tests are TRUE.
#' Some RSC servers intentionally won't have some features implemented.
#'
#' @param file Config file name
#' @inheritParams init_r6_check
#' @details If a test is missing from the config file, it is assume to be TRUE.
#' Therefore, the config file can be quite short and just list exceptions.
#' If the config file is missing, then all tests are carried out.
#'
#' When merging configs, we either
#' * merge the new with existing
#' * force: overwrite existing file
#' * error: if a config file exists, raise an error
#' @export
create_config = function(file, pkg_name) {
  # Return a function - allows unique config names
  function(dir = ".", default = TRUE, type = c("merge", "force", "error")) {
    type = match.arg(type)
    yaml_file = file.path(dir, file)
    if (type == "error" && file.exists(yaml_file)) {
      cli::cli_abort("Config file already exists.
                   Either delete or change the `type` argument.")
    }

    new_config = create_config_list(dir, file, default, pkg_name)
    if (type == "merge" && file.exists(yaml_file)) {
      existing_config = yaml::read_yaml(yaml_file)
      new_config = merge_configs(new_config, existing_config)
    }
    yaml::write_yaml(new_config, file = yaml_file)
    return(invisible(new_config))
  }
}
create_config_list = function(dir, file, default, pkg_name) {
  obj_info = get_check_info(dir, file, pkg_name)
  groups = unique(obj_info$group)
  shorts = purrr::map(groups, ~obj_info[obj_info$group == .x, ]$short)
  group_shorts = purrr::map(shorts, create_group_short, default = default)

  names(group_shorts) = groups
  group_shorts
}

get_check_info = function(dir, file, pkg_name) {
  r6_inits = init_r6_checks(dir, file, pkg_name)
  if (length(r6_inits) > 0L)
    purrr::map_dfr(r6_inits, function(r6) c("class" = class(r6)[1], r6$info()))
  else
    tibble::tibble(class = character(0), group = character(0),
                   short = character(0), context = character(0),
                   long = character(0))
}

merge_configs = function(new, existing) {
  xnames = names(existing)
  for (v in names(new)) {
    if (is.list(new[[v]]))  {
      new[[v]] = merge_configs(new[[v]], existing[[v]])
      # Ensure that existing list is the same "type"
      # If not, new gets precedent
    } else if (v %in% xnames &&
               !is.null(existing[[v]]) &&
               !is.list(existing[[v]])) {
      new[[v]] = existing[[v]]
    }
  }
  new
}

create_group_short = function(short, default) {
  group_short = vector("list", length = length(short))
  names(group_short) = short
  purrr::map(group_short, ~default)
}
