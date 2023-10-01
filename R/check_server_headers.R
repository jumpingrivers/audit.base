#' Check server headers
#'
#' In addition to the checks made by {serverHeaders} we also check
#' that Posit isn't in one of the headers.
#' @param server URL of server
#' @export
check_server_headers = function(server) {
  out = serverHeaders::check(server)
  posit_headers = get_posit_headers(out$headers)
  out$headers = dplyr::bind_rows(out$headers, posit_headers)
  out
}

# Detects if we are leaking server header information
get_posit_headers = function(headers) {
  posit_header = headers %>%
    dplyr::filter(.data$header == "server" &
                    stringr::str_detect(.data$message, "[p|P]osit")) %>%
    dplyr::mutate(documentation = "https://developer.mozilla.org/docs/Web/HTTP/Headers/Server",
                  primary_header = TRUE,
                  status = "WARN")
  if (nrow(posit_header) == 0) {
    cli::cli_alert_success("{cli::col_green('server')}: Does not leak information")
  } else {
    cli::cli_alert_danger("{cli::col_red('server')}: Contains too much information")
  }
  return(posit_header)
}
