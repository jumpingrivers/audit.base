# Offline stand-ins for the remote lookups, read from tests/testthat/fixtures
read_fixture_json = function(name) {
  jsonlite::read_json(testthat::test_path("fixtures", name))
}

# The NEWS page fixture serves both Connect and Workbench
local_posit_news = function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    read_posit_news = function(url) {
      rvest::read_html(testthat::test_path("fixtures", "posit-news.html"))
    },
    .env = env
  )
}

local_version_json = function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    fetch_json = function(url) {
      if (grepl("quarto-cli", url, fixed = TRUE)) {
        read_fixture_json("quarto-releases.json")
      } else if (grepl("cdn.posit.co/r/", url, fixed = TRUE)) {
        read_fixture_json("r-versions.json")
      } else if (grepl("download-metadata.json", url, fixed = TRUE)) {
        read_fixture_json("uv-metadata.json")
      } else {
        stop("No fixture for ", url)
      }
    },
    .env = env
  )
}
