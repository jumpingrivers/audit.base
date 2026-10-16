test_that("check_sys_deps() lists the packages whose system libraries aren't installed", {
  local_mocked_bindings(read_p3m = function(url) {
    expect_match(url, "distribution=ubuntu&release=24.04", fixed = TRUE)
    read_fixture_json("p3m-ubuntu-24.04.json")
  })
  ubuntu = c(
    "PRETTY_NAME=\"Ubuntu 24.04.3 LTS\"",
    "ID=ubuntu",
    "ID_LIKE=debian",
    "VERSION_ID=\"24.04\""
  )
  # apt list --installed, after its "Listing..." header
  apt = c(
    "libssl-dev/noble-updates,noble-security,now 3.0.13-0ubuntu3.6 amd64 [installed]",
    "libcurl4-openssl-dev/noble-updates,now 8.5.0-2ubuntu10.6 amd64 [installed]",
    "libxml2-dev/noble-updates,now 2.9.14+dfsg-1.3ubuntu3.5 amd64 [installed,automatic]"
  )
  installed = clean_libs(ubuntu, apt)
  expect_identical(installed, c("libcurl4-openssl-dev", "libssl-dev", "libxml2-dev"))

  missing = suppressMessages(check_sys_deps(ubuntu, installed))
  expected = tibble::tibble(
    pkg = c("sf", "fftw", "sf", "sf", "sf", "sf"),
    sys_libs = c(
      "gdal-bin",
      "libfftw3-dev",
      "libgdal-dev",
      "libgeos-dev",
      "libproj-dev",
      "libsqlite3-dev"
    )
  )
  expect_identical(missing, expected)

  for (sys_deps in list(missing, missing[0, ])) {
    q = get_quarto_sys_deps(list(sys_deps = sys_deps))
    expect_equal(colnames(q), c("sys_libs", "pkg", "n"))
  }
})
