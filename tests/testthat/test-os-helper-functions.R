test_that("OS tests", {
  # Avoid errors on CI Windows testing
  if (Sys.info()["sysname"] != "Linux") {
    testthat::skip()
  }
  libs = get_installed_libs()
  expect_gt(length(libs), 1000)
})

test_that("get_p3m_system() maps os-release to P3M's distribution and release", {
  os_releases = list(
    ubuntu = c("ID=ubuntu", "ID_LIKE=debian", "VERSION_ID=\"26.04\""),
    debian = c("ID=debian", "VERSION_ID=\"12\""),
    rhel = c("ID=\"rhel\"", "ID_LIKE=\"fedora\"", "VERSION_ID=\"10.0\""),
    centos = c("ID=\"centos\"", "ID_LIKE=\"rhel fedora\"", "VERSION_ID=\"7\""),
    rocky = c("ID=\"rocky\"", "ID_LIKE=\"rhel centos fedora\"", "VERSION_ID=\"9.4\""),
    alma = c("ID=\"almalinux\"", "ID_LIKE=\"rhel centos fedora\"", "VERSION_ID=\"8.10\""),
    suse = c("ID=\"opensuse-leap\"", "ID_LIKE=\"suse opensuse\"", "VERSION_ID=\"15.6\""),
    arch = c("ID=arch", "BUILD_ID=rolling")
  )
  expected = list(
    ubuntu = c("ubuntu", "26.04"),
    debian = c("debian", "12"),
    rhel = c("redhat", "10"),
    centos = c("centos", "7"),
    rocky = c("redhat", "9"),
    alma = c("redhat", "8"),
    suse = c("opensuse", "15.6"),
    arch = c(NA, "")
  )
  for (os in names(os_releases)) {
    system = get_p3m_system(os_release_to_df(os_releases[[os]]))
    expect_identical(c(system$distribution, system$release), expected[[os]], label = os)
  }
})

test_that("Unsupported systems warn and give NA instead of stopping", {
  arch = c("PRETTY_NAME=\"Arch Linux\"", "ID=arch")
  expect_warning(check_sys_deps(arch, "zlib"), "arch linux")
  expect_identical(suppressWarnings(check_sys_deps(arch, "zlib")), NA)
  expect_warning(clean_libs(arch, "zlib"), "arch linux")
  expect_identical(suppressWarnings(clean_libs(arch, "zlib")), NA_character_)

  out = get_quarto_sys_deps(list(sys_deps = NA))
  expect_equal(colnames(out), c("sys_libs", "pkg", "n"))
})

test_that("clean_libs() keeps the dots in RHEL package names", {
  rhel = c("ID=\"rhel\"", "ID_LIKE=\"fedora\"", "VERSION_ID=\"8.10\"")
  yum = c(
    "Installed Packages",
    "java-1.8.0-openjdk-devel.x86_64    1:1.8.0.412    @appstream",
    "libgit2_1.7-devel.x86_64    1.7.2    @epel",
    "gdal3.4-devel.x86_64    3.4.3-1    @epel",
    "a-very-long-package-name-that-yum-wraps.noarch",
    "                                   1.0-1          @appstream"
  )
  expected = c(
    "a-very-long-package-name-that-yum-wraps",
    "gdal3.4-devel",
    "java-1.8.0-openjdk-devel",
    "libgit2_1.7-devel"
  )
  expect_identical(clean_libs(rhel, yum), expected)
  # rpm -qa --qf "%{NAME}\\n" gives the names as they are
  expect_identical(clean_libs(rhel, expected), expected)
})
