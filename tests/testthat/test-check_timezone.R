test_that("check_timezone() passes when the timezone is set", {
  old_tz = Sys.getenv("TZ", unset = NA)
  on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz))
  Sys.setenv(TZ = "Europe/London")
  expect_true(suppressMessages(check_timezone()))
})
