test_that("%||% is imported from rlang, so it works before R 4.4.0", {
  imports = parent.env(asNamespace("audit.base"))
  expect_true(exists("%||%", envir = imports, inherits = FALSE))
})
