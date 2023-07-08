test_that("Testing configuation", {
  tmp_dir = tempdir()
  file = "test-config.yml"

  pkgs = paste0("audit.", c("base", "workbench", "connect"))
  for (pkg in pkgs) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cc = create_config(file = "test-config.yml", pkg_name = pkg)
      expect_true(is.function(cc))
      con = cc(dir = tmp_dir, default = TRUE, type = "force")
      expect_true(is.list(con) && file.exists(file.path(tmp_dir, file)))
    }
  }

  if (requireNamespace(pkg, quietly = TRUE)) {
    # Check defaults
    expect_true(all(unlist(purrr::list_flatten(con))))
    con = cc(dir = tmp_dir, default = FALSE, type = "force")
    expect_true(all(!unlist(purrr::list_flatten(con))))

    # Config file already exists
    expect_error(cc(dir = tmp_dir, default = TRUE, type = "error"))

    # Test merge
    con = cc(dir = tmp_dir, default = TRUE, type = "merge")
    expect_true(!all(unlist(purrr::list_flatten(con))))
  }
})
