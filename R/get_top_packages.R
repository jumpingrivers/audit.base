get_top_packages = function(distribution = "ubuntu",
                            release = "22.04") {
  reqs = get_pkg_requirements(distribution = distribution, release = release)
  pkgs = unique(reqs$pkg)
  downloads = cranlogs::cran_downloads(packages = pkgs, "last-month")
  downloads |>
    dplyr::group_by(package) |>
    dplyr::summarise(count = sum(count)) |>
    dplyr::arrange(-count)
}

downloads
top = get_top_packages()
top1 = top$package[top$count > 10000]

x = readRDS("~/auditworkbench.rds")
miss = x$sys_deps$pkg
miss[miss %in% top1]

nrow(downloads)
plot(downloads$count, log = "y")
cranlogs::cran_top_downloads("last-month", count = 100) |>
  tail()
