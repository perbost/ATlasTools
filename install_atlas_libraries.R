# Install Atlas R libraries from the public AtlasTools Git repository.
#
# GitHub example:
# source("https://raw.githubusercontent.com/perbost/ATlasTools/main/install_atlas_libraries.R")
# install_atlas_libraries()
#
# Forgejo example:
# source("https://git.perbost.org/regis/AtlasTools/raw/branch/main/install_atlas_libraries.R")
# install_atlas_libraries(repo_url = "https://git.perbost.org/regis/AtlasTools.git")

install_atlas_libraries <- function(repo_url = "https://github.com/perbost/ATlasTools.git",
                                    ref = "HEAD",
                                    subdir_prefix = ".",
                                    dependencies = NA,
                                    upgrade = "never",
                                    force = FALSE,
                                    quiet = FALSE) {
  if (!is.character(repo_url) || length(repo_url) != 1 || !nzchar(repo_url)) {
    stop("`repo_url` must be one public Git URL, for example 'https://github.com/perbost/ATlasTools.git'.")
  }

  repos <- getOption("repos")
  if (is.null(repos) || is.na(repos[["CRAN"]]) || identical(unname(repos[["CRAN"]]), "@CRAN@")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    utils::install.packages("BiocManager")
  }

  bioc_repos <- BiocManager::repositories()

  if (!requireNamespace("remotes", quietly = TRUE)) {
    utils::install.packages("remotes", repos = bioc_repos)
  }

  package_subdir <- function(package_dir) {
    if (is.null(subdir_prefix) || !nzchar(subdir_prefix) || identical(subdir_prefix, ".")) {
      return(package_dir)
    }
    paste(subdir_prefix, package_dir, sep = "/")
  }

  install_subdir <- function(package_dir) {
    remotes::install_git(
      url = repo_url,
      ref = ref,
      subdir = package_subdir(package_dir),
      dependencies = dependencies,
      upgrade = upgrade,
      force = force,
      quiet = quiet,
      repos = bioc_repos,
      build = TRUE,
      build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes")
    )
  }

  install_subdir("atlasvisualization")
  install_subdir("atlastoolsV2")

  invisible(TRUE)
}
