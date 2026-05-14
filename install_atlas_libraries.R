# Install Atlas R libraries from a public Git repository.
#
# Example:
# source("https://raw.githubusercontent.com/OWNER/REPOSITORY/main/Library/install_atlas_libraries.R")
# install_atlas_libraries("https://github.com/OWNER/REPOSITORY.git")
#
# Forgejo/Gitea example:
# source("https://forgejo.example.org/OWNER/REPOSITORY/raw/branch/main/Library/install_atlas_libraries.R")
# install_atlas_libraries("https://forgejo.example.org/OWNER/REPOSITORY.git")

install_atlas_libraries <- function(repo_url,
                                    ref = "HEAD",
                                    subdir_prefix = "Library",
                                    dependencies = NA,
                                    upgrade = "never",
                                    force = FALSE,
                                    quiet = FALSE) {
  if (!is.character(repo_url) || length(repo_url) != 1 || !nzchar(repo_url)) {
    stop("`repo_url` must be one public Git URL, for example 'https://github.com/OWNER/REPOSITORY.git'.")
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
