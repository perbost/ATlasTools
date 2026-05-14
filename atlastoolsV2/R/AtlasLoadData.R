.AtlasLoadData <- function(name, envir = parent.frame()) {
  if (requireNamespace("atlastoolsv2", quietly = TRUE)) {
    utils::data(list = name, package = "atlastoolsv2", envir = envir)
    if (exists(name, envir = envir, inherits = FALSE)) {
      return(invisible(get(name, envir = envir, inherits = FALSE)))
    }
  }

  package_path <- getOption("atlastoolsv2.package_path", NULL)
  local_paths <- c(
    if (!is.null(package_path)) file.path(package_path, "data", paste0(name, ".rda")),
    file.path("Library", "atlastools", "data", paste0(name, ".rda")),
    file.path("..", "Library", "atlastools", "data", paste0(name, ".rda")),
    file.path("atlastools", "data", paste0(name, ".rda")),
    file.path("..", "atlastools", "data", paste0(name, ".rda"))
  )
  local_paths <- local_paths[file.exists(local_paths)]

  if (length(local_paths) > 0) {
    load(local_paths[[1]], envir = envir)
    return(invisible(get(name, envir = envir, inherits = FALSE)))
  }

  stop(sprintf("Unable to load atlastoolsv2 data object '%s'.", name), call. = FALSE)
}

.AtlasConfigureParallelCluster <- function(cluster,
                                           atlastools.path = getOption("atlastoolsv2.package_path", NULL),
                                           atlasvisualization.path = getOption("AtlasVisualization.package_path", NULL)) {
  if (is.null(atlastools.path) || is.null(atlasvisualization.path)) {
    return(invisible(cluster))
  }
  if (!dir.exists(atlastools.path) || !dir.exists(atlasvisualization.path)) {
    return(invisible(cluster))
  }

  atlastools.path <- normalizePath(atlastools.path, mustWork = TRUE)
  atlasvisualization.path <- normalizePath(atlasvisualization.path, mustWork = TRUE)

  parallel::clusterCall(
    cluster,
    function(atlastools_path, atlasvisualization_path) {
      source_package_r_files <- function(package_path) {
        r_path <- file.path(package_path, "R")
        r_files <- list.files(
          r_path,
          pattern = "\\.R$",
          full.names = TRUE,
          ignore.case = TRUE
        )

        invisible(lapply(sort(r_files), source, local = .GlobalEnv))
      }

      options(atlastoolsv2.package_path = atlastools_path)
      options(AtlasVisualization.package_path = atlasvisualization_path)

      source_package_r_files(atlasvisualization_path)
      source_package_r_files(atlastools_path)
      invisible(TRUE)
    },
    atlastools.path,
    atlasvisualization.path
  )

  invisible(cluster)
}
