# Atlas R Libraries

This directory contains two R packages authored by Regis Perbost:

- `atlastoolsV2/` contains the R package `atlastoolsv2`: Atlas multi-omic
  data analysis helpers.
- `atlasvisualization/` contains the R package `AtlasVisualization`:
  visualization helpers used by Atlas analysis workflows.

## Install From A Public Git Repository

Publish this repository publicly on GitHub, Forgejo, or another Git server.
Public users should install `AtlasVisualization` first, then `atlastoolsv2`.
See `PUBLISHING.md` if you are deciding whether to publish the whole parent
repository or publish `Library/` as its own repository root.

### One Helper Function

After publishing, replace the URL below with your public repository URL:

```r
source("https://raw.githubusercontent.com/OWNER/REPOSITORY/main/Library/install_atlas_libraries.R")
install_atlas_libraries("https://github.com/OWNER/REPOSITORY.git")

library(AtlasVisualization)
library(atlastoolsv2)
```

If you publish the contents of `Library/` as the repository root, use:

```r
source("https://raw.githubusercontent.com/OWNER/REPOSITORY/main/install_atlas_libraries.R")
install_atlas_libraries(
  "https://github.com/OWNER/REPOSITORY.git",
  subdir_prefix = "."
)

library(AtlasVisualization)
library(atlastoolsv2)
```

For Forgejo or Gitea:

```r
source("https://forgejo.example.org/OWNER/REPOSITORY/raw/branch/main/Library/install_atlas_libraries.R")
install_atlas_libraries("https://forgejo.example.org/OWNER/REPOSITORY.git")

library(AtlasVisualization)
library(atlastoolsv2)
```

### Direct Install Commands

GitHub:

```r
install.packages(c("BiocManager", "remotes"))
repos <- BiocManager::repositories()

remotes::install_github(
  "OWNER/REPOSITORY",
  subdir = "Library/atlasvisualization",
  dependencies = NA,
  upgrade = "never",
  repos = repos
)

remotes::install_github(
  "OWNER/REPOSITORY",
  subdir = "Library/atlastoolsV2",
  dependencies = NA,
  upgrade = "never",
  repos = repos
)
```

Forgejo/Gitea:

```r
install.packages(c("BiocManager", "remotes"))
repos <- BiocManager::repositories()
git_url <- "https://forgejo.example.org/OWNER/REPOSITORY.git"

remotes::install_git(
  git_url,
  subdir = "Library/atlasvisualization",
  dependencies = NA,
  upgrade = "never",
  repos = repos
)

remotes::install_git(
  git_url,
  subdir = "Library/atlastoolsV2",
  dependencies = NA,
  upgrade = "never",
  repos = repos
)
```

If `Library/` itself is the repository root, use `subdir = "atlasvisualization"`
and `subdir = "atlastoolsV2"` instead.

The `BiocManager::repositories()` line is important because these packages use
Bioconductor dependencies such as `MOFA2`, `fgsea`, `gage`, and
`ConsensusClusterPlus`.

## License

The original code and documentation in these libraries are licensed under the
GNU General Public License, version 2 or later (`GPL-2.0-or-later`; R
`DESCRIPTION` form: `GPL (>= 2)`).

The GPL-2.0 and GPL-3.0 license texts are provided in `licenses/`. Third-party
R packages are not vendored here; they remain under their own licenses.

## Copyright And Attribution

Copyright (C) 2026 Regis Perbost <regis.perbost@gmail.com>
Alternate contact: <regis@perbost.org>

When copying, modifying, or redistributing these libraries, keep the copyright,
license, and attribution notices naming the relevant library and Regis Perbost.

When using these libraries in an analysis, report, publication, service, or
derived package, cite or acknowledge the library name and author:

> Regis Perbost. `atlastoolsv2` and `AtlasVisualization`: Atlas R libraries.

R citation metadata is available in each package under `inst/CITATION`, and
repository-level citation metadata is available in `CITATION.cff`.

## Dependency License Inventory

The direct dependency and copyleft review is recorded in
`THIRD-PARTY-NOTICES.md`.
