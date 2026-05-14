# AtlasTools

This repository contains two R packages authored by Regis Perbost:

- `atlastoolsV2/` contains the R package `atlastoolsv2`: Atlas multi-omic
  data analysis helpers.
- `atlasvisualization/` contains the R package `AtlasVisualization`:
  visualization helpers used by Atlas analysis workflows.

## Install

Public users should install `AtlasVisualization` first, then `atlastoolsv2`.
The helper below does that in the right order and configures Bioconductor
repositories for dependencies such as `MOFA2`, `fgsea`, `gage`, and
`ConsensusClusterPlus`.

### Helper Function

From GitHub:

```r
source("https://raw.githubusercontent.com/perbost/ATlasTools/main/install_atlas_libraries.R")
install_atlas_libraries()

library(AtlasVisualization)
library(atlastoolsv2)
```

From Forgejo:

```r
source("https://git.perbost.org/regis/AtlasTools/raw/branch/main/install_atlas_libraries.R")
install_atlas_libraries(
  repo_url = "https://git.perbost.org/regis/AtlasTools.git"
)

library(AtlasVisualization)
library(atlastoolsv2)
```

### Direct Install Commands

GitHub:

```r
install.packages(c("BiocManager", "remotes"))
repos <- BiocManager::repositories()

remotes::install_github(
  "perbost/ATlasTools",
  subdir = "atlasvisualization",
  dependencies = NA,
  upgrade = "never",
  repos = repos
)

remotes::install_github(
  "perbost/ATlasTools",
  subdir = "atlastoolsV2",
  dependencies = NA,
  upgrade = "never",
  repos = repos
)

library(AtlasVisualization)
library(atlastoolsv2)
```

Forgejo:

```r
install.packages(c("BiocManager", "remotes"))
repos <- BiocManager::repositories()
git_url <- "https://git.perbost.org/regis/AtlasTools.git"

remotes::install_git(
  git_url,
  subdir = "atlasvisualization",
  dependencies = NA,
  upgrade = "never",
  repos = repos
)

remotes::install_git(
  git_url,
  subdir = "atlastoolsV2",
  dependencies = NA,
  upgrade = "never",
  repos = repos
)
```

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
