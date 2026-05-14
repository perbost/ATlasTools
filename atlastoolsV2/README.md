# atlastoolsv2

`atlastoolsv2` provides helpers to build and analyse Atlas multi-omic projects,
including MOFA model workflows, SOM map generation, consensus clustering,
projection analysis, immunogram generation, and feature-correlation utilities.

## Author

Regis Perbost <regis.perbost@gmail.com>
Alternate contact: <regis@perbost.org>

## License

`atlastoolsv2` is licensed under the GNU General Public License, version 2 or
later (`GPL-2.0-or-later`; R `DESCRIPTION` form: `GPL (>= 2)`).

When copying, modifying, or redistributing this package, preserve notices naming
`atlastoolsv2` and Regis Perbost.

When using this package in an analysis, report, publication, service, or derived
package, cite or acknowledge `atlastoolsv2` and Regis Perbost.

See `../THIRD-PARTY-NOTICES.md` for direct dependency license notes.

## Installation

Install `AtlasVisualization` first because `atlastoolsv2` imports it.

From GitHub:

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

library(atlastoolsv2)
```

From Forgejo or Gitea:

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

library(atlastoolsv2)
```
