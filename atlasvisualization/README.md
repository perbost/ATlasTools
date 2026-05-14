# AtlasVisualization

`AtlasVisualization` provides visualization helpers for Atlas multi-omic
analysis workflows, including SOM maps, model summaries, box plots, radar
charts, and Jaccard-score plots.

## Author

Regis Perbost <regis.perbost@gmail.com>
Alternate contact: <regis@perbost.org>

## License

`AtlasVisualization` is licensed under the GNU General Public License, version
2 or later (`GPL-2.0-or-later`; R `DESCRIPTION` form: `GPL (>= 2)`).

When copying, modifying, or redistributing this package, preserve notices naming
`AtlasVisualization` and Regis Perbost.

When using this package in an analysis, report, publication, service, or derived
package, cite or acknowledge `AtlasVisualization` and Regis Perbost.

See `../THIRD-PARTY-NOTICES.md` for direct dependency license notes.

## Installation

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

library(AtlasVisualization)
```

From Forgejo or Gitea:

```r
install.packages(c("BiocManager", "remotes"))
repos <- BiocManager::repositories()

remotes::install_git(
  "https://forgejo.example.org/OWNER/REPOSITORY.git",
  subdir = "Library/atlasvisualization",
  dependencies = NA,
  upgrade = "never",
  repos = repos
)

library(AtlasVisualization)
```
