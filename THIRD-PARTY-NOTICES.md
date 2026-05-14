# Third-Party Notices

This inventory covers the direct R libraries used by `atlastoolsv2` and
`AtlasVisualization`, based on their `DESCRIPTION` files, explicit `pkg::`
namespace calls, and explicit `require()` calls in `R/` sources on 2026-05-14.

The selected license for the local packages is `GPL-2.0-or-later` (`GPL (>= 2)`
in R package metadata). This is a conservative copyleft choice because the
project directly uses multiple GPL libraries, including GPL-2-only packages.
The third-party packages are loaded as separate R dependencies and are not
vendored in this directory; they retain their own upstream licenses.

## Direct Dependencies

| Dependency | Used by | License |
| --- | --- | --- |
| `AtlasVisualization` | `atlastoolsv2` | Local package, GPL-2.0-or-later |
| `coin` | `atlastoolsv2` source (`coin::`) | GPL-2 |
| `combinat` | `atlastoolsv2` | GPL-2 |
| `ConsensusClusterPlus` | `atlastoolsv2` | GPL version 2 |
| `data.table` | `atlastoolsv2` | MPL-2.0 |
| `dplyr` | `atlastoolsv2` | MIT |
| `factoextra` | `atlastoolsv2` | GPL-2 |
| `fgsea` | `atlastoolsv2` | MIT |
| `fmsb` | `AtlasVisualization` | GPL-2 or GPL-3, expanded from GPL (>= 2) |
| `gage` | `atlastoolsv2` | GPL (>= 2.0) |
| `ggplot2` | Both packages | MIT |
| `ggpubr` | `AtlasVisualization` | GPL (>= 2) |
| `glmnet` | `atlastoolsv2` | GPL-2 |
| `glue` | `atlastoolsv2` | MIT |
| `gtable` | `AtlasVisualization` | MIT |
| `kohonen` | Both packages | GPL (>= 2) |
| `MASS` | `atlastoolsv2` | GPL-2 or GPL-3 |
| `mclust` | `atlastoolsv2` | GPL (>= 2) |
| `MOFA2` | Both packages | LGPL-3 |
| `parallel` | `atlastoolsv2` | Part of R |
| `pheatmap` | `atlastoolsv2` | GPL-2 |
| `prabclus` | `atlastoolsv2` | GPL |
| `purrr` | `atlastoolsv2` | MIT |
| `RColorBrewer` | `atlastoolsv2` | Apache-2.0 |
| `readr` | `atlastoolsv2` | MIT |
| `reshape2` | `atlastoolsv2` | MIT |
| `rlang` | `AtlasVisualization` | MIT |
| `scales` | Both packages source (`scales::`) | MIT |
| `stats` | `atlastoolsv2` | Part of R |
| `stringr` | `atlastoolsv2` | MIT |
| `testthat` | `atlastoolsv2` tests/import metadata | MIT |
| `tibble` | `atlastoolsv2` | MIT |
| `tidyr` | `atlastoolsv2` | MIT |
| `utils` | `atlastoolsv2` source (`utils::`) | Part of R |

## Copyleft Compatibility Note

`GPL-2.0-or-later` was selected for the local packages because it is compatible
with the GPL-family dependencies used by the project and gives downstream users
the option to apply later GPL versions where required by compatible dependencies.
Apache-2.0 and MPL-2.0 dependencies remain separate packages. If third-party
source code or data is copied into this directory in the future, its provenance
and license should be reviewed before distribution.
