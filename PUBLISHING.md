# Publishing The Atlas R Libraries

This repository is the standalone AtlasTools repository:

```text
https://git.perbost.org/regis/AtlasTools.git
https://github.com/perbost/ATlasTools.git
```

It contains two R package directories at the repository root:

```text
AtlasTools/
  atlasvisualization/
  atlastoolsV2/
  install_atlas_libraries.R
```

## Public Install Test

From GitHub:

```r
source("https://raw.githubusercontent.com/perbost/ATlasTools/main/install_atlas_libraries.R")
install_atlas_libraries()
```

From Forgejo:

```r
source("https://git.perbost.org/regis/AtlasTools/raw/branch/main/install_atlas_libraries.R")
install_atlas_libraries(
  repo_url = "https://git.perbost.org/regis/AtlasTools.git"
)
```

## Release Checklist

1. Make the repository public.
2. Keep `atlasvisualization/` and `atlastoolsV2/` as R package directories.
3. Install `AtlasVisualization` before `atlastoolsv2`.
4. Use `BiocManager::repositories()` in installation scripts because the
   packages use Bioconductor dependencies.
5. Push `main` to Forgejo and GitHub.
6. Create a Git tag such as `v0.1.0` once the first public version is ready.
7. Test installation from a fresh R session using the GitHub public URL.
