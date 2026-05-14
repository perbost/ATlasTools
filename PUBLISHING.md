# Publishing The Atlas R Libraries

These packages can be hosted publicly on GitHub, Forgejo, Gitea, or any Git
server reachable by `remotes::install_git()`.

## Repository Layout

Use one of these layouts.

### Option A: Publish The Whole Parent Repository

Keep this directory at the repository root as `Library/`:

```text
REPOSITORY/
  Library/
    atlasvisualization/
    atlastoolsV2/
    install_atlas_libraries.R
```

Public users install with the default helper setting:

```r
source("https://raw.githubusercontent.com/OWNER/REPOSITORY/main/Library/install_atlas_libraries.R")
install_atlas_libraries("https://github.com/OWNER/REPOSITORY.git")
```

### Option B: Publish `Library/` As The Repository Root

Move the contents of `Library/` to the root of a new public repository:

```text
REPOSITORY/
  atlasvisualization/
  atlastoolsV2/
  install_atlas_libraries.R
```

Public users install with `subdir_prefix = "."`:

```r
source("https://raw.githubusercontent.com/OWNER/REPOSITORY/main/install_atlas_libraries.R")
install_atlas_libraries(
  "https://github.com/OWNER/REPOSITORY.git",
  subdir_prefix = "."
)
```

## Release Checklist

1. Make the repository public.
2. Keep `atlasvisualization/` and `atlastoolsV2/` as R package directories.
3. Install `AtlasVisualization` before `atlastoolsv2`.
4. Use `BiocManager::repositories()` in installation scripts because the
   packages use Bioconductor dependencies.
5. Create a Git tag such as `v0.1.0` once the first public version is ready.
6. Test installation from a fresh R session using the public URL.
