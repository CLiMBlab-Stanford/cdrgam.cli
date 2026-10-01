# Install the cdrgam Launcher

Write a launcher that selects one configured CDR-GAM root and uses the R
installation that installed the package.

## Usage

``` r
install_cli(path = NULL, root = NULL)
```

## Arguments

- path:

  Destination path, normally in a directory on `PATH`. The platform
  default is `~/.local/bin/cdrgam` on Unix and `~/.local/bin/cdrgam.cmd`
  on Windows.

- root:

  CDR-GAM root to use when `CDRGAM_ROOT` is unset. If it is not
  configured, this function creates it with local execution defaults.

## Details

The launcher uses the R executable and package library that contain the
installed `cdrgam.cli` package. This prevents packages from another R
installation's user library from shadowing its dependencies. The default
root is `tools::R_user_dir("cdrgam.cli", "data")`. A nonempty
`CDRGAM_ROOT` overrides the launcher's fallback root. The launcher is a
POSIX shell script on Unix and a command script on Windows.

## Value

The normalized launcher path, invisibly.
