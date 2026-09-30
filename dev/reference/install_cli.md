# Install the Checkout-Bound cdrgam Launcher

Write a launcher that selects one configured harness instance and uses
the R installation that installed the package.

## Usage

``` r
install_cli(path = NULL, checkout = NULL)
```

## Arguments

- path:

  Destination path, normally in a directory on `PATH`. The platform
  default is `~/.local/bin/cdrgam` on Unix and `~/.local/bin/cdrgam.cmd`
  on Windows.

- checkout:

  A configured harness instance containing `.cdrgam/checkout.yml`.

## Details

The launcher uses the R executable and package library that contain the
installed `cdrgam.cli` package. This prevents packages from another R
installation's user library from shadowing its dependencies. The
launcher is a POSIX shell script on Unix and a command script on
Windows.

## Value

The normalized launcher path, invisibly.
