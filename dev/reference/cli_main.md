# Dispatch the cdrgam Command

Parse command-line arguments, render contextual help, and dispatch a
project-management command.

## Usage

``` r
cli_main(args = commandArgs(trailingOnly = TRUE))
```

## Arguments

- args:

  Command arguments excluding the executable name.

## Value

An integer exit status, invisibly.
