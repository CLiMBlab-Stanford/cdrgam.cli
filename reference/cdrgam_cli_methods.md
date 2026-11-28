# Write a methods section

Describes the selected work items and their upstream lineage as a
methods section with citations, and writes a trace of the inputs behind
it.

## Usage

``` r
cdrgam_cli_methods(
  projects = NULL,
  models = NULL,
  predictions = NULL,
  visualizations = NULL,
  comparisons = NULL,
  tests = NULL,
  preview = FALSE,
  format = "rtf",
  output = NULL,
  cdrgam_root = NULL
)
```

## Arguments

- projects, models, predictions, visualizations, comparisons, tests:

  Selectors with the same meaning as in
  [`cdrgam_cli_plan()`](https://climblab-stanford.github.io/cdrgam.cli/reference/project-commands.md).
  Several projects are pooled into one report.

- preview:

  Describe resolved definitions without reading artifacts.

- format:

  Output format: `"rtf"` (the default), `"md"`, or `"tex"`. The tex
  format writes LaTeX with natbib citations and a `methods.bib` file of
  the cited references.

- output:

  Output directory. Defaults to `results/methods` in the selected
  project and is required when several projects are selected. Existing
  files of the same names are replaced.

- cdrgam_root:

  Configured CDR-GAM root.

## Value

The paths of the written files, invisibly.

## Details

Without `preview`, only work items whose current resolved identity has a
complete artifact contribute. Stale and missing work items are omitted,
counted on stderr, and listed in `methods.json`. With `preview = TRUE`,
the report describes resolved definitions alone and reads no artifacts.
Both modes resolve dataset identities, so dataset sources must be
readable.
