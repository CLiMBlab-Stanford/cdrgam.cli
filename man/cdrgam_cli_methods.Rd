\name{cdrgam_cli_methods}
\alias{cdrgam_cli_methods}
\title{Write a methods section}
\usage{
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
}
\arguments{
\item{projects, models, predictions, visualizations, comparisons, tests}{Selectors
with the same meaning as in \code{\link[=cdrgam_cli_plan]{cdrgam_cli_plan()}}. Several
projects are pooled into one report.}

\item{preview}{Describe resolved definitions without reading artifacts.}

\item{format}{Output format: \code{"rtf"} (the default), \code{"md"}, or \code{"tex"}. The tex format writes LaTeX with natbib citations and a \code{methods.bib} file of the cited references.}

\item{output}{Output directory. Defaults to \code{results/methods} in the
selected project and is required when several projects are selected.
Existing files of the same names are replaced.}

\item{cdrgam_root}{Configured CDR-GAM root.}
}
\value{
The paths of the written files, invisibly.
}
\description{
Describes the selected work items and their upstream lineage as a methods
section with citations, and writes a trace of the inputs behind it.
}
\details{
Without \code{preview}, only work items whose current resolved identity has a
complete artifact contribute. Stale and missing work items are omitted,
counted on stderr, and listed in \code{methods.json}. With \code{preview = TRUE},
the report describes resolved definitions alone and reads no artifacts.
Both modes resolve dataset identities, so dataset sources must be readable.
}
