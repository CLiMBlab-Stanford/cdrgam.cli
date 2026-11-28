\name{cdrgam_cli_stop}
\alias{cdrgam_cli_stop}
\title{Stop selected Slurm work}
\usage{
cdrgam_cli_stop(
  projects = NULL,
  models = NULL,
  predictions = NULL,
  visualizations = NULL,
  comparisons = NULL,
  force = FALSE,
  scheduler = FALSE,
  cdrgam_root = NULL,
  tests = NULL
)
}
\arguments{
\item{projects, models, predictions, visualizations, comparisons, tests}{Selector
dimensions are conjunctive; repeated values within a dimension are
combined. With multiple projects, definition selectors match their union
rather than being required to match inside every project. Repeated
prediction selectors expand model partitions.}

\item{force}{Whether to record a work-item stop when the root scheduler
cannot be contacted. In scheduler mode, force also cancels active worker
jobs before the scheduler exits.}

\item{scheduler}{Whether to stop the idle root scheduler rather than work
items. Scheduler mode cannot be combined with selectors.}

\item{cdrgam_root}{Configured CDR-GAM root.}
}
\value{
Stopped work keys, invisibly.
}
\description{
Stop selected Slurm work
}
\details{
Stops matching target work items without removing their definitions,
artifacts, logs, or checkpoints. A later \verb{cdrgam run} may submit them again.
The root scheduler otherwise stays available until stopped or its Slurm time
limit expires.
}
