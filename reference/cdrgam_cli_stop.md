# Stop selected Slurm work

Stop selected Slurm work

## Usage

``` r
cdrgam_cli_stop(
  projects = NULL,
  models = NULL,
  predictions = NULL,
  visualizations = NULL,
  comparisons = NULL,
  force = FALSE,
  scheduler = FALSE,
  cdrgam_root = NULL
)
```

## Arguments

- projects, models, predictions, visualizations, comparisons:

  Selector dimensions are conjunctive; repeated values within a
  dimension are combined. With multiple projects, definition selectors
  match their union rather than being required to match inside every
  project. Repeated prediction selectors expand model partitions.

- force:

  Whether to record a work-item stop when the root scheduler cannot be
  contacted. In scheduler mode, force also cancels active worker jobs
  before the scheduler exits.

- scheduler:

  Whether to stop the idle root scheduler rather than work items.
  Scheduler mode cannot be combined with selectors.

- cdrgam_root:

  Configured CDR-GAM root.

## Value

Stopped work keys, invisibly.

## Details

Stops matching target work items without removing their definitions,
artifacts, logs, or checkpoints. A later `cdrgam run` may submit them
again. The root scheduler otherwise stays available until stopped or its
Slurm time limit expires.
