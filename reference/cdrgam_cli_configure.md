# Configure a CDR-GAM Root

Validate and atomically write root-local site and scheduler
configuration. The root and its private state directories are created if
needed. A root-local lock serializes configuration publication. Local
execution is portable; direct Slurm execution requires a Unix platform
with a POSIX shell and Slurm client commands.

## Usage

``` r
cdrgam_cli_configure(cdrgam_root = .cdrgam_cli_default_root(), concurrency = 1L,
  slurm_partition = NULL, slurm_account = NULL, slurm_cpus = NULL,
  slurm_memory = NULL, slurm_time = NULL, slurm_qos = NULL)
```

## Arguments

- cdrgam_root:

  Root containing site configuration, projects, and private
  orchestration state. The default is R's platform-specific user data
  directory for `cdrgam.cli`.

- concurrency:

  Maximum concurrent Slurm workers across the root.

- slurm_partition, slurm_account:

  Slurm routing fields. Supply both to enable Slurm or neither for local
  execution.

- slurm_cpus, slurm_memory, slurm_time, slurm_qos:

  Optional Slurm resource defaults.

## Value

The validated site configuration, invisibly.
