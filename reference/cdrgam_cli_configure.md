# Configure a CDR-GAM Source Checkout

Validate and atomically write instance-local launcher and scheduler
configuration. The configured root and its private state directories are
created if needed. A checkout-level lock serializes configuration
publication. Local execution is portable; direct Slurm execution
requires a Unix platform with a POSIX shell and Slurm client commands.

## Usage

``` r
cdrgam_cli_configure(checkout = ".", cdrgam_root, concurrency = 1L,
  slurm_partition = NULL, slurm_account = NULL, slurm_cpus = NULL,
  slurm_memory = NULL, slurm_time = NULL, slurm_qos = NULL)
```

## Arguments

- checkout:

  Writable harness instance directory. It is normally the source
  checkout during development, but need not contain package code.

- cdrgam_root:

  Root that will contain projects and private orchestration state.

- concurrency:

  Maximum concurrent Slurm workers across the instance.

- slurm_partition, slurm_account:

  Slurm routing fields. Supply both to enable Slurm or neither for local
  execution.

- slurm_cpus, slurm_memory, slurm_time, slurm_qos:

  Optional Slurm resource defaults.

## Value

The validated checkout configuration, invisibly.
