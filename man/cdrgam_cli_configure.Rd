\name{cdrgam_cli_configure}
\alias{cdrgam_cli_configure}
\title{Configure a CDR-GAM Source Checkout}
\usage{
cdrgam_cli_configure(checkout = ".", cdrgam_root, concurrency = 1L,
  slurm_partition = NULL, slurm_account = NULL, slurm_cpus = NULL,
  slurm_memory = NULL, slurm_time = NULL, slurm_qos = NULL)
}
\arguments{
  \item{checkout}{Writable harness instance directory. It is normally the
  source checkout during development, but need not contain package code.}
  \item{cdrgam_root}{Root that will contain projects and private orchestration
  state.}
  \item{concurrency}{Maximum concurrent Slurm workers across the instance.}
  \item{slurm_partition, slurm_account}{Slurm routing fields. Supply both to
  enable Slurm or neither for local execution.}
  \item{slurm_cpus, slurm_memory, slurm_time, slurm_qos}{Optional Slurm
  resource defaults.}
}
\description{
Validate and atomically write instance-local launcher and scheduler
configuration. The configured root and its private state directories are
created if needed. A checkout-level lock serializes configuration publication.
Local execution is portable; direct Slurm execution requires a Unix platform
with a POSIX shell and Slurm client commands.
}
\value{The validated checkout configuration, invisibly.}
