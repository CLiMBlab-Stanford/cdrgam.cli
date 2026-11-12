\name{cdrgam_cli_viz}
\alias{cdrgam_cli_viz}
\title{Serve the model viewer}
\usage{
cdrgam_cli_viz(cdrgam_root = NULL, stop = FALSE, restart = FALSE,
  local = FALSE, port = NULL, cpus = NULL, memory = NULL, time = NULL,
  partition = NULL, qos = NULL, wait = 120)
}
\arguments{
  \item{cdrgam_root}{Configured CDR-GAM root.}
  \item{stop}{Cancel the recorded viewer job instead of starting one.}
  \item{restart}{Cancel the recorded viewer job, then start a new one.}
  \item{local}{Serve from this process even on a Slurm-configured root.}
  \item{port}{Port to listen on. The default is a stable per-user port, or the
  next free port after it.}
  \item{cpus, memory, time, partition, qos}{Slurm resource overrides for the
  viewer job. The defaults are one CPU, 16G, and 12 hours, with the partition,
  account, and QOS taken from the root configuration.}
  \item{wait}{Seconds to wait for a submitted job to start serving.}
}
\value{
The active server record, invisibly, or \code{NULL} when the job has not
started within \code{wait} seconds or the server ran in the foreground.
}
\description{
Starts, reports, or stops the current user's browser viewer for completed fits
under the configured root. The viewer plots effect lines, with several lines
from any fits on one plot, and two-axis effect surfaces. Each line or surface
selects a term, the axes that vary, and fixed values for the remaining axes.
A reference can be subtracted from each line or surface; an empty reference
field leaves that predictor at the model default. Estimates and pointwise intervals are on the linear-predictor scale.
}
\details{
On a root whose scheduler is Slurm, the viewer runs in a Slurm job, and later
calls by the same user reuse that job. Otherwise, or with \code{local = TRUE},
it serves from this process until interrupted. A Slurm viewer listens on all
interfaces of its node, without authentication, so other users who can reach
the node can also open it. The printed \code{ssh} command forwards the port
through the login host named by the \env{CDRGAM_VIZ_GATEWAY} environment
variable, or through the invoking host when it is unset. A local viewer listens
only on the loopback interface. Viewer state, including the job record and server log, is kept
under \code{.cdrgam/viewers/} in the root.

In a Slurm viewer, the Refresh data button rescans for completed fits and
reloads fits whose results changed, and the Restart server button restarts the
server inside the same job with the currently installed code, on the same
port.

The viewer requires the optional packages \pkg{shiny}, \pkg{bslib},
\pkg{plotly}, \pkg{jsonlite}, and \pkg{later}.
}
