\name{project-commands}
\alias{cdrgam_cli_def}
\alias{cdrgam_cli_validate}
\alias{cdrgam_cli_list}
\alias{cdrgam_cli_log}
\alias{cdrgam_cli_plan}
\alias{cdrgam_cli_run}
\alias{cdrgam_cli_status}
\alias{cdrgam_cli_purge}
\alias{find_cdrgam_project}
\title{Manage Configured CDR-GAM Projects}
\usage{
cdrgam_cli_def(project = NULL, type = NULL, name = NULL,
  copy_from_to = NULL, checkout = NULL, editor = NULL)

cdrgam_cli_validate(projects = NULL, deep = FALSE, checkout = NULL)

cdrgam_cli_list(projects = NULL, checkout = NULL)

cdrgam_cli_log(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, lines = NULL,
  checkout = NULL, pager = NULL)

cdrgam_cli_plan(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, checkout = NULL)

cdrgam_cli_run(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, dry_run = FALSE,
  cpus = NULL, memory = NULL, time = NULL, qos = NULL,
  checkout = NULL)

cdrgam_cli_status(projects = NULL, checkout = NULL, pager = NULL,
  use_pager = TRUE)

cdrgam_cli_purge(projects = NULL, models = NULL, work = FALSE,
  logs = FALSE, yes = FALSE, checkout = NULL)

find_cdrgam_project(project = NULL, checkout = NULL)
}
\arguments{
  \item{name}{An atomic project name.}
  \item{type}{Optional definition type: \code{dataset}, \code{model},
  \code{visualization}, or \code{comparison}.}
  \item{copy_from_to}{Optional source and destination project names. This
  operation cannot be combined with other definition arguments.}
  \item{editor}{Editor command or callback. The default uses \env{VISUAL},
  then \env{EDITOR}, then R's configured editor.}
  \item{project}{One project directory name, or \code{"site"} for checkout
  configuration. The directory name is authoritative when it differs from
  the descriptive name in the project definition.}
  \item{projects}{Project selectors. The default uses the current project when
  possible and otherwise selects every configured project.}
  \item{models}{Model selectors.}
  \item{predictions}{Prediction partition or dataset selectors.}
  \item{visualizations}{Visualization selectors.}
  \item{comparisons}{Comparison selectors.}
  \item{checkout}{The configured source checkout. The launcher supplies it
  automatically.}
  \item{deep}{Whether to read datasets and compile training model designs.}
  \item{lines}{Optional maximum number of trailing lines from each log.}
  \item{pager}{Optional pager executable or command-specific callback.
  Interactive terminals use \command{less} by default.}
  \item{use_pager}{Whether interactive status output may open the pager.}
  \item{dry_run}{Whether to print the dependency-closed plan without running it.}
  \item{cpus, memory, time, qos}{Optional Slurm resource overrides.}
  \item{work}{Whether to select private work attempts for cleanup.}
  \item{logs}{Whether to select private scheduler logs for cleanup.}
  \item{yes}{Whether to remove selected generated paths after preview.}
}
\description{
These functions define, resolve, validate, plan, execute, inspect, and
clean projects beneath the checkout's configured CDR-GAM root. Local execution
is serial. Slurm execution uses the checkout-wide controller and concurrency
limit. Cleanup is restricted to generated artifacts and private orchestration
state.
Status preserves the artifact lifecycle state and displays published fits as
\code{Nonconverged} when their recorded optimizer diagnostics report that
convergence was not reached.
\code{cdrgam_cli_def()} creates missing definitions and edits existing YAML
through a validated temporary file. Project copying includes definitions but
not generated artifacts or private orchestration state.
}
