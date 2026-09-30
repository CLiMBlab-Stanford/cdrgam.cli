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
  source = NULL, checkout = NULL, editor = NULL,
  operation = c("edit", "init", "rm", "val"), deep = FALSE)

cdrgam_cli_validate(projects = NULL, deep = FALSE, checkout = NULL)

cdrgam_cli_list(projects = NULL, checkout = NULL)

cdrgam_cli_log(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, lines = NULL,
  checkout = NULL, pager = NULL, worker = FALSE)

cdrgam_cli_plan(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, checkout = NULL)

cdrgam_cli_run(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, dry_run = FALSE,
  cpus = NULL, memory = NULL, time = NULL, qos = NULL,
  checkout = NULL)

cdrgam_cli_status(projects = NULL, checkout = NULL, pager = NULL,
  use_pager = TRUE)

cdrgam_cli_purge(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, datasets = NULL,
  work = FALSE, logs = FALSE, yes = FALSE, checkout = NULL)

find_cdrgam_project(project = NULL, checkout = NULL)
}
\arguments{
  \item{name}{One or more definition names. Validation and removal accept
  \code{*} patterns; editing treats names literally.}
  \item{type}{Optional definition type: \code{dataset}, \code{model},
  \code{visualization}, or \code{comparison}.}
  \item{source}{Optional source project or subordinate definition name. The
  source initializes the requested target without generated artifacts. A
  project copy includes its definitions and \file{code/}; subordinate
  definitions are opened in the editor before publication.}
  \item{operation}{Either \code{"init"} to initialize a version-controlled
  project, \code{"edit"} to create or edit a definition,
  \code{"rm"} to remove a subordinate definition after safety checks, or
  \code{"val"} to validate definitions without changing them.}
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
  \item{datasets}{Dataset artifact selectors used by
  \code{cdrgam_cli_purge()}.}
  \item{checkout}{The configured harness instance. The launcher supplies it
  automatically.}
  \item{deep}{Whether to read datasets and compile training model designs.}
  \item{lines}{Optional maximum number of trailing lines from each log.}
  \item{worker}{Whether to show generic worker lifecycle logs instead of
  work-item process logs. Worker mode does not accept workload selectors.}
  \item{pager}{Optional pager executable or command-specific callback.
  Interactive terminals use \command{less} by default.}
  \item{use_pager}{Whether interactive status output may open the pager.}
  \item{dry_run}{Whether to print the dependency-closed plan without running it.}
  \item{cpus, memory, time, qos}{Optional Slurm resource overrides.}
  \item{work}{Whether to select private work attempts for cleanup.}
  \item{logs}{Whether to select project-private work-item logs for cleanup.}
  \item{yes}{Whether to remove selected generated paths after preview.}
}
\description{
These functions define, resolve, validate, plan, execute, inspect, and
clean projects beneath the checkout's configured CDR-GAM root. Local execution
is serial and isolates each work item in its own R process. Slurm execution
uses the checkout-wide controller and concurrency limit. Cleanup is restricted
to generated artifacts and private orchestration state.
Status preserves the artifact lifecycle state and displays published fits as
\code{Nonconverged} when their recorded optimizer diagnostics report that
convergence was not reached.
\code{cdrgam_cli_def()} initializes Git-managed projects, creates missing
definitions, and edits existing YAML
through a validated temporary file. A failed edit is retained in the managed
private draft directory and reopened by the next edit of the same definition;
the last valid published definition remains unchanged. With \code{source}, it
initializes a new project or subordinate definition from an existing one. A
project copy includes its definitions and project-owned \file{code/} tree. The \code{"rm"}
operation removes only unreferenced subordinate definitions and refuses to
remove definitions with generated results. The \code{"val"} operation validates
a project or one definition and its prerequisites without changing files.
Definition selectors accept multiple names. Validation and removal expand
\code{*} patterns; editing processes literal names in order. A batch removal
checks every target before removing any definition.
Copying does not include generated artifacts or private orchestration state.
Purging removes only selected generated artifacts and their registry records.
Registry records and private attempt directories are removed even when the
corresponding generated artifacts are already absent. The \code{work} and
\code{logs} options add private orchestration paths to the selection; they do
not suppress the selected result artifacts.
Work-item logs are overwritten when the corresponding logical workload starts
again. Worker logs record the timestamped sequence of work items claimed by
each generic worker. \code{cdrgam_cli_log()} shows work-item logs by default and
worker logs when \code{worker = TRUE}.
}
