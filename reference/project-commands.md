# Manage Configured CDR-GAM Projects

These functions define, resolve, validate, plan, execute, inspect, and
clean projects beneath the configured CDR-GAM root. Local execution is
serial and isolates each work item in its own R process. Slurm execution
uses the root-wide controller and concurrency limit. Cleanup is
restricted to generated artifacts and private orchestration state.
Status preserves the artifact lifecycle state and displays published
fits as `Nonconverged` when their recorded optimizer diagnostics report
that convergence was not reached. It displays a converged fit as
`Warning` when the core diagnostics identify a numerical or inferential
condition that requires review. Warning fits remain complete artifacts.
`cdrgam_cli_def()` initializes Git-managed projects, creates missing
definitions, and edits existing YAML through a validated temporary file.
A failed edit is retained in the managed private draft directory and
reopened by the next edit of the same definition; the last valid
published definition remains unchanged. With `source`, it initializes a
new project or subordinate definition from an existing one. A project
copy includes its definitions and project-owned `code/` tree. The `"rm"`
operation removes only unreferenced subordinate definitions and refuses
to remove definitions with generated results. The `"val"` operation
validates a project or one definition and its prerequisites without
changing files. The `"mv"` operation renames an unreferenced subordinate
definition verbatim and refuses to proceed while generated results or
registry workloads exist for its old name. Definition selectors accept
multiple names. Validation and removal expand `*` patterns; editing
processes literal names in order. A batch removal checks every target
before removing any definition. Copying does not include generated
artifacts or private orchestration state. Purging removes only selected
generated artifacts and their registry records. Registry records and
private attempt directories are removed even when the corresponding
generated artifacts are already absent. The `work` and `logs` options
add private orchestration paths to the selection; they do not suppress
the selected result artifacts. Work-item logs are overwritten when the
corresponding logical workload starts again. Worker logs record the
timestamped sequence of work items claimed by each generic worker.
`cdrgam_cli_log()` shows work-item logs by default and worker logs when
`worker = TRUE`.

## Usage

``` r
cdrgam_cli_def(project = NULL, type = NULL, name = NULL,
  source = NULL, cdrgam_root = NULL, editor = NULL,
  operation = c("edit", "init", "rm", "mv", "val"), deep = FALSE)

cdrgam_cli_validate(projects = NULL, deep = FALSE, cdrgam_root = NULL)

cdrgam_cli_list(projects = NULL, cdrgam_root = NULL)

cdrgam_cli_log(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, lines = NULL,
  cdrgam_root = NULL, pager = NULL, worker = FALSE, running = FALSE)

cdrgam_cli_plan(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, cdrgam_root = NULL)

cdrgam_cli_run(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, dry_run = FALSE,
  cpus = NULL, partition = NULL, memory = NULL, time = NULL,
  scheduler_time = NULL, qos = NULL, cdrgam_root = NULL)

cdrgam_cli_status(projects = NULL, cdrgam_root = NULL, pager = NULL,
  use_pager = TRUE)

cdrgam_cli_purge(projects = NULL, models = NULL, predictions = NULL,
  visualizations = NULL, comparisons = NULL, datasets = NULL,
  work = FALSE, logs = FALSE, force = FALSE, cdrgam_root = NULL,
  confirm = NULL, pager = NULL, use_pager = TRUE)

find_cdrgam_project(project = NULL, cdrgam_root = NULL)
```

## Arguments

- name:

  One or more definition names. Validation and removal accept `*`
  patterns; editing treats names literally. Renaming requires the
  existing and new names, in that order.

- type:

  Optional definition type: `dataset`, `model`, `visualization`, or
  `comparison`.

- source:

  Optional source project or subordinate definition name. The source
  initializes the requested target without generated artifacts. A
  project copy includes its definitions and `code/`; subordinate
  definitions are opened in the editor before publication.

- operation:

  Either `"init"` to initialize a version-controlled project, `"edit"`
  to create or edit a definition, `"rm"` to remove a subordinate
  definition after safety checks, or `"mv"` to rename one unused
  subordinate definition, or `"val"` to validate definitions without
  changing them.

- editor:

  Editor command or callback. The default uses `VISUAL`, then `EDITOR`,
  then R's configured editor.

- project:

  An optional project directory name inferred from the current directory
  when omitted, or `"site"` for root-local configuration. Project
  initialization requires an explicit new name. The directory name is
  authoritative when it differs from the descriptive name in the project
  definition.

- projects:

  Project selectors. The default uses the current project when possible
  and otherwise selects every configured project.

- models:

  Model selectors. Repeated values are combined. Across multiple
  projects, selectors match the union of available models rather than
  being required to match within every project.

- predictions:

  Prediction partition or dataset selectors.

- visualizations:

  Visualization selectors.

- comparisons:

  Comparison selectors.

- datasets:

  Dataset artifact selectors used by `cdrgam_cli_purge()`.

- cdrgam_root:

  The configured CDR-GAM root. The launcher supplies it automatically
  through `CDRGAM_ROOT`.

- deep:

  Whether to read datasets and compile training model designs.

- lines:

  Optional maximum number of trailing lines from each log.

- worker:

  Whether to show generic worker lifecycle logs instead of work-item
  process logs. Worker mode does not accept workload selectors.

- running:

  Whether to show only logs for currently running work items.

- pager:

  Optional pager executable or command-specific callback. Interactive
  terminals use `less` by default.

- use_pager:

  Whether interactive status output or a purge selection may open the
  pager.

- dry_run:

  Whether to print the dependency-closed plan without running it.

- cpus, partition, memory, time, qos:

  Optional Slurm worker resource overrides. `partition` also applies to
  a newly submitted root scheduler.

- scheduler_time:

  Optional root scheduler time-limit override. Worker time remains
  controlled by `time`.

- work:

  Whether to select private work attempts for cleanup.

- logs:

  Whether to select project-private work-item logs for cleanup.

- force:

  Whether to remove selected generated paths without showing the preview
  or prompting for confirmation.

- confirm:

  Whether `cdrgam_cli_purge()` confirms removal after showing the
  selection. By default, confirmation is offered only when standard
  input and output are terminals.
