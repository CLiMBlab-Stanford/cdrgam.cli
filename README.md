# cdrgam.cli

`cdrgam.cli` manages named projects and dependency-ordered CDR-GAM workloads.
One harness instance is configured with a root directory, a global concurrency
limit, and optional Slurm defaults. During development the instance directory
is normally the source checkout. It may instead be any writable directory,
which keeps mutable configuration outside an installed R package. The same
commands run serially on a local machine or submit work through a shared Slurm
scheduler and generic worker pool. A Slurm worker exits after five consecutive
minutes without ready work; the scheduler submits another worker when later
work becomes ready.

Install from the checkout:

```sh
./scripts/install
```

The installer creates `.cdrgam/checkout.yml` when necessary and installs a
launcher bound to this checkout. Projects then live under the configured root:

Use `./scripts/install --configure` to replace an existing checkout
configuration interactively.

## Platform support

Local project definition, validation, fitting, prediction, visualization, and
serial orchestration use portable R APIs and are intended to run on Linux,
macOS, and Windows. `install_cli()` writes a POSIX launcher on Unix and a
`.cmd` launcher on Windows. Names that conflict with Windows device files are
rejected so a project root can be moved between platforms.

Direct Slurm execution is a Unix feature. It requires `sh`, `sbatch`, and the
other configured Slurm commands. Subprocess execution, scheduler query
timeouts, process liveness checks, and checkout locks use cross-platform R
packages rather than platform shell utilities. Windows users can omit the
Slurm fields and use local execution. Local work remains serial but each item
runs in an isolated R process. Paging uses `less` on Unix when installed and
R's configured file viewer on Windows. Forked finite-gradient evaluation and
Linux memory-limit detection belong to the `cdrgam` core; unsupported
acceleration paths fall back to portable serial or lower-memory behavior.

```sh
cdrgam def edit brown
cdrgam def edit brown --dataset training
cdrgam def edit brown --model main
cdrgam def val brown --deep
cdrgam run -P brown -m main
```

Every command provides contextual help. Use `cdrgam --help` to list public
commands, `cdrgam COMMAND --help` for its options, or `cdrgam help def edit`
for a nested command. Internal scheduler and worker commands are omitted from
public help.

`cdrgam def edit` creates missing definitions and opens existing ones with
`$VISUAL`, `$EDITOR`, or R's configured editor. If validation fails, the edited
content is saved as a private draft and reopened by the same command; the last
valid published definition remains unchanged. Closing the editor without saving
cancels the edit without creating a draft. Initialize a project from another
project's definitions, without copying generated artifacts, with:

```sh
cdrgam def edit brown-replication --source brown
```

The same option opens a copy of a subordinate definition for editing before
publishing it under the new name:

```sh
cdrgam def edit brown --model main-alternative --source main
```

Remove an unused definition explicitly with `def rm`:

```sh
cdrgam def rm brown --model main-alternative
```

Removal is refused while another definition references the target or while
generated results exist. Remove matching results first when requested:

```sh
cdrgam purge -P brown -m main-alternative --yes
```

`def rm` selects targets by filename and does not validate their contents, so
malformed definitions remain removable. It reads the reference-bearing fields
of relevant non-target definitions and enforces reference, result, and
active-work safeguards.

Validate an entire project or diagnose one definition without changing it:

```sh
cdrgam def val brown --deep
cdrgam def val brown --model main alternative
```

List all definitions, or combine selectors to list specific matches:

```sh
cdrgam def ls
cdrgam def ls brown
cdrgam def ls brown --model
cdrgam def ls brown --dataset 'brown-*' --model 'main*'
```

Bare `cdrgam def ls` lists the checkout's available projects. A selector
without values lists every definition of that type. Once any type selector is
supplied, unselected types are omitted.

Definition selectors accept multiple values. `edit` processes literal names in
order; `ls`, `val`, and `rm` also accept `*` patterns and operate on every
match.

A model declares its training and prediction datasets:

```yaml
datasets:
  train: brown-train
  val: brown-val
  test: brown-test
```

Run selected predictions with model-internal partition names:

```sh
cdrgam run -P brown -m main -p val -p test
```

If a prediction selector is not a model partition, it must exactly match a
dataset definition. Downstream visualizations and comparisons request their
fit and prediction dependencies automatically.

See [docs/project-schema.md](docs/project-schema.md) for the definition schema
and [IMPLEMENTATION_PLAN.md](IMPLEMENTATION_PLAN.md) for orchestration
invariants and implementation structure.
