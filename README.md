# cdrgam.cli

`cdrgam.cli` manages named projects and dependency-ordered CDR-GAM workloads.
One harness instance is configured with a root directory, a global concurrency
limit, and optional Slurm defaults. During development the instance directory
is normally the source checkout. It may instead be any writable directory,
which keeps mutable configuration outside an installed R package. The same
commands run serially on a local machine or submit work through a shared Slurm
scheduler and generic worker pool.

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

`cdrgam def edit` creates missing definitions and opens existing ones with
`$VISUAL`, `$EDITOR`, or R's configured editor. If validation fails, the edited
content is saved as a private draft and reopened by the same command; the last
valid published definition remains unchanged. Initialize a project from another
project's definitions, without copying generated artifacts, with:

```sh
cdrgam def edit brown-replication --source brown
```

The same option copies a subordinate definition within a project:

```sh
cdrgam def edit brown --model main-alternative --source main
```

Delete an unused definition explicitly with `def del`:

```sh
cdrgam def del brown --model main-alternative
```

Deletion is refused while another definition references the target or while
generated results exist. Remove matching results first when requested:

```sh
cdrgam purge -P brown -m main-alternative --yes
```

Validate an entire project or diagnose one definition without changing it:

```sh
cdrgam def val brown --deep
cdrgam def val brown --model main alternative
```

Definition selectors accept multiple values. `edit` processes literal names in
order; `val` and `del` also accept `*` patterns and operate on every match.

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
