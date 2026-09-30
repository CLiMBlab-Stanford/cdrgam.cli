# cdrgam.cli

`cdrgam.cli` manages named projects and dependency-ordered CDR-GAM
workloads. One harness instance is configured with a root directory, a
global concurrency limit, and optional Slurm defaults. During
development the instance directory is normally the source checkout. It
may instead be any writable directory, which keeps mutable configuration
outside an installed R package. The same commands run serially on a
local machine or submit work through a shared Slurm scheduler and
generic worker pool. A Slurm worker exits after five consecutive minutes
without ready work; the scheduler submits another worker when later work
becomes ready.

Install from the checkout:

``` sh
./scripts/install
```

The installer creates `.cdrgam/checkout.yml` when necessary and installs
a launcher bound to this checkout. Projects then live under the
configured root:

Use `./scripts/install --configure` to replace an existing checkout
configuration interactively.

Install the core and CLI development checkouts into an isolated library
and write a separate `cdrgam-dev` launcher with:

``` sh
./scripts/install-dev
```

The development launcher uses the configured checkout selected by
`CDRGAM_CHECKOUT`, or this source checkout by default. Set
`CDRGAM_CORE_SOURCE` when the flattened core checkout is not its
sibling. It does not replace packages installed in the active R library
or the stable `cdrgam` launcher.

## Platform support

Local project definition, validation, fitting, prediction,
visualization, and serial orchestration use portable R APIs and are
intended to run on Linux, macOS, and Windows.
[`install_cli()`](https://climblab-stanford.github.io/cdrgam.cli/dev/reference/install_cli.md)
writes a POSIX launcher on Unix and a `.cmd` launcher on Windows. Names
that conflict with Windows device files are rejected so a project root
can be moved between platforms.

Direct Slurm execution is a Unix feature. It requires `sh`, `sbatch`,
and the other configured Slurm commands. Subprocess execution, scheduler
query timeouts, process liveness checks, and checkout locks use
cross-platform R packages rather than platform shell utilities. Windows
users can omit the Slurm fields and use local execution. Local work
remains serial but each item runs in an isolated R process. Paging uses
`less` on Unix when installed and R’s configured file viewer on Windows.
Forked finite-gradient evaluation and Linux memory-limit detection
belong to the `cdrgam` core; unsupported acceleration paths fall back to
portable serial or lower-memory behavior.

``` sh
cdrgam def init brown
cdrgam def edit brown --dataset training
cdrgam def edit brown --model main
cdrgam def val brown --deep
cdrgam run -P brown -m main
```

Every command provides contextual help. Use `cdrgam --help` to list
public commands, `cdrgam COMMAND --help` for its options, or
`cdrgam help def edit` for a nested command. Internal scheduler and
worker commands are omitted from public help.

`cdrgam def init` creates a project, initializes a Git repository on
`main`, and stages its initial source files. `cdrgam def edit` creates
missing definitions and opens existing ones with `$VISUAL`, `$EDITOR`,
or R’s configured editor. If validation fails, the edited content is
saved as a private draft and reopened by the same command; the last
valid published definition remains unchanged. Closing the editor without
saving cancels the edit without creating a draft. Initialize a project
from another project’s definitions and project-owned `code/`, without
copying generated artifacts, with:

``` sh
cdrgam def init brown-replication --source brown
```

The same option opens a copy of a subordinate definition for editing
before publishing it under the new name:

``` sh
cdrgam def edit brown --model main-alternative --source main
```

Remove an unused definition explicitly with `def rm`:

``` sh
cdrgam def rm brown --model main-alternative
```

Removal is refused while another definition references the target or
while generated results exist. Remove matching results first when
requested:

``` sh
cdrgam purge -P brown -m main-alternative --yes
```

`def rm` selects targets by filename and does not validate their
contents, so malformed definitions remain removable. It reads the
reference-bearing fields of relevant non-target definitions and enforces
reference, result, and active-work safeguards.

Validate an entire project or diagnose one definition without changing
it:

``` sh
cdrgam def val brown --deep
cdrgam def val brown --model main alternative
```

List all definitions, or combine selectors to list specific matches:

``` sh
cdrgam def ls
cdrgam def ls brown
cdrgam def ls brown --model
cdrgam def ls brown --dataset 'brown-*' --model 'main*'
cdrgam def ls brown --model 're:^main-(linear|nonlinear)$'
```

Bare `cdrgam def ls` lists the checkout’s available projects. A selector
without values lists every definition of that type. Once any type
selector is supplied, unselected types are omitted.

Definition selectors accept multiple values. `edit` processes literal
names in order; `ls`, `val`, and `rm` also accept `*` globs and
`re:`-prefixed Perl-compatible regular expressions and operate on every
match. Regular expressions use ordinary substring matching unless you
include `^` or `$`. Quote patterns so the shell does not interpret them.

A model declares its training and prediction datasets:

``` yaml
datasets:
  train: brown-train
  val: brown-val
  test: brown-test
```

Run selected predictions with model-internal partition names:

``` sh
cdrgam run -P brown -m main -p val -p test
cdrgam run -P val -m 're:^natstor-(raw|log)-l[01]s[01]h[01]$'
```

Project, model, visualization, and comparison selectors accept exact
names, `*` globs, and `re:`-prefixed regular expressions. If a
prediction selector is not a model partition, it must exactly match a
dataset definition. Downstream visualizations and comparisons request
their fit and prediction dependencies automatically.

Generated artifacts live under the project’s ignored `results/`
directory. Prepare independently publishable sources and results with:

``` sh
cdrgam publish brown --results archive --archive brown-results.tar.gz
cdrgam publish brown --results url --url URL --sha256 DIGEST
cdrgam publish brown --results none --commit 'Publish analysis sources' --push
```

Archive mode includes only complete, current, converged selected
artifacts and their dependency closure. `publish` stages
`publication.yml`; commit and push occur only when explicitly requested.
Fetching installs project sources, verifies separately published results
when available, and rebuilds local registry state from artifact
manifests:

``` sh
cdrgam fetch https://github.com/example/brown-cdrgam
cdrgam fetch SOURCE --source-only
cdrgam fetch SOURCE --results brown-results.tar.gz
```

See
[docs/project-schema.md](https://climblab-stanford.github.io/cdrgam.cli/dev/docs/project-schema.md)
for the definition schema and
[IMPLEMENTATION_PLAN.md](https://climblab-stanford.github.io/cdrgam.cli/dev/IMPLEMENTATION_PLAN.md)
for orchestration invariants and implementation structure.
