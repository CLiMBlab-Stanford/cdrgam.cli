# cdrgam.cli

`cdrgam.cli` manages named projects and dependency-ordered CDR-GAM
workloads. One harness instance is a root directory containing its site
configuration, projects, registry, scheduler metadata, and other private
state. The root has a global concurrency limit and optional Slurm
defaults. The same commands run serially on a local machine or submit
work through a shared Slurm scheduler and generic worker pool. A Slurm
worker exits after five consecutive minutes without ready work; the
scheduler submits another worker when later work becomes ready.

Install from the checkout:

``` sh
./scripts/install
```

The installer creates `$CDRGAM_ROOT/.cdrgam/site.yml` when necessary and
installs a launcher with that root as its fallback. A caller-supplied
`CDRGAM_ROOT` takes precedence, so temporarily changing it switches the
whole harness instance—including configuration and registry—without
separating any of its state. Otherwise, the root defaults to R’s
platform-specific user data directory, such as
`~/.local/share/R/cdrgam.cli` on Linux. Projects live under the
configured root.

After installation from a package repository, run
[`install_cli()`](https://climblab-stanford.github.io/cdrgam.cli/reference/install_cli.md).
It creates the default root and its site configuration. Use
`cdrgam def edit -P site` to change scheduler settings. To move a root,
stop its workers, move the entire directory, and set `CDRGAM_ROOT` or
reinstall the launcher with the new path.

Use `./scripts/install --configure` to replace an existing root
configuration interactively.

Install the core and CLI development checkouts into an isolated library
and write a separate `cdrgam-dev` launcher with:

``` sh
./scripts/install-dev
```

The development launcher uses the root selected by `CDRGAM_ROOT`, or the
platform default when the variable is absent. Set `CDRGAM_CORE_SOURCE`
when the flattened core checkout is not its sibling. It does not replace
packages installed in the active R library or the stable `cdrgam`
launcher.

## Platform support

Local project definition, validation, fitting, prediction,
visualization, and serial orchestration use portable R APIs and are
intended to run on Linux, macOS, and Windows.
[`install_cli()`](https://climblab-stanford.github.io/cdrgam.cli/reference/install_cli.md)
writes a POSIX launcher on Unix and a `.cmd` launcher on Windows. Names
that conflict with Windows device files are rejected so a project root
can be moved between platforms.

Direct Slurm execution is a Unix feature. It requires `sh`, `sbatch`,
and the other configured Slurm commands. Subprocess execution, scheduler
query timeouts, process liveness checks, and root locks use
cross-platform R packages rather than platform shell utilities. Windows
users can omit the Slurm fields and use local execution. Local work
remains serial but each item runs in an isolated R process. Paging uses
`less` on Unix when installed and R’s configured file viewer on Windows.
Forked finite-gradient evaluation and Linux memory-limit detection
belong to the `cdrgam` core; unsupported acceleration paths fall back to
portable serial or lower-memory behavior.

``` sh
cdrgam def init brown
cdrgam def edit -P brown --dataset training
cdrgam def edit -P brown --model main
cdrgam def val -P brown --deep
cdrgam run -P brown -m main
```

Every command provides contextual help. Use `cdrgam --help` to list
public commands, `cdrgam COMMAND --help` for its options, or
`cdrgam help def edit` for a nested command. Internal scheduler and
worker commands are omitted from public help.

Project-scoped commands accept `-P/--project`. When it is omitted,
definition commands first infer the project from the current directory,
then look for a unique project matching the supplied definition
selector. Ambiguous matches, new definitions requested outside a
project, project initialization, and whole-project removal still require
an explicit project.

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
cdrgam def edit -P brown --model main-alternative --source main
```

Remove an unused definition explicitly with `def rm`:

``` sh
cdrgam def rm -P brown --model main-alternative
```

Removal is refused while another definition references the target or
while generated results exist. Remove matching results first when
requested:

``` sh
cdrgam purge -P brown -m main-alternative --force
```

`def rm` selects targets by filename and does not validate their
contents, so malformed definitions remain removable. It reads the
reference-bearing fields of relevant non-target definitions and enforces
reference, result, and active-work safeguards.

Validate an entire project or diagnose one definition without changing
it:

``` sh
cdrgam def val -P brown --deep
cdrgam def val -P brown --model main alternative
```

List all definitions, or combine selectors to list specific matches:

``` sh
cdrgam def ls
cdrgam def ls -P brown
cdrgam def ls -P brown --model
cdrgam def ls -P brown --dataset 'brown-*' --model 'main*'
cdrgam def ls -P brown --model 're:^main-(linear|nonlinear)$'
```

Bare `cdrgam def ls` lists the root’s available projects. When a
definition selector is supplied without `-P`, the current project or the
unique project containing a match is inferred. A selector without values
lists every definition of that type. Once any type selector is supplied,
unselected types are omitted.

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
names, `*` globs, and `re:`-prefixed regular expressions. Repeated
values within one selector are combined, while different selector
dimensions are conjunctive. When no project is selected, matching
workloads are combined across projects; each model pattern need not
occur in every project. If a prediction selector is not a model
partition, it must exactly match a dataset definition. Downstream
visualizations and comparisons request their fit and prediction
dependencies automatically.

Fits classified as `Warning` or `Nonconverged` include
`simplification-report.txt` and `simplification-report.yml` with ranked,
diagnostic suggestions. Reporting does not change the fitted model. Set
`autosimplify: true` in a model definition to permit eligible formula
patches and fresh refits until the fit is healthy or the configured step
limit is reached.

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

`cdrgam viz` serves a browser viewer for every completed fit under the
root. It draws effect lines, with several lines per plot drawn from any
fits, and two-axis effect surfaces. For each line or surface you choose
the term, the axes that vary, and fixed values for the remaining axes of
tensor terms. A reference can be subtracted from each line; an empty
reference field leaves that predictor at the model default, so terms
involving it drop out. With the Total composition, fixing `wlen` at one
value, leaving its reference empty, and giving `repetition_index` the
same reference as its fixed value gives the word-length effect at that
repetition. Axes that are not varied start at lag 0 and at their
training means. Fixed and reference fields accept numbers or expressions
such as `mean + 2 * sd`, and lag fields also accept `peak`, `peak-pos`,
and `peak-neg`, as in visualization definitions. Effects are on the
linear-predictor scale with pointwise or, under **Options**,
simultaneous intervals. References need the development `cdrgam`. The
viewer needs the optional R packages `shiny`, `bslib`, `plotly`,
`jsonlite`, and `later`.

On a Slurm-configured root the server runs in a Slurm job owned by the
calling user, and later `cdrgam viz` calls reuse it. The command prints
an `ssh` command that forwards the server’s port through a login host.
Set `CDRGAM_VIZ_GATEWAY` to the login host you reach from your
workstation, such as `user@login.example.edu`; otherwise the command
names the host it ran on. The job’s server listens on all of its node’s
interfaces without authentication, so anyone who can reach the node can
open the viewer. On other roots, or with `--local`, the server runs in
the foreground and listens only on loopback:

In a Slurm viewer, **Refresh data** rescans for completed fits and
reloads any fit whose results changed, and **Restart server** restarts
the server inside the same job so that newly installed code takes
effect, for example after `./scripts/install-dev`. Plotted lines are
kept across a restart in the browser tab.

``` sh
cdrgam viz
cdrgam viz --time 2-00:00:00 --memory 32G --restart
cdrgam viz --stop
```

See
[docs/project-schema.md](https://climblab-stanford.github.io/cdrgam.cli/docs/project-schema.md)
for the definition schema and
[IMPLEMENTATION_PLAN.md](https://climblab-stanford.github.io/cdrgam.cli/IMPLEMENTATION_PLAN.md)
for orchestration invariants and implementation structure.
