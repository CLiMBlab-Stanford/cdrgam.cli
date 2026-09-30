# Project and checkout schema

## Checkout configuration

Installation creates `.cdrgam/checkout.yml` relative to a writable harness
instance directory. The source checkout is the default instance during
development, but an installed package may use any writable directory. The
launcher records the instance path, so separate instances do not share
configuration implicitly.

```yaml
schema: 1
cdrgam_root: /path/to/cdrgam-root
concurrency: 4
slurm_partition: sphinx
slurm_account: nlp
slurm_cpus: 4
slurm_memory: 64G
slurm_time: 1-00:00:00
```

`cdrgam_root` and `concurrency` are required. `slurm_partition` and
`slurm_account` must either both be present or both be absent. Their presence
selects Slurm execution. Their absence selects serial local execution. The
remaining Slurm fields are optional defaults and may be overridden by
`cdrgam run`.

Use `cdrgam def edit site` to edit this checkout-local file. The edited YAML is
validated before it atomically replaces the current configuration.

Local execution is supported on Linux, macOS, and Windows. Direct Slurm
execution generates POSIX shell scripts and is unavailable on Windows. The
Slurm fields may remain absent on platforms without Slurm.

The root contains all projects and private orchestration state:

```text
cdrgam-root/
  projects/
    PROJECT/
  .cdrgam/
    registry.sqlite3
    controller.yml
    scheduler.yml
    scheduler/
    logs/
      workers/
    work/
    workers/
```

The controller discovery document contains the short-lived scheduler TCP
endpoint and authentication token. It is created with user-only permissions
and removed when the scheduler exits. Scheduler scripts and logs, generic
worker scripts, and worker lifecycle logs remain as private execution records.

Project-owned references are stored relative to the project root and anchored
by the generated project ID. Checkout-wide scheduler and worker references are
stored relative to `cdrgam_root`. To move a root, first let the scheduler and
workers stop, move the complete directory, and then update `cdrgam_root` with
`cdrgam def edit site`.

An inactive project may be renamed by renaming its directory under `projects/`.
The directory name is the effective project name; the stable ID preserves work
identities and resolves stored artifact and attempt paths beneath the renamed
directory. Updating `project.name` to match is recommended for readability but
is not required for resolution. Dataset source and preprocessing paths that
point inside the root must be relative to their project. Absolute paths are
still accepted for external input data and remain the user's responsibility
when the root moves.

## Names

Project, dataset, partition, model, visualization, comparison, and analysis
names must match:

```text
^[a-z][a-z0-9-]*$
```

Underscores are reserved for generated compound labels.

Each subordinate definition is named exclusively by its filename stem. For
example, `definitions/models/main.yml` defines `main`; it does not contain a
redundant model identity field.

## Project layout

```text
projects/PROJECT/
  code/
    ...
  definitions/
    project.yml
    datasets/
    models/
    visualizations/
    comparisons/
    analyses/
  results/
    datasets/
      DATASET/
    models/
      MODEL/
        effects/
          EFFECT-IDENTITY/
        predictions/
          DATASET/
        visualizations/
          VISUALIZATION/
    comparisons/
      COMPARISON/
    analyses/
      ANALYSIS/
  .cdrgam/
    logs/
      KIND/
        WORKLOAD.log
    work/
```

`code/`, `definitions/`, and other nonignored project metadata are user-owned
project sources. The harness creates
`code/` as a place for analysis scripts but does not interpret, execute, or
purge its contents. Dataset definitions may refer to scripts there through
project-relative paths. The result directories contain published artifacts and
may be recreated. Attempts and checkpoints live under the project's
`.cdrgam/work` directory so that large temporary files share the artifact
filesystem. The complete `results/` tree is ignored by Git unless a user
explicitly selects Git result publication. Each logical work item has one
project-private process log that is
overwritten when that workload starts again. Checkout-wide request records,
scheduler scripts, worker scripts, scheduler logs, and one lifecycle log per
generic worker remain under the root-level `.cdrgam` directory.

## Project definition

```yaml
schema: 1
project:
  name: brown
  id: project-4e1c...
```

The generated ID remains stable if the project directory moves or is renamed
within its configured root. At runtime the directory name takes precedence
over the descriptive `project.name` value.

`cdrgam def init PROJECT` creates a project, initializes a Git repository on
branch `main`, and stages its initial sources. `cdrgam def edit PROJECT` creates
a missing project or edits its project definition.
Definition selectors create or edit subordinate definitions, for example
`cdrgam def edit PROJECT --dataset DATASET` and
`cdrgam def edit PROJECT --model MODEL`. Existing definitions are edited through a
temporary file and replace the published YAML only after validation succeeds.
If validation fails or the editor exits with an error, the edited content is
saved under the project's `.cdrgam/drafts/` directory. Running the same command
reopens that draft. Successful publication removes it. Site drafts use the
checkout's `.cdrgam/drafts/` directory.
Closing the editor without saving cancels the edit: no target is published and
no draft is created.

`cdrgam def init DESTINATION --source SOURCE` creates a new project ID and copies
the source project's complete `definitions/` and `code/` trees. It does not
copy datasets, fitted models, predictions, visualizations, comparisons,
analyses, logs, or private orchestration state. Relative source and
preprocessing paths are preserved and may therefore need editing in the
destination project.

With a definition selector, `--source` instead initializes a new definition
of the selected type in the target project. For example,
`cdrgam def edit brown --model alternative --source main` opens a staged copy
of `main.yml` in the editor and publishes it as `alternative.yml` after
validation. Staging copies the source bytes directly, preserving comments and
formatting unless the editor changes them. The source and target must belong
to the same project and the target must not exist.

The filename stem is the sole name of a dataset, model, visualization, or
comparison definition. For example, `definitions/models/main.yml` defines the
model named `main`; subordinate definitions must omit redundant `dataset`,
`model`, `visualization`, and `comparison` identity fields. A `model` field in
a visualization remains an upstream model reference, rather than the
visualization's own name.

`cdrgam def rm PROJECT --TYPE NAME` removes one subordinate definition. It
does not remove site or project definitions, generated results, or referenced
definitions. If results exist, the diagnostic provides the matching
`cdrgam purge` command to run before retrying removal. `cdrgam purge` never
selects files beneath `definitions/`. Removal selects targets by filename and
does not parse or validate their contents. It parses only the fields of other
definitions needed to identify references; those references still prevent
removal. An unreadable definition that could refer to the target also prevents
removal because dependency safety cannot be established.

Bare `cdrgam def ls` lists every available project in the checkout.
`cdrgam def ls PROJECT` lists the project definition and every subordinate
definition. Dataset, model, visualization, and comparison selectors accept
multiple names, `*` globs, and `re:`-prefixed Perl-compatible regular
expressions. Regular expressions use substring matching unless explicitly
anchored with `^` or `$`. Multiple selector types may be combined; the result
contains the matching definitions from each supplied type. Listing uses
definition filenames and does not require the YAML contents to validate. A
selector without values selects every definition of that type, as in `cdrgam
def ls brown --model`.

`cdrgam def val PROJECT` validates the complete project. A definition selector
validates only that definition and its direct prerequisites, so an unrelated
broken definition does not prevent focused diagnosis. Pass `--deep` to read
referenced datasets and prepare model designs. `cdrgam def val site` validates
the checkout configuration.

Definition selectors accept multiple values after one option, or through a
repeated option. `def edit` treats each value as a literal definition name and
opens the files in order. `def ls`, `def val`, and `def rm` accept `*` globs
and `re:` regular expressions and apply to every match. Removal checks every
match before removing any file.

## Dataset definition

```yaml
schema: 1
sources:
  impulses:
    path: data/impulses.csv
    format: csv
    separator: ","
    types:
      time: double
      subject: factor
      surprisal: double
  responses:
    path: data/responses.csv
    format: csv
    separator: ","
    types:
      time: double
      subject: factor
      reading-time: double
columns:
  series: [subject]
  impulse_time: time
  response_time: time
  row_id: response-row
  factors: [subject]
  factor_interactions:
    item_id: [document, sentence, position]
filters:
  - column: reading-time
    fun: ">"
    args: 100
  - factor: subject
    min: 101
```

RDS sources must contain data frames. For delimited sources, `types` may be
omitted or may declare only selected columns; undeclared columns use R's
native type inference. Supported declarations are `character`, `double`,
`integer`, `logical`, and `factor`. A separator must be one character.

Filters apply only to responses, in declaration order. A `column` filter calls
the named R function with the column followed by `args`. A `factor`/`min`
filter keeps values having at least `min` occurrences among rows retained so
far. Filtering predictors would change predictor histories and is not
supported.

`factor_interactions` creates factor columns from the attested combinations of
two or more source columns. Construction occurs after response filtering, uses
`interaction(..., drop=TRUE)`, and applies independently to every stream that
contains all listed source columns. A stream containing only some of the listed
columns is invalid. This supports compact random-effect terms such as
`s(item_id, bs="re")` without retaining a full Cartesian product of source
factor levels.

An optional preprocessing hook names trusted project code:

```yaml
preprocess:
  script: scripts/prepare-streams.R
  function: prepare_streams
```

It receives impulse and response data frames and must return a list containing
data frames named `impulses` and `responses`. Script content contributes to the
dataset identity.

## Model definition

```yaml
schema: 1
datasets:
  train: brown-train
  val: brown-val
  test: brown-test
window: [0, 2]
knots_l: [0, 0.025, 0.05, 0.1, 0.2, 0.4, 0.75, 1.2, 1.6, 2]
k_l: 10
k_t: null
k_p: 5
bs_l: cr
bs_t: cr
bs_p: cr
formula: >
  reading-time ~ irf(surprisal)
fit:
  family: gaussian
  link: identity
  backend: sparse
  rescale_predictors: true
autosimplify:
  enabled: false
  max_steps: 5
  conservatism: 1
```

`datasets.train` is required and selects the fitting dataset. Other keys are
model-internal prediction partitions. A prediction selector first resolves as
a partition key and then, if no key matches, as an exact dataset definition.
The resolved dataset name—not its partition alias—appears in the artifact path
and identity.

Formula values may use YAML block syntax for readability. The harness parses
and deparses each formula into a single canonical string in the resolved model
definition, so line wrapping and indentation do not affect artifact identity.
The YAML file itself retains the user's formatting.

`window` is optional and supplies the default lag window for every IRF in the
model. Every impulse in the applicable series and window contributes to the
response-level design. An individual `irf()` can still override the window.
The top-level `k_l`, `k_t`, and `k_p` fields similarly provide defaults for
lag, response-time, and predictor basis dimensions; `bs_l`, `bs_t`, and `bs_p`
provide their basis defaults. Arguments supplied by an individual `irf()`
override them, including an explicit `NULL`. A scalar `k_p` or `bs_p` is
recycled across a term's predictors. A `NULL` predictor dimension is linear;
use an R list such as
`k_p=list(NULL, 4)` to mix linear and smooth predictors.

`knots_l` optionally supplies exactly `k_l` strictly increasing lag-basis
construction points in the original lag units. They must lie inside `window`
and span the linked training lags. Concentrating interior points in a narrow
interval increases representational resolution there without increasing
`k_l`. Individual `irf()` terms may override the model-level value.
`knots_l` is unavailable with `bs_l: ps`, whose knot contract differs.

Settings omitted from `fit` follow the installed core defaults. Completed fit
manifests record both requested and effective settings.

`autosimplify` is optional and disabled when omitted. Set it to `true`, or to
a mapping with `enabled: true`, to let a nonconverged fit be diagnosed,
simplified, and refitted. The core ranks candidates by convergence evidence
and discounts edits by the estimated fitted degrees of freedom they remove;
larger `conservatism` values favor narrower edits more strongly. `max_steps`
limits fit attempts, including the initial requested model. Optional `allow`
and `protect` lists restrict the accepted action names and exact reported term
names. Supported actions are `drop_grouped_deviation`, `drop_random_effect`,
`drop_term`, and `intercept_only_parameter`.

Each formula change starts with a fresh optimizer state. A converged terminal
fit alone is published at the model's standard artifact path. Nonconverged
intermediate fits remain under `.cdrgam/autosimplify/steps` in that artifact,
and `derived-config.yml` records the formula actually fitted. The private
chain records the original definition contract and every exact formula
replacement. If no eligible edit yields convergence within `max_steps`, the
work item fails and no nonconverged model is published at the standard path.

`fit.family` accepts `gaussian`, `binomial`, `poisson`, `Gamma`, or `gaulss`
through
the core package's validated family registry. `fit.link` is optional and
selects an explicit link supported by that family; it requires `fit.family`.
Noncanonical-link models currently require the native `mgcv` backend. The
`block` and `sparse` backends also support `binomial(logit)`, `poisson(log)`,
and estimated-dispersion `Gamma(log)`; generalized sparse fitting uses
streamed PIRLS and an exact Laplace score. Prediction artifacts store
response-scale predictions (used for residuals and comparison metrics)
alongside separately named link-scale predictions.

`gaulss` fits a Gaussian location--scale model through native `mgcv`, the
dense `block` reference backend, or the streamed `sparse` backend. The sparse
solver uses an exact outer smoothing-parameter score. Its formula is a
two-entry mapping; `location` supplies the
response and `scale` may be one-sided:

```yaml
formula:
  location: reading-time ~ irf(surprisal)
  scale: ~ irf(word-length)
fit:
  family: gaulss
  backend: mgcv
```

Distributional predictions retain the ordinary `prediction` and `residual`
columns for the location parameter and add response- and link-scale estimate
and standard-error columns for both `location` and `scale`. Following mgcv,
the response-scale `scale_prediction` is reciprocal standard deviation; the
artifact also includes the derived `standard_deviation_prediction` and its
delta-method standard error. Predictor rescaling is not yet available for
distributional formulas.

## Visualization definition

```yaml
schema: 1
model: main
query:
  terms:
    predictors: "*"
    grouped: false
  composition: total
  grouping: population
  axes:
    lag: {grid: fitted, n: 300}
    predictors:
      "*":
        at: {summary: mean, offset-sd: 1}
  uncertainty: {kind: pointwise, level: 0.95}
render:
  geometry: line
  mappings: {x: lag, y: estimate, color: term}
  interval: ribbon
  theme: paper
  formats: [pdf, png]
```

A visualization separates statistical evaluation in `query` from ggplot2
presentation in `render`. The scheduler inserts an internal effect-grid work
item between the fit and the rendered visualization. Identical queries share
that work item even when their render settings differ. The effect-grid RDS and
CSV live under `models/MODEL/effects/EFFECT-IDENTITY/`; rendered files live
under `models/MODEL/visualizations/VISUALIZATION/`.

`terms` may be a list of exact fitted term labels or a selector mapping. A
selector accepts `names`, `predictors`, `match` (`contains` or `exact`), and
`grouped`. The predictor value `"*"` selects every nonconstant IRF. A request
that matches no terms fails before rendering.

`composition` controls the effect being evaluated. `term` returns only the
selected term. `total` adds its fitted marginal hierarchy, using the joint
coefficient covariance for uncertainty. `deviation` retains the selected term
without its marginals. `grouping` separately selects the population IRF,
group-specific deviation, or their conditional sum. `groups` can restrict the
factor levels used by `deviation` and `conditional` requests.

Each lag, time, or predictor axis accepts explicit numeric values or one of:

```yaml
axes:
  lag: {grid: fitted, n: 200}
  time: {values: [100, 200, 300]}
  predictors:
    surprisal: {quantiles: [0.1, 0.5, 0.9]}
    frequency:
      at: {summary: mean, offset-sd: 1}
```

Unspecified lag axes vary over their fitted domains. Other unspecified axes
are fixed at their training median. The wildcard predictor entry supplies a
fallback for every predictor axis. Axis values and effect-grid columns use
source data units.

Line renderings support ribbon, boundary-line, or no interval display. Raster,
contour, and combined raster-contour geometries support predictor-by-lag,
time-by-lag, and predictor-by-predictor surfaces. For example:

```yaml
render:
  geometry: raster-contour
  mappings: {x: lag, y: "predictor:surprisal", fill: estimate, facet: term}
  interval: companion
  formats: [pdf]
```

`predictor:` is an optional disambiguating prefix in mappings. A `companion`
interval produces a second surface showing pointwise standard errors. Mapping
fields are `x`, `y`, `color`, `fill`, `linetype`, `group`, and `facet`; themes are
`paper`, `minimal`, and `slides`.

Multiple layers can superimpose related estimands without repeating the axis
query. Each layer has an `id` and may override `composition`, `grouping`,
`groups`, and interval or style settings:

```yaml
layers:
  - id: subjects
    grouping: conditional
    interval: none
    style: {color: grey70, alpha: 0.2, linewidth: 0.3}
  - id: population
    grouping: population
    style: {color: black, linewidth: 1.0}
```

For this overlay, select the corresponding grouped term with
`terms: {predictors: ..., grouped: true}`. The population layer resolves its
ungrouped counterpart, and the conditional layer adds each requested
deviation. The renderer keeps term, group, and layer series separate unless an
explicit `group` mapping is supplied.

The legacy default model booklet remains available explicitly:

```yaml
schema: 1
model: main
kind: booklet
pages: 1
```

## Comparison definition

```yaml
schema: 1
models: [baseline, main]
evaluation:
  dataset: test
  row_id: response-row
methods: [mse, mae]
```

The evaluation selector is resolved independently through each model's dataset
mapping. Every model must resolve it to the same dataset. Comparisons depend on
the corresponding prediction artifacts.

## Selection and dependencies

`run` and `plan` accept project, model, prediction, visualization, and
comparison selectors. Supplied dimensions are conjunctive. Repeating
`--prediction` requests multiple partitions of the same selected model.
Project, model, visualization, and comparison selectors accept exact names,
`*` globs, and `re:`-prefixed Perl-compatible regular expressions. Regexes use
substring matching unless anchored. Prediction selectors remain exact because
each value resolves first as a model-local partition and then as a dataset
definition.

```sh
cdrgam run -P brown -m main -p val -p test
cdrgam run -P brown -m main -v main-effects
cdrgam run -P brown -c alternatives
cdrgam run -P val -m 're:^natstor-(raw|log)-l[01]s[01]h[01]$'
```

Downstream work includes its complete dependency closure. Explicit ancestors
are removed when an explicitly selected descendant already requires them, so
requesting a model and its visualization does not duplicate work. Repeated
requests reuse complete artifacts with matching resolved identities and active
work with matching work identities.

With no selectors, `run` requests all configured terminal work. Non-training
entries in model dataset mappings form the finite ordinary prediction set.
Arbitrary dataset predictions are created only by an explicit selector or a
downstream definition.

## Manifests and private state

Every complete artifact contains `manifest.yml`. It records the resolved
scientific identity, immediate input identities, normalized configuration,
software fingerprints, execution environment, timestamps, diagnostics, and
output checksums. Reuse requires both identity equality and valid checksums.

The registry retains one current identity and one last-run attempt for each
logical workload. Replacing an identity removes its inactive attempt and any
registered downstream work that depended on it. An active workload must finish
or fail before a new identity can replace it. Each logical workload has one
process log, regardless of identity or attempt count; a restart truncates that
log before execution. Each generic worker has one lifecycle log containing its
timestamped sequence of claimed, completed, and failed work items. `cdrgam log`
shows work-item logs, including live logs, while `cdrgam log --worker` shows
worker lifecycle logs.

In local mode the request graph executes serially, with each work item in an
isolated R process. In Slurm mode an ephemeral `cdrgam-scheduler` job is the
only parallel writer to the SQLite registry and enforces the checkout-wide
concurrency limit across projects. It launches generic `cdrgam-worker` jobs
that claim ready work over TCP. A worker may run multiple compatible items,
even across projects. Workers validate the recorded R and package environment
before loading inputs. Short checkout-level locks serialize configuration and
scheduler publication; SQLite manages registry locking.

If a worker disappears while it owns an item, that item becomes failed and its
dependants become blocked. The scheduler does not retry terminal failures; a
new `cdrgam run` request is required. A temporary failure to query Slurm leaves
worker state unchanged.

## Publication and fetching

Every initialized project is a Git repository. Successful `def edit`, `def rm`,
and project-copy operations stage the source paths they change. Invalid drafts
are private and remain unstaged. The harness never commits or pushes
implicitly.

`cdrgam publish PROJECT` validates sources, writes the tracked
`publication.yml`, and stages it. Result modes are:

- `none`, which publishes project sources without results;
- `archive`, which writes a verified `.tar.gz` containing selected artifacts,
  their dependency closure, checksums, contracts, and a source snapshot;
- `url`, which records a direct archive URL and required SHA-256 checksum; and
- `git`, which force-stages selected results despite the normal ignore rule.

Archive and Git modes reject incomplete, stale, and nonconverged selected
artifacts. `--commit MESSAGE` commits all changes already staged in the project
repository, including definition edits. `--push` runs Git push only when it is
explicitly supplied together with `--commit`. Uploading a generated archive to a data repository is a
separate operation; rerun publication in URL mode after obtaining its direct
download URL.

`cdrgam fetch LOCATION` accepts a Git repository or a direct project source
archive. It validates the stable project ID and publication metadata. When a
direct results URL is present, or `--results LOCATION` supplies an override,
the command downloads the archive, verifies its SHA-256 and per-file inventory,
and installs it under `results/`. The completed project is moved into the local
root atomically, and the checkout registry is reconstructed from complete
artifact manifests. `--source-only` skips result hydration.

Git revisions are provenance rather than freshness inputs. Result publications
record the project revision and dirty state present when the archive was made,
while each artifact retains the core and harness implementation information
recorded at execution. Artifact contracts and checksums determine whether a
fetched result is current for the fetched definitions.
