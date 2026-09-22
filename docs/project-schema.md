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
    work/
    workers/
```

The controller discovery document contains the short-lived scheduler TCP
endpoint and authentication token. It is created with user-only permissions
and removed when the scheduler exits. Scheduler and generic worker scripts and
logs remain as private execution records.

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

## Project layout

```text
projects/PROJECT/
  definitions/
    project.yml
    datasets/
    models/
    visualizations/
    comparisons/
    analyses/
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
    work/
```

Definitions are user-owned inputs. The other top-level directories contain
published artifacts and may be recreated. Attempts and checkpoints live under
the project's `.cdrgam/work` directory so that large temporary files share the
artifact filesystem. Checkout-wide request records, scheduler scripts, worker
scripts, and scheduler logs remain under the root-level `.cdrgam` directory.

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

`cdrgam def edit PROJECT` creates a missing project or edits its project definition.
Definition selectors create or edit subordinate definitions, for example
`cdrgam def edit PROJECT --dataset DATASET` and
`cdrgam def edit PROJECT --model MODEL`. Existing definitions are edited through a
temporary file and replace the published YAML only after validation succeeds.
If validation fails or the editor exits with an error, the edited content is
saved under the project's `.cdrgam/drafts/` directory. Running the same command
reopens that draft. Successful publication removes it. Site drafts use the
checkout's `.cdrgam/drafts/` directory.

`cdrgam def edit DESTINATION --source SOURCE` creates a new project ID and copies
the source project's complete `definitions/` tree. It does not copy
datasets, fitted models, predictions, visualizations, comparisons, analyses,
logs, or private orchestration state. Relative source and preprocessing paths
are preserved and may therefore need editing in the destination project.

With a definition selector, `--source` instead initializes a new definition
of the selected type in the target project. For example,
`cdrgam def edit brown --model alternative --source main` copies `main.yml` to
`alternative.yml` and changes its `model` field to `alternative`. The source
and target must belong to the same project and the target must not exist.

`cdrgam def del PROJECT --TYPE NAME` deletes one subordinate definition. It
does not delete site or project definitions, generated results, or referenced
definitions. If results exist, the diagnostic provides the matching
`cdrgam purge` command to run before retrying deletion. `cdrgam purge` never
selects files beneath `definitions/`.

`cdrgam def val PROJECT` validates the complete project. A definition selector
validates only that definition and its direct prerequisites, so an unrelated
broken definition does not prevent focused diagnosis. Pass `--deep` to read
referenced datasets and prepare model designs. `cdrgam def val site` validates
the checkout configuration.

Definition selectors accept multiple values after one option, or through a
repeated option. `def edit` treats each value as a literal definition name and
opens the files in order. `def val` and `def del` accept `*` patterns and apply
to every match. Deletion checks every match before removing any file.

## Dataset definition

```yaml
schema: 1
dataset: brown-train
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
model: main
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
```

`datasets.train` is required and selects the fitting dataset. Other keys are
model-internal prediction partitions. A prediction selector first resolves as
a partition key and then, if no key matches, as an exact dataset definition.
The resolved dataset name—not its partition alias—appears in the artifact path
and identity.

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

`gaulss` fits a native `mgcv` Gaussian location--scale model. Its formula is
a two-entry mapping; `location` supplies the response and `scale` may be
one-sided:

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
visualization: main-effects
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
visualization: diagnostic-booklet
model: main
kind: booklet
pages: 1
```

## Comparison definition

```yaml
schema: 1
comparison: alternatives
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

```sh
cdrgam run -P brown -m main -p val -p test
cdrgam run -P brown -m main -v main-effects
cdrgam run -P brown -c alternatives
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
or fail before a new identity can replace it. Private work logs follow the same
retention rule.

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
