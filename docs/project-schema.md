# Project and site schema

## Root and site configuration

Installation creates `.cdrgam/site.yml` inside the CDR-GAM root. The default
root is `tools::R_user_dir("cdrgam.cli", "data")`, such as
`~/.local/share/R/cdrgam.cli` on Linux. The launcher records this as a fallback;
a nonempty `CDRGAM_ROOT` environment variable selects another complete root for
that invocation. Because configuration, registry, scheduler state, and projects
move together, selecting a root cannot silently pair data with configuration
from another harness instance.

```yaml
schema: 1
concurrency: 4
slurm_partition: sphinx
slurm_account: nlp
slurm_cpus: 4
slurm_memory: 64G
slurm_time: 1-00:00:00
```

`concurrency` is required. `slurm_partition` and
`slurm_account` must either both be present or both be absent. Their presence
selects Slurm execution. Their absence selects serial local execution. The
remaining Slurm fields are optional defaults and may be overridden by
`cdrgam run`.

Use `cdrgam def edit site` to edit this root-local file. The edited YAML is
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
by the generated project ID. Root-wide scheduler and worker references are
stored relative to `cdrgam_root`. To move a root, first let the scheduler and
workers stop, move the complete directory, and then select its new location
through `CDRGAM_ROOT` or reinstall the launcher with that root as its fallback.

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
    tests/
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
    tests/
      TEST/
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
root's `.cdrgam/drafts/` directory.
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

The filename stem is the sole name of a dataset, model, visualization,
comparison, or test definition. For example, `definitions/models/main.yml` defines the
model named `main`; subordinate definitions must omit redundant `dataset`,
`model`, `visualization`, `comparison`, and `test` identity fields. A `model`
field in a visualization or test remains an upstream model reference, rather
than the definition's own name.

`cdrgam def rm PROJECT --TYPE NAME` removes one subordinate definition. It
does not remove site or project definitions, generated results, or referenced
definitions. If results exist, the diagnostic provides the matching
`cdrgam purge` command to run before retrying removal. `cdrgam purge` never
selects files beneath `definitions/`. Removal selects targets by filename and
does not parse or validate their contents. It parses only the fields of other
definitions needed to identify references; those references still prevent
removal. An unreadable definition that could refer to the target also prevents
removal because dependency safety cannot be established.

Bare `cdrgam def ls` lists every available project in the root.
`cdrgam def ls PROJECT` lists the project definition and every subordinate
definition. Dataset, model, visualization, comparison, and test selectors accept
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
the root-local site configuration.

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

`fit.rescale_predictors` defaults to `true` in the core package: continuous
predictors and lag coordinates are divided by their training standard
deviations, and effects are still reported in native units. Set it to `false`
to fit in native units.

`fit.center_predictors` defaults to `true` in the core package: linear IRF
predictors, including binary ones, are centered at their training means, so
`irf(1)` is the response to an event with average linear predictors. Set it to
`false` for uncentered predictors. Individual terms can override it in the
formula with `irf(..., center=)`.

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
presentation in `render`. A visualization evaluates its query from the fit
each time it renders; effect grids are not cached between runs. The plotted
numbers are written beside the figure: `effect-grid.csv`, with
`effect-peaks.csv`, `effect-peak-draws.csv`, `effect-pointwise-peaks.csv`,
and `effect-tests.csv` when the query produces them. A visualization with
`lines` writes the same files for each line, named `line-ID-grid.csv` and so
on. Rendered
files live under `visualizations/VISUALIZATION/`, beside `models/`, because
a visualization may draw lines from several models.

`terms` may be a list of exact fitted term labels or a selector mapping. A
selector accepts `names`, `predictors`, `match` (`contains` or `exact`),
`grouped`, and `interacting`. The predictor value `"*"` selects every
nonconstant IRF. `interacting: false` keeps terms with at most one predictor,
including `irf(1)` unless `predictors` excludes it, and `interacting: true`
keeps the rest. A request that matches no terms fails before rendering.

`composition` controls the effect being evaluated. `term` returns only the
selected term. `total` adds its fitted marginal hierarchy, using the joint
coefficient covariance for uncertainty. The hierarchy contains every ungrouped
term whose predictors are a subset of the selected term's, so the total for
`irf(wlen, repetition_index, k_p=list(NULL, 3))` adds `irf(wlen)`,
`irf(repetition_index, k_p=3)`, and `irf(1)`. The total is therefore the
response to an event with the requested predictor values. To isolate one
predictor's effect, subtract a `reference` in that predictor: terms that do
not involve it cancel. `grouping` separately selects the population IRF,
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
are fixed at their training mean. The wildcard predictor entry supplies a
fallback for every predictor axis. Axis values and effect-grid columns use
source data units.

Wherever an axis or reference takes a number, it also takes an expression in
that axis's training `mean`, `sd`, and `median`, using numbers, `+`, `-`,
`*`, `/`, and parentheses. A bare expression fixes the axis at one value.
`range` gives a grid between two values, with `n` points or the query's `n`:

```yaml
axes:
  lag: peak
  predictors:
    surprisal: mean + 2 * sd
    frequency: {values: [mean - sd, mean, mean + sd]}
    "*": {range: [mean - 2 * sd, mean + 2 * sd], n: 50}
reference:
  "*": mean
```

Lag values may also use `peak` or `peak-pos`, the lag at which a term's
response is largest, and `peak-neg`, the lag at which it is smallest, over
the fitted lag domain. The response used is the effect, under the layer's
`composition`, with each of the term's predictors at the mean plus one SD,
minus the effect at the means, and with other axes at their training means.
Under `term` it is the term alone, and for `irf(1)` it is `irf(1)` itself.
Under `total` the lower-order terms count too, so an interaction's peak is
that of the total response its lines show.
Expressions are resolved separately for each selected term, so `"*"` entries
use each predictor's own summaries and each term's own peak. To hold every
term at the same lag, name the term whose peak to use: `peak(irf(1))`,
`peak-neg(s(lag, wlen))`, or `peak("irf(1)") + 0.1`. Quotes around the label
are optional. The label is checked against the fit at evaluation, and the
named term must have a lag axis. A `"*"`
reference applies to every predictor of a term that has no reference of its
own; a term without predictors, such as `irf(1)`, gets no reference.
Expressions fail at evaluation when the fit stores no summary for the axis.

`lag: peak-pointwise` instead holds each evaluation point at its own lag: the
lag where the evaluated effect, after the `reference` subtraction and under the
layer's `composition`, is largest at that point's other axis values.
`peak-pointwise-neg` takes the smallest. Without a reference, the effect itself
is used. Unlike `peak`, the lag can change along a line, and it follows the
query's own fixed and reference values rather than a one-SD contrast. The peak
at each point is found on a lag grid of `n` points; estimates and intervals are
then evaluated at each point's (value, lag) pair. Either keyword must be the
whole lag value, as a string or as `at`. It cannot appear in an expression, in
a range or list of values, or in a reference, and it requires
`uncertainty.kind: pointwise` and no `test`. It needs a cdrgam release whose
`estimate_effect()` accepts `points`.

`peak-pointwise(LABEL)` and `peak-pointwise-neg(LABEL)` take each point's lag
from another term instead. For each plotted point, the named term is evaluated
over lag at that point's values of its own predictors, with the query's
`reference` for those predictors, and the point is held at that term's peak
lag. Plotting `surp:rep` with `lag: peak-pointwise(surp)` reads the
interaction at the lag where the `surp` main effect peaks for each `surp`
value; points that share a `surp` value share a lag. The named term must have
a lag axis and at least one predictor, and its predictors must all be
predictors of the plotted term. Peak intervals come from posterior draws of
the named term. Under `uncertainty.kind: simultaneous`, both terms use the same
coefficient draws: each draw re-finds the named term's peak lags and evaluates
the plotted term at them, and the band covers those draws jointly. This needs
a cdrgam release with `sample_effect_coefficients()`.

`reference` subtracts the same effect evaluated at reference values of some
axes. Axes not named in `reference` keep their evaluated values. Each entry
uses the axis-request syntax and must resolve to one value, or is `default`
for a predictor. A `default` predictor stays at the model default: every term
that involves it drops out of the reference. For a centered linear predictor
this equals its center; for a smooth predictor it is the training-average
contribution implied by the term's identifiability constraint. With every
predictor at `default` and `composition: total`, the reference is `irf(1)`,
the response to an average event. Reference axes add an `AXIS_reference`
column to the effect grid, which is empty for a default, and the intervals
account for the covariance of the two evaluations. This query plots the
effect of a one-SD increase in word length at each repetition:

```yaml
query:
  terms: [wlen:repetition_index]
  composition: total
  axes:
    lag: {grid: fitted, n: 300}
    predictors:
      wlen: {at: {summary: mean, offset-sd: 1}}
      repetition_index: {values: [1, 2, 3, 4, 5]}
  reference:
    wlen: {at: {summary: mean}}
  test: true
render:
  geometry: line
  mappings: {x: lag, y: estimate, color: "predictor:repetition_index"}
  interval: ribbon
```

`wlen: default` gives the same result here because word length is a centered
linear predictor. A reference in `repetition_index` instead, such as
`repetition_index: 1`, plots each repetition's difference from the first. With `test: true`, the effect
work item also writes `effect-tests.csv`. It holds one joint Wald test per
term, group, and layer of the null hypothesis that the effect is zero at every
grid point. The test uses the same grid, so the grid should be dense enough to
span the effect.

`uncertainty.kind` is `pointwise` or `simultaneous`. Simultaneous limits
cover every grid point of a term and group jointly at `level`. The harness
estimates their covariance from the core's standard errors of differences
between grid points, on up to `uncertainty.points` grid points (default 40)
spread evenly along each varying axis, and takes the critical value of the
largest absolute t statistic from 10,000 simulated draws with a fixed seed.
It reports this value in a `critical_value` column. The cost is one extra
core evaluation per covariance point and reference value, for example 81 for
a 40-point lag curve with a predictor reference. Reference predictors must
have values; `default` is not supported.

When a value of the query uses a peak, such as `lag: peak`,
`lag: peak-pointwise`, or a reference at a peak, simultaneous limits also
cover the uncertainty about where the peak lies. They then come from
`uncertainty.peak_draws` posterior draws (default 1000, with a fixed seed).
Each draw re-locates the peaks on the same coefficient draw and evaluates the
effect at the values those peaks give. The band is the estimate plus or minus
`critical_value` times each point's draw standard deviation. `critical_value`
is the `level` quantile over draws of the largest absolute standardized
deviation within the term and group. Under `peak-pointwise`, each draw takes
the effect at its own peak for each point. Pointwise limits treat peak lags as
known. Simultaneous limits at a peak need a `cdrgam` release with
`sample_effect_coefficients()`.

With `peak-pointwise`, the effect artifact has `effect-pointwise-peaks.csv`
(and `.rds`), with one row per evaluated point: its term, layer, group, axis
values, direction, peak `lag`, and the central `level` interval of the peak
lags of `uncertainty.peak_draws` posterior draws of the search grid.

When any value uses a peak, the effect artifact also has `effect-peaks.csv`
(and `.rds`). It has one row per term, peak term, and direction, with the
peak lag in `lag`. `lower` and `upper` give the central `level` interval of
posterior draws of that peak lag. Each draw samples the coefficients from
their approximate posterior and takes the peak of the resulting curve on the
same lag grid, so draws are no finer than the grid spacing. The draws are in
`effect-peak-draws.csv`. `uncertainty.peak_draws` sets their number (default
1000, with a fixed seed); `0` skips them. The interval is empty when the
installed `cdrgam` predates `simulate_effect()`. A flat or bimodal response
can give draws in separate clusters, which the interval alone does not show.

Each term with exactly one predictor also has `predictor` and
`predictor_value` columns in the effect grid. They hold the predictor's name
and value, so terms with different predictors can share an axis.

Line renderings draw one curve per term, group, layer, and value of every
other varying axis. A numeric `color` or `linetype` mapping with at most 12
values uses a discrete legend. `scales` (`fixed`, `free`, `free_x`, or
`free_y`) sets the facet scales.

`palette` colors the `color` mapping of line, point, and bar renderings, and
the bar fill. It accepts `default` (ggplot2's hue palette; `hue` is an alias),
`rainbow`, `grey`, `okabe-ito`, a viridis option (`magma`, `inferno`,
`plasma`, `viridis`, `cividis`, `rocket`, `mako`, or `turbo`), or a
ColorBrewer palette such as `Set1`, `Dark2`, or `RdBu`. Names are matched
without regard to case. `rainbow` colors lines from red to violet and draws
`irf(1)` in black. `okabe-ito` gives the viewer's line colors: the
colorblind-safe Okabe-Ito colors except yellow, then grey and black, eight
in all. `okabe-ito` or a ColorBrewer palette with fewer colors than the plot
has color levels fails at rendering; numeric color mappings use the palette's continuous form.

`palette` may instead list two or more colors, each as hex (`#RRGGBB` or
`#RGB`), `rgb(R, G, B)` with channels from 0 to 255, or an R color name. The
plot then colors its levels at equal steps along the listed colors, from the
first to the last, and a numeric mapping uses the same colors as a continuous
gradient. Colors are stored in hex, so spellings of the same color give the
same visualization identity:

```yaml
render:
  geometry: line
  mappings: {x: lag, y: estimate, color: "predictor:frequency"}
  palette: ["#2166AC", "rgb(178, 24, 43)"]
```

`geometry: point` draws each estimate as a point with an error bar, and
`geometry: bar` as a bar from zero with an error bar. `geometry:
connected-point` draws points as `point` does and joins each series' points
in x order with a thin line, which has no interval band of its own. Their
`interval` is
`errorbar` (default) or `none`. Series that share an x position, such as
several terms at one lag, are placed side by side. Bars suit estimates at a
few values: an x mapping to a categorical column such as `term` or `group`,
including an effect where no axis varies, or a numeric axis requested with
`values`. For example, the effect of each term at its peak lag:

```yaml
query:
  terms: {predictors: "*", interacting: false}
  axes:
    lag: peak
    predictors: {"*": mean + sd}
  reference: {"*": mean}
render:
  geometry: bar
  mappings: {x: term, y: estimate, color: term}
  palette: Dark2
```

`spacing` draws a numeric x at its evaluated values, with one tick per
value. `uniform` places the ticks equally far apart in sorted order and
`linear` in proportion to their values. Bars over a numeric x use `uniform`
unless `spacing` is set; lines and points otherwise keep a continuous x-axis.
`spacing` has no effect on a categorical x and is not accepted for surfaces.

`breaks` lists the x values to label on a continuous axis, such as
`breaks: [0, 5, 10]`. It changes only the ticks, not where the effect is
evaluated, and cannot be combined with `spacing: uniform`. The viewer's **X
tick values** option sets it for lines. For points and bars, the viewer
evaluates the effect at those values instead of the grid and hides **Grid
points per axis**.

`tick_labels` replaces the tick labels with text. With `breaks`, it gives one
label per break, in the order the breaks are written:

```yaml
render:
  geometry: line
  breaks: [0, 0.25, 0.5]
  tick_labels: [onset, 250 ms, 500 ms]
```

Without `breaks`, it labels each distinct x value in increasing order, and
the number of labels must match the number of values. An x-axis that would
otherwise be continuous then gets a tick at each value. An empty label
leaves its tick unlabelled. In the viewer, **X tick labels** appears once X
tick values are typed and takes one label per value, in the order typed,
separated by commas.

`statistic: peak-lag` plots the peak lags that the query used instead of the
effect estimates, with their posterior intervals as error bars. It requires
point, connected-point, or bar geometry and a query whose lag values use a
peak. The plotted
rows have the columns `term`, `source`, `direction`, `peak` (the label the
peak timeline uses), `estimate` (the peak lag), `lower`, and `upper`:

```yaml
render:
  geometry: bar
  mappings: {x: peak, y: estimate, color: term}
  statistic: peak-lag
```

`peaks` controls a timeline of peak lags drawn below a line or point plot: `auto`
(default) draws it when the terms were held at different peak lags, `always`
whenever a peak was used, and `never` not at all. Each row shows one term's
peak lag as a long tick, its posterior interval as a bar, and the draws as
faint ticks, in the term's line color when `color` maps to `term`. The
timeline shares the plot's x-axis when that axis is `lag` and the plot has
no facets and uniform spacing is not set; otherwise it has its own lag
axis. The viewer draws the same
timeline under its line plots.

When the query uses `peak-pointwise`, a line or point plot also gets a panel
below the main plot with each line's peak lag over the main x-axis and its
interval band, in the line's color. `peaks: never` removes it. The panel is
not drawn for faceted plots or a categorical x-axis. The viewer draws the same
panel, saved separately as `plot-pointwise.rds` in rendered visualizations.

The `cdrgam viz` viewer draws from the same plot description as rendered
files, so a render setting looks the same in both. Its **Appearance** panel
has a control for `geometry`, `interval`, `theme`, `title`, `xlab`, `ylab`,
`palette`, `peaks`, `spacing`, and `statistic`, checked as in a definition.
The viewer sets `mappings` itself: x is each line's horizontal axis and
color is the line. It draws no facets, so `scales` does not apply, and
`formats`, `width`, `height`, `panel_width`, `panel_height`, and `dpi`
affect only rendered files. The
viewer's palette defaults to `okabe-ito` and switches every line to
`viridis` when there are more than eight; rendered files use `default` and
fail past a palette's last color. Its 3D surface has no render geometry.
**Show render YAML** prints the panel's settings as a `render` block; add
`mappings` for the definition's query before using it.

Line renderings support ribbon, boundary-line, or no interval display.
Layer `interval` settings must suit the geometry: `errorbar` or `none` for
points, connected points, and bars, and `ribbon`, `lines`, or `none` for lines. Raster,
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
fields are `x`, `y`, `color`, `fill`, `linetype`, `group`, and `facet`. Themes are
`paper` (ggplot2's bw), `slides` (minimal at a larger base size), and the native
ggplot2 themes `grey`, `gray`, `bw`, `linedraw`, `light`, `dark`, `minimal`,
`classic`, `void`, and `test`. `y_zero: true` extends the vertical axis of a
line, point, or bar plot to include zero. `reverse_palette: true` reads the
palette from its last color: a palette with a fixed list of colors, such as
`okabe-ito` or `Set1`, gives the first level its last color, the next level
the color before it, and so on. Palettes that spread over the levels, such as
viridis or a gradient, and continuous color scales are reversed end to end.
The viewer's **Reverse palette order** box sets it.

`zero_line` draws a thin grey line at zero effect beneath the data, in each
panel whose vertical axis includes zero: the range of `ylim` when it is set,
and otherwise that of the panel's estimates and intervals with the usual 5%
padding. It never extends an
axis to reach zero; use `y_zero` for that. It is on unless set to `false`, and
bars draw their own baseline instead. The viewer's **Line at zero** box sets
it.

`width` and `height` set the size of a rendered file in inches, 8 by 5 by
default; the plot panel takes whatever the axes, titles, and legend leave.
`panel_width` and `panel_height` instead fix the size of the plot panel, so
figures with legends of different sizes keep identical panels:

```yaml
render:
  geometry: line
  panel_width: 3
  panel_height: 3
```

The file is then sized to fit the panel with its axes, titles, and legend.
Each facet panel takes the given size, and a peak timeline or pointwise-peak
panel below the plot keeps the panel's width and is aligned with it. The two
are set together and cannot be combined with `width` or `height`. They need
ggplot2 4.0.0 or later.

`ylim: [low, high]` sets the vertical display range of a line, point, or bar
plot; `null` keeps that end automatic, as in `ylim: [null, 0.5]`. It only
zooms the view: estimates and intervals outside it are drawn to the edge, not
removed, and nothing is evaluated differently. To change where an effect is
evaluated instead, give the varying axis a range in the query, such as
`axes: {predictors: {surprisal: {range: [mean - 2 * sd, mean + 2 * sd]}}}` or
`axes: {lag: {range: [0, 0.5]}}`. In the viewer, **Vertical axis range** under
**Appearance** sets `ylim`, and **X range** under **Options** sets each line's
evaluation range, which the exported definition YAML records.

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

### Lines

A visualization can instead list `lines`, each one effect of one term with
its own model, axes, reference, composition, and grouping, drawn on one
plot. This is the form the viewer exports under **Definition YAML**, and
`cdrgam run` renders it with the figure the viewer draws:

```yaml
schema: 1
model: main
query:
  composition: total
  n: 200
  axes:
    lag: peak
    predictors: {z: mean}
  uncertainty: {kind: simultaneous}
lines:
  - term: x
    axes:
      predictors: {x: {grid: fitted}}
  - id: interaction
    term: "x:z"
    label: x by z at z = 1
    axes:
      predictors: {x: {grid: fitted}, z: 1}
    reference: {z: 0}
  - term: y
    model: other
    axes:
      lag: {grid: fitted}
render:
  geometry: line
  palette: okabe-ito
```

`query` here holds defaults that every line inherits; it takes the query
fields other than `terms`. A line overrides any of them. `axes` and
`reference` merge axis by axis (`axes.predictors` predictor by predictor) and
`uncertainty` setting by setting, so a line can change one axis and keep
the rest. Other fields replace the default. Each line needs `term`, the
exact label of one term. `model` defaults to the definition's, and outputs
live under the definition's model. `id` defaults to `line1`, `line2`, and so
on; `label`, the legend entry, defaults to the term, with the model when it
differs from the definition's.

Each line's query must vary exactly one axis, its horizontal axis. Lines over
different axes share the horizontal axis, titled with their axis names
unless `xlab` is set. `render.mappings` defaults to that axis, the estimate,
and color by line; mappings may also use the `line` column (the line id) and
`.line_x` (the horizontal value). Line, point, and bar geometries are
supported. `layers` do not apply, since each line sets its own composition
and grouping. Each line is its own effect work item, so a line that two
visualizations share is evaluated once. As in the viewer, the peak timeline
appears whenever a line uses a peak, unless `render.peaks` is `never`.

Two presets give common CDR plots without writing a query:

```yaml
schema: 1
model: main
preset: irf-lag
options:
  irf1: true
```

```yaml
schema: 1
model: main
preset: irf-predictor
options:
  hold: {lag: peak}
render:
  formats: [pdf, png]
```

Both select every ungrouped IRF with at most one predictor, excluding
`irf(1)` unless `irf1: true` is set for `irf-lag`. They use simultaneous 95%
intervals, the rainbow palette, and one curve per term. `irf-lag` plots each
term over the fitted lag domain at `step` (default `mean + sd`) minus the
term at `reference` (default `mean`), which is the term's deviation from
`irf(1)` for a step from the mean. `irf-predictor` plots each term in its
own facet over `values` (default the training range), minus the term at
`reference`, at lag `hold.lag` (default 0). `hold.time` (default `mean`)
fixes time axes. Other options are `terms`, where a list of labels replaces
the default selection and a selector mapping overrides its fields; `n`; `level`; `interval` (`simultaneous` or
`pointwise`); `points`; and `unconditional`. A `render` mapping overrides the
preset's render settings field by field. A preset definition cannot also
contain `query` or `layers`.

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

## Test definition

A test definition tests native model terms and estimates and tests summary
statistics of fitted effects. `terms` requests whole-function tests of terms
of the model, both mgcv smooths and IRF terms:

```yaml
schema: 1
model: main
terms: ['s(time)', 'unigramsurp:repetition_index', 're:\|subject$']
```

Terms are selected by their labels in the smooth table of `summary()`, by exact
label, `*` glob, or `re:` regular expression; `['*']` selects every smooth
term. An unmatched selector fails and lists the available labels.
`cdrgam def terms -P PROJECT [-m MODEL ...]` prints the labels of each
selected model, or of every model, from its latest fit artifact, as a YAML
list to paste under `terms`. Each term
gets the test that `summary()` reports for it: Wood's (2013) test, or Wood's
(2013b) random-effect test for fully penalized terms, with the same `edf`,
reference degrees of freedom (`df`), statistic, and p-value. The `method`
column records which test was used, or why none could be computed.

Derived statistics use `effects` and `estimands`, which must be given
together. A definition may combine them with `terms`:

```yaml
schema: 1
model: main
effects:
  response:
    terms: x
    axes:
      lag: {grid: fitted, n: 50}
      predictors: {x: 1}
  contrast:
    terms: x
    axes:
      lag: {values: [0.2, 0.8]}
      predictors: {x: 1}
estimands:
  early_minus_late: {weights: {contrast: [1, -1]}}
  area: {area: response}
  peak: {peak_lag: response}
  trough_size: {peak_value: response, direction: neg}
  whole: {wood: response}
null: 0
uncertainty: {level: 0.95, unconditional: false, draws: 2000, seed: 1}
```

Each effect is an effect query, as in a visualization `query`, that selects
exactly one term. It accepts `terms`, `composition`, `grouping`, `groups`,
`axes`, `reference`, and `n`, with the same value language. Pointwise peak
lags (`peak-pointwise`) are not available; `peak` and `peak(TERM)` are.

Each estimand uses one form:

| Form | Value | Inference |
| --- | --- | --- |
| `weights` | Weights by effect name, one per effect row or one recycled weight | Wald |
| `mean` | An effect name: the mean over its rows | Wald |
| `area` | An effect name: the trapezoidal area over lag | Wald |
| `peak_lag` | An effect name: the lag of its maximum, or minimum with `direction: neg` | Simulation |
| `peak_value` | An effect name: the value at that lag | Simulation |
| `wood` | An effect name: Wood's whole-functional test of its terms | Wood (2013) |

`area`, `peak_lag`, and `peak_value` need an effect that varies over lag
only, for one group. `null` gives the value of every estimand under the null
hypothesis; an estimand's own `null` overrides it. A `wood` estimand tests that
the effect's whole function is zero, as `summary()` tests smooth terms, and
uses only the effect's `terms`, `composition`, `grouping`, and `groups`.

All effects are evaluated on one joint distribution of their coefficients, so
contrasts between effects that share coefficients have the right variance.
Linear estimands use exact Wald inference; peak estimands apply the summary to
`draws` coefficient draws and report quantile intervals and posterior tail
areas, whose resolution is `1 / draws`. Simultaneous intervals and adjusted
p-values cover the linear estimands together and the peak estimands together.

`draws` and `seed` are execution settings. They are excluded from the
resolved identity, so changing them alone does not make the test stale. The
other fields, including `level`, are part of it. A test depends on its model's
fit and writes `tests.csv` (one row per selected term, then one per estimand,
or per group for a `wood` estimand of a conditional effect), `effects-grid.csv` (the evaluated effects),
and `request.yml` (the resolved effect requests and the joint Wald test of the
linear estimands).

## Selection and dependencies

`run` and `plan` accept project, model, prediction, visualization, comparison,
and test selectors. Supplied dimensions are conjunctive. Repeating
`--prediction` requests multiple partitions of the same selected model.
Project, model, visualization, comparison, and test selectors accept exact names,
`*` globs, and `re:`-prefixed Perl-compatible regular expressions. Regexes use
substring matching unless anchored. Prediction selectors remain exact because
each value resolves first as a model-local partition and then as a dataset
definition.

```sh
cdrgam run -P brown -m main -p val -p test
cdrgam run -P brown -m main -v main-effects
cdrgam run -P brown -c alternatives
cdrgam run -P brown -t effect-shape
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
only parallel writer to the SQLite registry and enforces the root-wide
concurrency limit across projects. It launches generic `cdrgam-worker` jobs
that claim ready work over TCP. A worker may run multiple compatible items,
even across projects. Workers validate the recorded R and package environment
before loading inputs. Short root-level locks serialize configuration and
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
root atomically, and the root registry is reconstructed from complete
artifact manifests. `--source-only` skips result hydration.

Git revisions are provenance rather than freshness inputs. Result publications
record the project revision and dirty state present when the archive was made,
while each artifact retains the core and harness implementation information
recorded at execution. Artifact contracts and checksums determine whether a
fetched result is current for the fetched definitions.
