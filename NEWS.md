# cdrgam.cli (development version)

- Visualizations accept `geometry: point`, which draws estimates as points
  with error bars, and `geometry: bar`, which draws them as bars with error
  bars. `interval: errorbar` is their default. Series at one x position are
  placed side by side.

- `render.spacing` draws a numeric x-axis at its evaluated values, one tick
  per value, spaced `uniform`ly in sorted order or `linear`ly by value.

- `render.statistic: peak-lag` plots the peak lags a query used, with their
  posterior intervals, as points or bars.

- `render.palette` accepts `grey`, the viridis options, and ColorBrewer
  palette names in addition to `default` and `rainbow`. The peak timeline
  uses the same colors as the main plot.

- `render.palette: okabe-ito` gives the viewer's eight line colors: the
  colorblind-safe Okabe-Ito colors except yellow, then grey and black.

- The viewer draws its plots from the same plot specification as rendered
  visualizations. Lines keep their colors; with more than eight lines the
  viewer switches every line to viridis and says so under the plot.
  Heatmaps use the rendered surfaces' diverging colors, symmetric about
  zero.

- The viewer's **Appearance** panel offers the `render` settings that apply
  to it: points and bars, peak lags, intervals, tick spacing, palettes
  including custom gradients, the peak timeline, titles, and theme. It
  replaces the **Show intervals** option and the surface **Display**
  buttons; surfaces add contour-only and heatmap-with-contours displays and
  a standard-error heatmap. A test fails when a render field has no viewer
  control and no stated reason.

- **Show render YAML** in the viewer prints its appearance settings as a
  `render` block for a visualization definition.

- The peak timeline keeps its lane order when only some peaks have
  posterior draws; a lane without draws was drawn at the top.

- The vertical axis of a rendered surface is titled by its mapped column,
  or `ylab`, instead of "Effect on linear predictor".

- Invalid render choices name the field and its accepted values, as in
  `render.theme must be one of paper, minimal, slides`.

- `render.palette` also accepts a list of two or more colors (hex,
  `rgb(R, G, B)`, or R color names). The plot colors its lines, points, or
  bars at equal steps from the first color to the last.

# cdrgam.cli 0.4.0

- Lag values accept `peak(<term>)` and `peak-neg(<term>)`, which hold every
  term at the lag where the named term's response is largest or smallest.
  `<term>` is a term label from the effect catalog, such as
  `peak(irf(1))`.

- Effects that use a peak write `effect-peaks.csv` with each peak lag and a
  `level` interval from posterior draws of that lag, and the draws in
  `effect-peak-draws.csv`. `uncertainty.peak_draws` sets the number of draws
  (default 1000; `0` skips them). The draws need `cdrgam` 0.4.0.

- Line plots that use a peak can show a timeline of peak lags, intervals,
  and draws below the main panel. `render.peaks` is `auto` (when terms were
  held at different peak lags), `always`, or `never`. The viewer draws the
  same timeline.

- `cdrgam viz` on a root without Slurm, or with `--local`, prints the
  viewer's address and opens the browser; `--no-browser` suppresses that.
  A local viewer makes no Slurm calls, so it can run where Slurm does not,
  such as Windows. `--stop` and `--restart` now
  explain that a local viewer is stopped by interrupting it.

- Linux hosted checks run on Ubuntu 24.04.

# cdrgam.cli 0.3.0

- Visualization definitions accept `preset: irf-lag` and
  `preset: irf-predictor`. `irf-lag` plots every ungrouped IRF with one
  predictor over lag, at the predictor's mean plus one SD relative to its
  mean, and can add `irf(1)`. `irf-predictor` plots the same terms over their
  predictor values at a fixed lag, 0 by default. Both use simultaneous 95%
  intervals and a rainbow palette by default, and hold time axes at their
  training means.

- Effect-query axis values and references accept arithmetic over an axis's
  training `mean`, `sd`, and `median`, such as `mean + 2 * sd`, and a
  `range: [low, high]` grid. Lag values also accept `peak` (or `peak-pos`)
  and `peak-neg`, the lag at which a term's response is largest or smallest.
  A wildcard reference `"*"` applies to every predictor of each term, and
  the term selector field `interacting` keeps or drops terms with more than
  one predictor.

- `uncertainty: {kind: simultaneous}` replaces pointwise limits with
  simultaneous ones over each term's grid, computed in the harness from
  `estimate_effect()` alone. The viewer offers the same intervals, accepts
  value expressions in its fixed and reference fields, and now starts
  unvaried axes at lag 0 and training means rather than medians.

- Effect queries now hold unspecified time and predictor axes at their
  training means rather than medians. Grids from existing definitions that
  relied on the median change.

- Effects are now evaluated one term at a time. Effect grids gain
  `predictor` and `predictor_value` columns for terms with one predictor,
  and `critical_value` for simultaneous intervals. Line renderings accept
  `palette: rainbow` and facet `scales`.

- Published fit diagnostics include `newton_step_max` and
  `predicted_decrease_floor` from the core, beside `predicted_decrease` and
  the raw gradient maxima. A gradient above the tolerance along a
  well-curved direction can then be read against the negligible Newton step
  it implies.

- Rerunning work after its Slurm worker ran out of memory no longer fails at
  once. The killed worker left its attempt marked running, and when the
  node reused its process ID for a process the scheduler could not inspect,
  the attempt was treated as still active, so every new claim failed. An
  attempt whose Slurm job has ended is now inactive, as is one whose process
  belongs to another user. When the scheduler fails work without an active
  attempt, it records the reason on the latest attempt, so status no longer
  shows the earlier attempt's error. When `squeue` no longer knows the job,
  as happens once Slurm purges a finished job, Slurm accounting decides
  instead of the recorded process ID.

- Definitions without `fit.rescale_predictors` now fit with rescaled
  predictors, because the core default changed to `true`. Their model
  identities and fitted results change accordingly; set
  `fit.rescale_predictors: false` to keep native-unit fits.

- `diagnostics.yml` now includes the core's `notes`, the interior and full
  outer-Hessian minimum eigenvalues, and one `hessian_boundary` record per
  smoothing parameter on the boundary of its range, with its profile bound.
  These require the development `cdrgam` that classifies boundary smoothing
  parameters.

- Effect visualization queries accept `reference`, which subtracts the effect
  at reference axis values, with `default` leaving a predictor at the model
  default, and `test: true`, which writes joint Wald tests to
  `effect-tests.csv`. Both require the development `cdrgam` that provides
  `test_effect()`. With `composition: total`, interaction terms such as
  `irf(wlen, repetition_index, k_p=list(NULL, 3))` now include their
  lower-order terms, which follows the core change.
- The `deviation` composition is removed from visualization queries, layers,
  and the viewer. It returned the same values as `term`. Use
  `grouping: deviation` for grouped deviations.
- `cdrgam viz` has a Reference control. Each line or surface can subtract the
  effect at reference values of its predictors; an empty field uses the model
  default. The line label records the reference, and saved lines keep it
  across reloads.
- The viewer's lines pane has a fixed height and scrolls, so the plot keeps
  its size as lines are added. The pane could previously be pushed out of
  view.
- Line renderings draw a separate curve for each value of every varying axis
  other than `x`. Several values of a moderator were previously joined into
  one path. Numeric `color` and `linetype` mappings with at most 12 values use
  a discrete legend.
- `cdrgam purge` builds its selection in seconds for projects with many models
  and datasets. It previously built and renormalized a path for every model and
  dataset combination against every target, which took more than 25 minutes
  for a project with 92 models and 65 datasets; it now builds paths only for
  work directories that exist.
- Model definitions accept `fit.center_predictors` (true or false), passed to
  the core package's linear-predictor centering.
- Adds `cdrgam viz`, a browser viewer for completed fits. It plots effect
  lines, including several lines from different fits on one plot, and two-axis
  surfaces, with fixed values for the remaining tensor axes. On Slurm roots the
  server runs in a per-user Slurm job that later calls reuse. Its Refresh data
  button picks up new and refitted results, and its Restart server button
  reloads installed code without a new Slurm job. The viewer's dependencies
  (`shiny`, `bslib`, `plotly`, `jsonlite`, `later`) are optional.
- Publishes each finished autosimplification fit to the model's results
  directory as it completes. Earlier fits move to `model_history/`. When no
  allowed simplification resolves the remaining fit warnings, the last fit stays
  published with its warnings, `status` reports an error, and dependent work
  does not run.
- Keeps workers connected when the scheduler restarts on the same node. Process
  start times were stored with about 8 significant digits, so a live scheduler
  or attempt could look dead and workers ignored the scheduler's new address.
  A worker that cannot report a nonconverged or unresolved fit now stops
  cleanly instead of failing.

# cdrgam.cli 0.2.1

- Repairs release-tag validation for annotated tag messages containing Git's
  formatting syntax.

# cdrgam.cli 0.2.0

- Gives clean installations a writable, cross-platform default CDR-GAM root
  in R's user data directory. Root-local `.cdrgam/site.yml`, registry, scheduler
  state, and projects form one movable harness instance. `CDRGAM_ROOT` switches
  that complete instance without splitting configuration from data.
- Replaces the exported `checkout` argument with `cdrgam_root`. Existing sites
  migrate by placing their scheduler fields and concurrency setting in
  `<CDRGAM_ROOT>/.cdrgam/site.yml` and omitting the now-implicit
  `cdrgam_root` field.
- Speeds up work-graph construction by reusing source checksums while file
  size and modification time are unchanged and by removing redundant
  filesystem probes from managed-path construction.
- Reports the current `cdrgam run` stage on standard error. Interactive
  terminals receive an in-place, color-rotating activity indicator.
- Makes purge remove matching artifacts, attempts, logs, and registry records,
  and strengthens internal module boundaries for focused development tests.

# cdrgam.cli 0.1.0

- Establishes the first public development release of the CDR-GAM project
  harness.
- Provides version-controlled project definitions, validation, dependency
  planning, local execution, and shared Slurm orchestration.
- Manages atomic artifacts, logs, status, cleanup, publication, and retrieval
  across projects in one configured harness instance.
