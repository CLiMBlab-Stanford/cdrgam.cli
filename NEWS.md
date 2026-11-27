# cdrgam.cli 0.5.0

- New `cdrgam methods` command. It writes a cited methods section for the
  selected work items in RTF (the default), Markdown, or LaTeX, together with
  supplementary tables and a `methods.json` trace. Several projects can be
  described in one report. See `docs/methods-reports.md`. Requires cdrgam
  0.5.0 or later for `methods_facts()`.

- New test definitions, under `definitions/tests/`. `terms` requests the
  whole-function tests that `summary()` reports for native model terms, mgcv
  smooths and IRF terms alike, selected by label: `terms: ['s(time)']`.
  Parametric terms are included and get mgcv's per-term Wald tests.
  `cdrgam def terms [-m MODEL ...]` prints each fitted model's labels; fits
  now also record `parametric-terms.csv`. `cdrgam run -t MODEL`, when no
  test has that name, creates a test of every term of the model and runs it.
  `effects` and `estimands` estimate and test derived summary statistics of
  fitted effects: weighted contrasts, means, areas, peak lags and values, and
  Wood's whole-functional tests of compositions. Effects use the
  visualization value language and are evaluated on one joint coefficient
  distribution. `cdrgam run -t NAME` writes `results/tests/NAME/tests.tsv`, tab-separated for
  `column -t -s $'\t'`.
  `--test`/`-t` selects tests in `def`, `plan`, `run`, `stop`, `log`, and
  `purge`. Estimands need a `cdrgam` release with `effect_inference()`.

- Simultaneous bands read the effect covariance from one core evaluation,
  instead of one evaluation per point. A line of `spr-main` with 40 band
  points takes about 3 seconds instead of 70. Their critical values, and
  those of bands at peaks, now come from `cdrgam`'s
  `simultaneous_critical_value()`, so the CLI no longer computes them
  itself. Simultaneous intervals need a `cdrgam` release with that function
  and report an error asking to update `cdrgam` otherwise.

- `cdrgam def edit` accepts `*` globs and `re:` regular expressions, as
  `def ls`, `def val`, and `def rm` do, and opens each existing match in
  turn: `cdrgam def edit -P rep -v '*-wlen'`. Literal names still create
  missing definitions; patterns cannot be combined with `--source`.

- Line and point plots draw a thin line at zero effect in each panel whose
  vertical range includes zero. `render.zero_line: false`, or clearing
  **Line at zero** in `cdrgam viz`, turns it off. Existing visualizations
  keep their identities and draw the line when they next render.

- `render.panel_width` and `render.panel_height` fix the size of the plot
  panel in inches, and the rendered file grows to fit the axes, titles, and
  legend. Figures with different legends then have identical panels. They
  cannot be combined with `width` or `height`. `cdrgam.cli` now requires
  ggplot2 4.0.0 or later.

- Each line in the `cdrgam viz` line list has **Duplicate**, which adds a
  copy below it labeled "(copy)". The line being edited is marked with a
  tinted background and a bar on its left instead of a blue fill, so its
  links stay readable.

- `render.ylim: [low, high]` sets the vertical display range of line, point,
  and bar plots without dropping rows; either end may be `null`. In
  `cdrgam viz`, **Vertical axis range** sets it, and **X range** under
  **Options** sets where lines are evaluated, as numbers or expressions such
  as `mean - 2 * sd`.

- `cdrgam purge` selectors also match names that no longer have a
  definition, such as a visualization removed with `cdrgam def rm`. Their
  results, work directories, logs, and registry records can be purged by
  name, as `cdrgam status` already lists them.

- `render.reverse_palette`, **Reverse palette order** in `cdrgam viz`,
  starts the palette from its last color, so with `okabe-ito` the first
  line is black and the second grey. Spread palettes such as
  viridis are reversed end to end. The viewer's line list shows the same
  colors.

- In `cdrgam viz`, X tick values typed for lines now set the plot's ticks.
  They had reached the exported render YAML but not the plot itself.

- `render.tick_labels` sets the text of the x tick labels: one per break in
  the order written, or one per distinct x value when there are no breaks.
  In `cdrgam viz`, **X tick labels** sets it for typed X tick values.

- `render.geometry: connected-point`, **Connected points** in `cdrgam viz`,
  draws points with their error bars and joins each series' points in x
  order with a thin line, without an interval band, to show trends.

- `lag: peak-pointwise(LABEL)` and `peak-pointwise-neg(LABEL)` hold each
  point at the peak lag of another term, found at that point's values of the
  named term's predictors. For example, an interaction can be read at the
  lag where one of its main effects peaks. The named term's predictors must
  be predictors of the plotted term. Simultaneous bands draw both terms
  from the same coefficient sample, so they include the uncertainty in the
  borrowed peak lags.

- The `cdrgam run` progress line shows a braille spinner in place of the
  color-cycling dot. It uses nro's frames and its 0.08-second frame interval.

- `render.breaks` labels chosen x values on a continuous axis without
  changing where the effect is evaluated. In `cdrgam viz`, **X tick values**
  now sets it for lines, which stay on the evaluation grid instead of being
  evaluated only at the ticks. Points and bars are still evaluated at the
  tick values, and **Grid points per axis** is hidden while it has no effect.
  An empty grid-points field uses the default of 200.

- Effect grids are no longer separate work items or cached. A visualization
  evaluates its effects from the fit each time it renders, so its figure
  always reflects the installed `cdrgam`. The plotted numbers are written
  beside the figure in `results/visualizations/NAME/`: `effect-grid.csv`
  and its companions, or `line-ID-grid.csv` and its companions for each
  line. Effect directories under `.cdrgam/work/` from earlier versions are
  no longer read and can be deleted.

- Rendered visualizations are written to `results/visualizations/NAME/`
  instead of `results/models/MODEL/visualizations/NAME/`, because a
  visualization with `lines` may draw from several models. Existing
  visualizations re-render into the new location on the next run. The legend of a lines plot no longer shows a `line`
  title.

- Parallel jobs that share a work item, such as a fit used by several
  visualizations, no longer conflict. Claiming and publishing an item hold a
  per-item lock, and a new attempt is marked running at once, so another job
  no longer deletes it as stale. A job that reaches an item another job is
  running waits, then reuses the published artifact. Under the scheduler,
  the worker takes other ready work instead. Creating a shared results
  directory no longer fails when another job creates it first.

- `cdrgam purge --work --logs` with a workload selector removes only the
  selected work items' attempts, archived artifacts, and logs. It previously
  removed the project's entire work and log directories, including files
  that belonged to other models. Without a workload selector, both options
  still remove the whole directories.

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

- Simultaneous intervals for values that use a peak, such as `lag: peak` or
  `peak-pointwise`, now include the uncertainty about where the peak lies:
  posterior draws each re-find the peak before the band is formed. Lines at
  `peak-pointwise` accept simultaneous intervals. This needs cdrgam's new
  `sample_effect_coefficients()`, and changes the bands of the `irf-predictor`
  preset, which plots at peak lags.

- Visualization definitions accept `lines`: one effect per line, each with
  its own term, model, axes, reference, composition, and grouping, drawn on
  one plot. The definition's `query` gives defaults that lines inherit and
  may override. The viewer evaluates every line through the same code as
  `cdrgam run`, draws the same figure, and exports its lines, options, and
  appearance as such a definition under **Definition YAML**.

- `render.theme` accepts every native ggplot2 theme (`grey`, `gray`, `bw`,
  `linedraw`, `light`, `dark`, `minimal`, `classic`, `void`, `test`), and
  `render.y_zero: true` keeps zero on the vertical axis. The viewer offers
  both under **Appearance** and starts in `classic`; definitions still
  default to `paper`.

- The viewer keeps its form, including the model, term, axes, fixed and
  reference values, and the **Options** panel, across **Refresh data**, page
  reloads, and server restarts, as it already kept lines and appearance.

- In the viewer, the peak timeline appears whenever a line uses a peak,
  unless **Show peak lags** under the plot is off, and its rows carry the
  legend labels. Its ticks stay within their own row, and stacked panels
  leave room for each x-axis title.

- Points at one x position sit in adjacent columns one point wide, so
  clusters at different positions stay apart; bars still fill 80% of the
  gap.
  Arrows in the lines list reorder lines, the palette menu shows each
  palette's colors, and **X tick values** under **Options** evaluates lines
  at chosen x values with a tick at each.

- `lag: peak-pointwise` and `peak-pointwise-neg` hold each evaluation point
  at its own peak lag: the lag where the query's effect, after its reference
  subtraction, is largest or smallest at that point. Effect artifacts record
  the lags in `effect-pointwise-peaks.csv` with posterior intervals, and line
  and point plots, rendered and in the viewer, add a panel of peak lag over
  the x-axis. This needs `estimate_effect(points=)` from the development
  cdrgam.

- Under `composition: total`, and the viewer's Total composition, `peak`
  and `peak-neg` locate the peak of the total response to a one-SD increase
  rather than of the term alone. An interaction's peak now matches the curve
  its lines show. Peak tables gain a `composition` column.

- The viewer's reference fields cover only the fixed axes, lag included;
  a plotted axis keeps its value in the reference. Previously each plotted
  predictor was set to its model default, which dropped the term from the
  reference. While a line is being edited, **Update line** and **Cancel**
  replace **Add line**.

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
