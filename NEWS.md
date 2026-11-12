# cdrgam.cli (development version)

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
