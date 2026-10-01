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
