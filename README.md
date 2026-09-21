# cdrgam.cli

`cdrgam.cli` manages named projects and dependency-ordered CDR-GAM workloads.
One source checkout is configured with a root directory, a global concurrency
limit, and optional Slurm defaults. The same commands run serially on a local
machine or submit work through a checkout-wide Slurm scheduler and generic
worker pool.

Install from the checkout:

```sh
./scripts/install
```

The installer creates `.cdrgam/checkout.yml` when necessary and installs a
launcher bound to this checkout. Projects then live under the configured root:

Use `./scripts/install --configure` to replace an existing checkout
configuration interactively.

```sh
cdrgam def brown
cdrgam def brown --dataset training
cdrgam def brown --model main
cdrgam validate -P brown --deep
cdrgam run -P brown -m main
```

`cdrgam def` creates missing definitions and opens existing ones with
`$VISUAL`, `$EDITOR`, or R's configured editor. Copy a project's definitions,
but not its generated artifacts, with:

```sh
cdrgam def --copy-from-to brown brown-replication
```

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
