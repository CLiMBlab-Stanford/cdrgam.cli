# Repository instructions for AI agents

## Required reading

Before making a material change, read `AI_PROVENANCE.md` and register the
provider, tool or agent, exposed model identifier, period, and roles under
**Systems used**. Update an existing row when appropriate.

Before creating or revising documentation, docstrings, diagnostics, or
explanatory comments, read and follow `WRITING_POLICY.md`.

## Package boundary

`cdrgam.cli` owns project schemas, validation, orchestration, file I/O,
artifact identity and lifecycle, scheduling, and presentation. The `cdrgam`
package owns model preparation, fitting, prediction, inference, and
statistical plot-data generation. Use only exported `cdrgam` APIs. Add and test
a missing public seam in the core repository instead of using `:::` here.

Keep scientific identity separate from execution and presentation settings.
Build every managed path through the central path builder. Definitions are
user-owned and no cleanup operation may select them.

## Correctness and testing

Install both packages into a clean temporary R library for integration tests.
Run the standalone test and package check before handing off a broad change:

```sh
Rscript tests/phase1.R
R CMD check --no-manual --no-build-vignettes cdrgam.cli_*.tar.gz
```

Do not commit package tarballs, check directories, installed libraries,
project outputs, checkpoints, logs, plots, or fitted objects.

## Publication

Commit, push, tag, or create a pull request only when the user explicitly
requests it. AI systems are not Git authors or signatories. Follow
`CONTRIBUTING.md` for attribution and identity requirements.
