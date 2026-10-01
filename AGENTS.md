# Repository instructions for AI agents

## Required reading

Before making a material change, read `AI_PROVENANCE.md` and register
the provider, tool or agent, exposed model identifier, period, and roles
under **Systems used**. Update an existing row when appropriate.

Before creating or revising documentation, docstrings, diagnostics, or
explanatory comments, read and follow `WRITING_POLICY.md`.

## Package boundary

`cdrgam.cli` owns project schemas, validation, orchestration, file I/O,
artifact identity and lifecycle, scheduling, and presentation. The
`cdrgam` package owns model preparation, fitting, prediction, inference,
and statistical plot-data generation. Use only exported `cdrgam` APIs.
Add and test a missing public seam in the core repository instead of
using `:::` here.

Keep scientific identity separate from execution and presentation
settings. Build every managed path through the central path builder.
Definitions are user-owned and no cleanup operation may select them.

## Correctness and testing

Use the narrowest applicable `scripts/test` scope while developing. Add
or run focused tests for the behavior changed by the task. Reserve the
integration scope and clean temporary installation of both packages for
changes that cross the core/CLI boundary or affect installed-package
behavior.

Do not run a full `R CMD check` for routine development changes. The
hosted release gate is the authoritative full-suite, package-check,
documentation, and cross-platform validation. Run equivalent full
validation locally only when preparing a release, investigating a gate
failure, or when the user asks for it explicitly.

Available focused scopes are documented in `CONTRIBUTING.md`; for
example:

``` sh
./scripts/test paths
./scripts/test registry
./scripts/test integration
```

Do not commit package tarballs, check directories, installed libraries,
project outputs, checkpoints, logs, plots, or fitted objects.

## Publication

Commit, push, tag, or create a pull request only when the user
explicitly requests it. Changes enter the release-only `main` branch
through a pull request after its hosted release gate and cross-platform
checks pass. AI systems are not Git authors or signatories. Follow
`CONTRIBUTING.md` for attribution and identity requirements.
