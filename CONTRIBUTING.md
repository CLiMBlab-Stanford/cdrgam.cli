# Contributing

Keep changes reviewable and include tests for changed behavior. Integration
tests must install `cdrgam` and `cdrgam.cli` into a clean temporary library and
exercise only exported core APIs. Generated project state and package build
artifacts do not belong in version control.

Use the narrowest applicable test scope while developing:

```sh
./scripts/test architecture
./scripts/test cli
./scripts/test paths
./scripts/test registry
./scripts/test identities
./scripts/test integration
```

`./scripts/test all` runs every scope. Each invocation installs the current
CLI source into a temporary library. The integration scope covers the complete
project lifecycle and is reserved for changes that cross component boundaries.

Keep calls to `cdrgam` and knowledge of fitted-object representation in
`R/core-adapter.R`. Keep SQLite access in `R/registry.R`. The architecture test
enforces these boundaries. Add a public core API when the adapter lacks a
required operation; do not inspect an additional core implementation field in
another CLI module.

Before a release, update `Version` in `DESCRIPTION`, run the complete tests,
and run `R CMD check` on a clean source package. Document intentional schema
or command-line incompatibilities and their migration path.

Run the repository's correctness-focused R lint profile after installing the
package and its dependencies:

```sh
Rscript -e 'lintr::lint_package(".")'
```

The profile preserves the established formatting conventions. New lint rules
should identify actionable defects without requiring unrelated restyling.

Material AI assistance must be disclosed in affected commits with an
`Assisted-by: <tool-or-agent>:<model-identifier>` trailer. A human contributor
remains the author and is responsible for review. Do not invent or modify Git
identity, substitute another account, fabricate a signature, or add a
`Signed-off-by:` certification without explicit human authorization.

Commits, pushes, tags, pull requests, and releases require an explicit user
request. Technical prose must follow `WRITING_POLICY.md`.
