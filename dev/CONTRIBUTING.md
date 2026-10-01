# Contributing

Keep changes reviewable and include tests for changed behavior.
Integration tests must install `cdrgam` and `cdrgam.cli` into a clean
temporary library and exercise only exported core APIs. Generated
project state and package build artifacts do not belong in version
control.

Use the narrowest applicable test scope while developing:

``` sh
./scripts/test architecture
./scripts/test cli
./scripts/test paths
./scripts/test registry
./scripts/test identities
./scripts/test integration
```

`./scripts/test all` runs every scope. Each invocation installs the
current CLI source into a temporary library. The integration scope
covers the complete project lifecycle and is reserved for changes that
cross component boundaries.

Keep calls to `cdrgam` and knowledge of fitted-object representation in
`R/core-adapter.R`. Keep SQLite access in `R/registry.R`. The
architecture test enforces these boundaries. Add a public core API when
the adapter lacks a required operation; do not inspect an additional
core implementation field in another CLI module.

Before proposing a release, update `Version` in `DESCRIPTION` and
document intentional schema or command-line incompatibilities and their
migration path. The hosted release gate is the authoritative
clean-environment check.

Run the repository’s correctness-focused R lint profile after installing
the package and its dependencies:

``` sh
Rscript -e 'lintr::lint_package(".")'
```

The profile preserves the established formatting conventions. New lint
rules should identify actionable defects without requiring unrelated
restyling.

Material AI assistance must be disclosed in affected commits with an
`Assisted-by: <tool-or-agent>:<model-identifier>` trailer. A human
contributor remains the author and is responsible for review. Do not
invent or modify Git identity, substitute another account, fabricate a
signature, or add a `Signed-off-by:` certification without explicit
human authorization.

Commits, pushes, tags, pull requests, and releases require an explicit
user request. Technical prose must follow `WRITING_POLICY.md`.

## Releases and compatibility

`main` contains released code. Prepare releases on `dev` or a release
branch and merge them into `main` only through a pull request. Every
pull request to `main` must change `Version` in `DESCRIPTION` to a later
`MAJOR.MINOR.PATCH` value. Repository protection requires the release
gate and the Linux, macOS, and Windows package checks to pass before
merge. The release gate runs the lint profile, the complete test suite
through `R CMD check`, and the complete pkgdown build against the
released core package.

Use patch releases for compatible fixes. During the 0.x series, use
minor releases for new features and intentional interface changes.
Release the core package first when a harness release requires a newer
core API.

After the pull request merges, create an annotated `vMAJOR.MINOR.PATCH`
tag on the validated merge commit, using a concise human-written release
summary as the tag message. The tag workflow rejects versions that do
not match `DESCRIPTION`, lightweight or empty tags, and commits outside
`main`, then uses that message as the corresponding GitHub Release
description. Verify the tag workflow and final description before
treating publication as complete. Never move or replace a published tag.
