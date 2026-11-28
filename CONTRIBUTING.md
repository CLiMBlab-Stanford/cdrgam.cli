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
./scripts/test effects
./scripts/test render
./scripts/test methods
./scripts/test viz
./scripts/test integration
./scripts/test portability
```

The `methods` scope compares the methods report of its fixture projects with
the files in `tests/methods-golden/`. After an intended change to the report
text, rerun it with `CDRGAM_UPDATE_GOLDEN=1` to rewrite those files, and review
their diff.

Development scripts resolve R through the active Conda environment first. If
none is active, they use the `cdrgam` Conda environment when it exists. Set
`CDRGAM_CONDA_ENV` to choose a different environment or `R_BIN` to select an
exact executable. Local scripts otherwise fail rather than silently choosing
a system R; `CDRGAM_ALLOW_SYSTEM_R=1` is an explicit override. Hosted checks
continue to use their provisioned R.

`./scripts/test all` runs every scope. Each invocation installs the current
CLI source into a temporary library. The integration scope covers the complete
project lifecycle and is reserved for changes that cross component boundaries.
The portability scope also checks the cross-platform preflight selection
policy and is appropriate after changes to paths, launchers, process handling,
or scheduler integration.

Development branches are not release-gated. Before opening a release pull
request, run the manual cross-platform preflight when changes may affect paths,
file I/O, launchers, process handling, scheduler integration, or runtime
configuration:

```sh
gh workflow run cross-platform-preflight.yaml --ref dev -f scope=auto
gh run watch
```

The preflight compares the selected commit with the newest successful
automatic or full preflight ancestor on that branch. If no such run exists, it
uses the merge base with `main`. It runs the Windows, macOS, and Linux
portability profile only when the cumulative un-preflighted changes match the
policy. Use `scope=full` to force complete package checks. A narrow manual
scope does not advance the automatic baseline.

Keep calls to `cdrgam` and knowledge of fitted-object representation in
`R/core-adapter.R`. Keep SQLite access in `R/registry.R`. The architecture test
enforces these boundaries. Add a public core API when the adapter lacks a
required operation; do not inspect an additional core implementation field in
another CLI module.

Rendered visualizations and the `cdrgam viz` viewer share render settings.
To add a render field:

1. Add an entry to `.cdrgam_cli_render_fields` in `R/render-fields.R`. Its
   validation, default, and geometry constraints apply to definitions and to
   the viewer. A field without a default stays absent from existing renders,
   which keeps their visualization identities; a field with a default must be
   listed after `formats`.
2. Resolve what the field changes in `.cdrgam_cli_plot_spec()` in
   `R/plot-spec.R`, not in a renderer, and draw it in both
   `R/visualization.R` (ggplot2) and `R/viz-plotly.R` (plotly).
3. Set its `viewer` status. A `control` field needs an input in
   `.cdrgam_cli_viz_appearance()` and handling in `.cdrgam_cli_viz_render()`;
   a `derived` or `file` field needs a `reason`.
4. Add the field's values to the backend parity cases in `tests/render.R`.

The `render` scope fails when validation changes the identity of an existing
render, when the two renderers draw a specification differently, and the
`viz` scope fails when a `control` field has no viewer input.

Before proposing a release, update `Version` in `DESCRIPTION` and document
intentional schema or command-line incompatibilities and their migration path.
The hosted release gate is the authoritative clean-environment check.

Run the repository's correctness-focused R lint profile after installing the
package and its dependencies:

```sh
"$(./scripts/r-bin)" -e 'lintr::lint_package(".")'
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

## Releases and compatibility

`main` contains released code. Prepare releases on `dev` or a release branch
and merge them into `main` only through a pull request. Every pull request to
`main` must change `Version` in `DESCRIPTION` to a later
`MAJOR.MINOR.PATCH` value. Repository protection requires the release gate and
the Linux, macOS, and Windows package checks to pass before merge. The release
gate runs the lint profile, the complete test suite through `R CMD check`, and
the complete pkgdown build against the released core package.

Use patch releases for compatible fixes. During the 0.x series, use minor
releases for new features and intentional interface changes. Release the core
package first when a harness release requires a newer core API.

After the pull request merges, create an annotated `vMAJOR.MINOR.PATCH` tag on
the validated merge commit, using a concise summary written for that release,
not generated by a script or template, as the tag message. The tag workflow
rejects versions that do not match `DESCRIPTION`, lightweight or empty tags,
and commits outside `main`, then uses that message as the corresponding
GitHub Release description. Verify the tag workflow and final description
before treating publication as complete.
Never move or replace a published tag.
