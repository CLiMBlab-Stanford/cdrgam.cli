# Contributing

Keep changes reviewable and include tests for changed behavior. Integration
tests must install `cdrgam` and `cdrgam.cli` into a clean temporary library and
exercise only exported core APIs. Generated project state and package build
artifacts do not belong in version control.

Before a release, update `Version` in `DESCRIPTION`, run the complete tests,
and run `R CMD check` on a clean source package. Document intentional schema
or command-line incompatibilities and their migration path.

Material AI assistance must be disclosed in affected commits with an
`Assisted-by: <tool-or-agent>:<model-identifier>` trailer. A human contributor
remains the author and is responsible for review. Do not invent or modify Git
identity, substitute another account, fabricate a signature, or add a
`Signed-off-by:` certification without explicit human authorization.

Commits, pushes, tags, pull requests, and releases require an explicit user
request. Technical prose must follow `WRITING_POLICY.md`.
