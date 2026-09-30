# Publish and Fetch CDR-GAM Projects

`cdrgam_cli_publish()` validates project sources and selected complete
artifacts, writes tracked publication metadata, and stages it. Archive
mode creates a checksum-verified result archive but does not upload it.
Commit and push actions occur only when explicitly requested.

`cdrgam_cli_fetch()` installs a published source repository or archive
under the configured root. It verifies separately hosted results before
installation and reconstructs local registry state from complete
artifact manifests. Git revisions are recorded as provenance; artifact
contracts and checksums determine freshness.

## Usage

``` r
cdrgam_cli_publish(project, results = c("none", "archive", "url", "git"),
  archive = NULL, url = NULL, sha256 = NULL, models = NULL,
  predictions = NULL, visualizations = NULL, comparisons = NULL,
  commit = NULL, push = FALSE, checkout = NULL)

cdrgam_cli_fetch(location, project = NULL, source_only = FALSE,
  results = NULL, checkout = NULL)
```

## Arguments

- project:

  Project name. For fetching, an optional local project name.

- results:

  For publication, the result mode. For fetching, an optional local path
  or direct URL overriding the publication manifest.

- archive:

  Destination for a generated compressed tar archive.

- url:

  Direct download URL for a separately hosted results archive.

- sha256:

  Required SHA-256 checksum for URL publication.

- models, predictions, visualizations, comparisons:

  Optional workload selectors. Selected downstream work includes its
  dependency closure.

- commit:

  Optional Git commit message. All changes already staged in the project
  repository are included.

- push:

  Whether to push the current branch after publication. This requires
  `commit`.

- checkout:

  Configured harness instance directory.

- location:

  Git repository, local Git repository, or direct project source archive
  location.

- source_only:

  Whether to skip separately published results.
