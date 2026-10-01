\name{publication}
\alias{cdrgam_cli_publish}
\alias{cdrgam_cli_fetch}
\title{Publish and Fetch CDR-GAM Projects}
\usage{
cdrgam_cli_publish(project, results = c("none", "archive", "url", "git"),
  archive = NULL, url = NULL, sha256 = NULL, models = NULL,
  predictions = NULL, visualizations = NULL, comparisons = NULL,
  commit = NULL, push = FALSE, cdrgam_root = NULL)

cdrgam_cli_fetch(location, project = NULL, source_only = FALSE,
  results = NULL, cdrgam_root = NULL)
}
\arguments{
  \item{project}{Project name. For fetching, an optional local project name.}
  \item{results}{For publication, the result mode. For fetching, an optional
  local path or direct URL overriding the publication manifest.}
  \item{archive}{Destination for a generated compressed tar archive.}
  \item{url}{Direct download URL for a separately hosted results archive.}
  \item{sha256}{Required SHA-256 checksum for URL publication.}
  \item{models, predictions, visualizations, comparisons}{Optional workload
  selectors. Selected downstream work includes its dependency closure.}
  \item{commit}{Optional Git commit message. All changes already staged in the
  project repository are included.}
  \item{push}{Whether to push the current branch after publication. This
  requires \code{commit}.}
  \item{cdrgam_root}{Configured CDR-GAM root.}
  \item{location}{Git repository, local Git repository, or direct project
  source archive location.}
  \item{source_only}{Whether to skip separately published results.}
}
\description{
\code{cdrgam_cli_publish()} validates project sources and selected complete
artifacts, writes tracked publication metadata, and stages it. Archive mode
creates a checksum-verified result archive but does not upload it. Commit and
push actions occur only when explicitly requested.

\code{cdrgam_cli_fetch()} installs a published source repository or archive
under the configured root. It verifies separately hosted results before
installation and reconstructs local registry state from complete artifact
manifests. Git revisions are recorded as provenance; artifact contracts and
checksums determine freshness.
}
