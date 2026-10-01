\name{install_cli}
\alias{install_cli}
\title{Install the cdrgam Launcher}
\usage{
install_cli(path = NULL, root = NULL)
}
\arguments{
  \item{path}{Destination path, normally in a directory on \env{PATH}. The
  platform default is \file{~/.local/bin/cdrgam} on Unix and
  \file{~/.local/bin/cdrgam.cmd} on Windows.}
  \item{root}{CDR-GAM root to use when \env{CDRGAM_ROOT} is unset. If it is
  not configured, this function creates it with local execution defaults.}
}
\description{
Write a launcher that selects one configured CDR-GAM root and uses the R
installation that installed the package.
}
\details{
The launcher uses the R executable and package library that contain the
installed \code{cdrgam.cli} package. This prevents packages from another R
installation's user library from shadowing its dependencies. The default root
is \code{tools::R_user_dir("cdrgam.cli", "data")}. A nonempty
\env{CDRGAM_ROOT} overrides the launcher's fallback root.
The launcher is a POSIX shell script on Unix and a command script on Windows.
}
\value{The normalized launcher path, invisibly.}
