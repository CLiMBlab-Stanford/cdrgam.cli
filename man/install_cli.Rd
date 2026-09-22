\name{install_cli}
\alias{install_cli}
\title{Install the Checkout-Bound cdrgam Launcher}
\usage{
install_cli(path = NULL, checkout = NULL)
}
\arguments{
  \item{path}{Destination path, normally in a directory on \env{PATH}. The
  platform default is \file{~/.local/bin/cdrgam} on Unix and
  \file{~/.local/bin/cdrgam.cmd} on Windows.}
  \item{checkout}{A configured harness instance containing
  \file{.cdrgam/checkout.yml}.}
}
\description{
Write a launcher that selects one configured harness instance and uses the R
installation that installed the package.
}
\details{
The launcher uses the R executable and package library that contain the
installed \code{cdrgam.cli} package. This prevents packages from another R
installation's user library from shadowing its dependencies.
The launcher is a POSIX shell script on Unix and a command script on Windows.
}
\value{The normalized launcher path, invisibly.}
