\name{install_cli}
\alias{install_cli}
\title{Install the Checkout-Bound cdrgam Launcher}
\usage{
install_cli(path = file.path("~", ".local", "bin", "cdrgam"),
  checkout = NULL)
}
\arguments{
  \item{path}{Destination path, normally in a directory on \env{PATH}.}
  \item{checkout}{A source checkout containing \file{.cdrgam/checkout.yml}.}
}
\description{
Write an executable launcher that selects one configured source checkout and
uses the R installation that installed the package.
}
\details{
The launcher uses the R executable and package library that contain the
installed \code{cdrgam.cli} package. This prevents packages from another R
installation's user library from shadowing its dependencies.
}
\value{The normalized launcher path, invisibly.}
