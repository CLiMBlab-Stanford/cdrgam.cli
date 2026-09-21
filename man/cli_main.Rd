\name{cli_main}
\alias{cli_main}
\title{Dispatch the cdrgam Command}
\usage{
cli_main(args = commandArgs(trailingOnly = TRUE))
}
\arguments{
  \item{args}{Command arguments excluding the executable name.}
}
\description{
Parse command-line arguments and dispatch a project-management command.
}
\value{An integer exit status, invisibly.}
