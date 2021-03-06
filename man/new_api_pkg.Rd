% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apitools.R
\name{new_api_pkg}
\alias{new_api_pkg}
\title{new_api_pkg}
\usage{
new_api_pkg(pkg = NULL, key = NULL, base = NULL, version = NULL)
}
\arguments{
\item{pkg}{Name of package.}

\item{key}{Authorization key(s). In interactive sessions this defaults to
input supplied to the console.}

\item{base}{Base URL to API. In interactive sessions this defaults to input
supplied to the console.}

\item{version}{Version number for API. In interactive sessions this defaults
to input supplied to the console.}
}
\value{
Creates new R package with API funs skeleton.
}
\description{
Automates the creation of new API packages.
}
