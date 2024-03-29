% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wkb.R
\name{new_wk_wkb}
\alias{new_wk_wkb}
\alias{validate_wk_wkb}
\alias{is_wk_wkb}
\title{S3 Details for wk_wkb}
\usage{
new_wk_wkb(x = list(), crs = NULL, geodesic = NULL)

validate_wk_wkb(x)

is_wk_wkb(x)
}
\arguments{
\item{x}{A (possibly) \code{\link[=wkb]{wkb()}} vector}

\item{crs}{A value to be propagated as the CRS for this vector.}

\item{geodesic}{\code{TRUE} if edges must be interpolated as geodesics when
coordinates are spherical, \code{FALSE} otherwise.}
}
\description{
S3 Details for wk_wkb
}
