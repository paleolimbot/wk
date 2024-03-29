% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orient-filter.R
\name{wk_orient}
\alias{wk_orient}
\alias{wk_orient_filter}
\alias{wk_clockwise}
\alias{wk_counterclockwise}
\title{Orient polygon coordinates}
\usage{
wk_orient(handleable, ..., direction = wk_counterclockwise())

wk_orient_filter(handler, direction = wk_counterclockwise())

wk_clockwise()

wk_counterclockwise()
}
\arguments{
\item{handleable}{A geometry vector (e.g., \code{\link[=wkb]{wkb()}}, \code{\link[=wkt]{wkt()}}, \code{\link[=xy]{xy()}},
\code{\link[=rct]{rct()}}, or \code{\link[sf:sfc]{sf::st_sfc()}}) for which \code{\link[=wk_handle]{wk_handle()}} is defined.}

\item{...}{Passed to the \code{\link[=wk_handle]{wk_handle()}} method.}

\item{direction}{The winding polygon winding direction}

\item{handler}{A \link[=wk_handle]{wk_handler} object.}
}
\value{
\code{handleable} with consistently oriented polygons, in \code{direction}
winding order.
}
\description{
Orient polygon coordinates
}
\examples{
wk_orient(wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
wk_orient(
  wkt("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))"),
  direction = wk_clockwise()
)

}
