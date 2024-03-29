% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-data-frame.R, R/pkg-sf.R
\name{wk_handle.data.frame}
\alias{wk_handle.data.frame}
\alias{wk_restore.data.frame}
\alias{wk_restore.tbl_df}
\alias{wk_translate.data.frame}
\alias{wk_translate.tbl_df}
\alias{wk_translate.sf}
\alias{wk_restore.sf}
\title{Use data.frame with wk}
\usage{
\method{wk_handle}{data.frame}(handleable, handler, ...)

\method{wk_restore}{data.frame}(handleable, result, ...)

\method{wk_restore}{tbl_df}(handleable, result, ...)

\method{wk_translate}{data.frame}(handleable, to, ...)

\method{wk_translate}{tbl_df}(handleable, to, ...)

\method{wk_translate}{sf}(handleable, to, ...)

\method{wk_restore}{sf}(handleable, result, ...)
}
\arguments{
\item{handleable}{A geometry vector (e.g., \code{\link[=wkb]{wkb()}}, \code{\link[=wkt]{wkt()}}, \code{\link[=xy]{xy()}},
\code{\link[=rct]{rct()}}, or \code{\link[sf:sfc]{sf::st_sfc()}}) for which \code{\link[=wk_handle]{wk_handle()}} is defined.}

\item{handler}{A \link[=wk_handle]{wk_handler} object.}

\item{...}{Passed to the \code{\link[=wk_handle]{wk_handle()}} method.}

\item{result}{The result of a filter operation intended to be a
transformation.}

\item{to}{A prototype object.}
}
\description{
Use data.frame with wk
}
\examples{
wk_handle(data.frame(a = wkt("POINT (0 1)")), wkb_writer())
wk_translate(wkt("POINT (0 1)"), data.frame(col_name = wkb()))
wk_translate(data.frame(a = wkt("POINT (0 1)")), data.frame(wkb()))

}
