% Please edit documentation in R/static.R
\name{httd}
\alias{httd}
\alias{httw}
\title{Serve the static files under a directory}
\usage{
httd(dir = ".", ...)

httw(dir = ".", pattern = NULL, build = NULL, ...)
}
\arguments{
\item{dir}{the root directory to serve}

\item{...}{server configurations passed to \code{\link{server_config}()}}
}
\description{
If there is an \file{index.html} under this directory, it will be displayed;
otherwise the list of files is displayed, with links on their names. After we
run this function, we can go to \samp{http://localhost:port} to browse the
web pages either created from R or read from HTML files.
}
\details{
\code{httd()} is a pure static server, and \code{httw()} is similar but
watches for changes under the directory: if an HTML file is being viewed in
the browser, and any files are modified under the directory, the HTML page
will be automatically refreshed.
}
\examples{
#' see https://github.com/yihui/servr for command line usage
# or run inside an R session
if (interactive()) servr::httd()
}
\references{
\url{https://github.com/yihui/servr}
}
