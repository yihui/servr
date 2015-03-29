% Please edit documentation in R/static.R
\name{httd}
\alias{httd}
\title{Serve the static files under a directory}
\usage{
httd(dir = ".", ...)
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
\examples{
#' see https://github.com/yihui/servr for command line usage
# or run inside an R session
if (interactive()) servr::httd()
}
\references{
\url{https://github.com/yihui/servr}
}
