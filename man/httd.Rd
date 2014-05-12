\name{httd}
\alias{httd}
\title{Serve the static files under a directory}
\usage{
httd(dir = ".", port, launch.browser)
}
\arguments{
\item{dir}{the directory to serve}

\item{port}{the TCP port number; by default it is \code{4321}, unless a
command line argument of the form \code{-pNNNN} (N is a digit from 0 to 9)
was passed in when R was launched, in which case \code{NNNN} will be used
as the port number}

\item{launch.browser}{whether to launch the default web browser; by default,
it is \code{TRUE} if the R session is \code{\link{interactive}()}, or when
a command line argument \code{-b} was passed to R}
}
\description{
If there is an \file{index.html} under this directory, it will be displayed;
otherwise the list of files is displayed, with links on their names. After we
run this function, we can go to \url{http://localhost:port} to browse the web
pages either created from R or read from HTML files.
}
\examples{
#' see https://github.com/yihui/servr for command line usage
# or run inside an R session
if (interactive()) servr::httd()
}
\references{
\url{https://github.com/yihui/servr}
}
