\name{server_config}
\alias{server_config}
\title{Server configurations}
\usage{
server_config(dir, host = "127.0.0.1", port, browser, daemon)
}
\arguments{
\item{dir}{the root directory to serve}

\item{port}{the TCP port number; by default it is \code{4321}, unless a
command line argument of the form \code{-pNNNN} (N is a digit from 0 to 9)
was passed in when R was launched, in which case \code{NNNN} will be used
as the port number}

\item{browser}{whether to launch the default web browser; by default, it is
\code{TRUE} if the R session is \code{\link{interactive}()}, or when a
command line argument \code{-b} was passed to R (see
\code{\link{commandArgs}()}); N.B. the RStudio viewer is used as the web
browser if available}

\item{daemon}{whether to launch a daemonized server (the server does not
block the current R session) or a blocking server; by default, it is
\code{TRUE} if a command line argument \code{-d} was passed to R (through
\command{Rscript}); normally it should be \code{FALSE} by default}
}
\value{
A list of configuration information of the form \code{list(host,
  port, start_server = function(app) {}, ...)}.
}
\description{
The server functions in this package are configured through this function.
}
