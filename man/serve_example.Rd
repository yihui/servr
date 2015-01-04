% Please edit documentation in R/utils.R
\name{serve_example}
\alias{serve_example}
\title{A convenience function to serve examples in this package}
\usage{
serve_example(name, FUN, ..., run = interactive())
}
\arguments{
\item{name}{the directory name of the example under the directory
\code{system.file('examples', package = 'servr')}}

\item{FUN}{a server function that takes the example path as its first
argument, e.g. \code{\link{httd}}, or \code{\link{rmdv1}}}

\item{...}{other arguments passed to \code{FUN}}

\item{run}{whether to run the example (this is mainly for \command{R CMD
check} purposes: the examples will not be really served when the R session
is not interactive, so they will not block \command{R CMD check})}
}
\value{
\code{NULL} if \code{run = FALSE}, otherwise the value returned from
  \code{FUN()}.
}
\description{
Use server functions to serve built-in examples of this package.
}
\examples{
# R Markdown v1 or v2
servr::serve_example("rmd", servr::rmdv1)
servr::serve_example("rmd", servr::rmdv2)

# GNU Make
servr::serve_example("make1", servr::make)
servr::serve_example("make2", servr::make)
}
