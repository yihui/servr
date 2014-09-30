\name{jekyll}
\alias{jekyll}
\alias{rmdv1}
\alias{rmdv2}
\title{Serve R Markdown based websites}
\usage{
jekyll(dir = ".", input = c(".", "_source", "_posts"), output = c(".", "_posts", 
    "_posts"), script = "build.R", baseurl, port, launch.browser)

rmdv2(dir = ".", script = "build.R", port, launch.browser)

rmdv1(dir = ".", script = "build.R", port, launch.browser)
}
\arguments{
\item{dir}{the root directory of the website}

\item{input}{the input directories that contain R Markdown documents (the
directories must be relative instead of absolute; same for \code{output}
directories)}

\item{output}{the output directories corresponding to \code{input}; for an
input document \file{foo.Rmd} under the directory \code{input[i]}, its
output document \file{foo.md} (or \file{foo.html}) is generated under
\code{output[i]} if the output document is older than the input document}

\item{script}{the name of an R script to re-build R Markdown documents; it
will be executed via command line of the form \command{Rscript build.R arg1
arg2} where \code{build.R} is the script specified by this argument,
\code{arg1} is the input filename, and \code{arg2} is the output filename;
inside the R script, you can use \code{\link{commandArgs}(TRUE)} to capture
\code{c(arg1, arg2)}, e.g. \code{knitr::knit(commandArgs(TRUE)[1],
commandArgs(TRUE)[2])}}

\item{baseurl}{the base URL for Jekyll (will be read from \file{_config.yml}
if missing)}

\item{port}{the TCP port number; by default it is \code{4321}, unless a
command line argument of the form \code{-pNNNN} (N is a digit from 0 to 9)
was passed in when R was launched, in which case \code{NNNN} will be used
as the port number}

\item{launch.browser}{whether to launch the default web browser; by default,
it is \code{TRUE} if the R session is \code{\link{interactive}()}, or when
a command line argument \code{-b} was passed to R (see
\code{\link{commandArgs}()}); N.B. the RStudio viewer is used as the web
browser if available}
}
\description{
R Markdown documents (with the filename extension \file{.Rmd}) are
re-compiled using \pkg{knitr} or \pkg{rmarkdown} when necessary (source files
are newer than output files), and the HTML pages will be automatically
refreshed in the web browser accordingly.
}
\details{
The function \code{jekyll()} sets up a web server to serve a Jekyll-based
website. A connection is established between R and the HTML pages through
WebSockets so that R can notify the HTML pages to refresh themselves if any R
Markdown documents have been re-compiled.

The functions \code{rmdv1()} and \code{rmdv2()} are similar to
  \code{jekyll()}, and the only difference is the way to compile R Markdown
  documents: \code{rmdv1()} uses the \pkg{markdown} package (a.k.a R Markdown
  v1) via \code{\link[knitr]{knit2html}()}, and \code{rmdv2()} calls
  \code{\link[rmarkdown]{render}()} in the \pkg{rmarkdown} package (a.k.a R
  Markdown v2).
}
\note{
Apparently \code{jekyll()} and \code{rmdv1()} require the \pkg{knitr}
  package, and \code{rmdv2()} requires \pkg{rmarkdown}. You have to install
  them before calling the server functions here.

  All R Markdown documents are compiled in separate R sessions. If you have
  any R Markdown documents that should not be compiled as standalone
  documents (e.g. child documents), you can use different filename
  extensions, such as \file{.Rmarkdown}.
}
\examples{
if (interactive()) servr::rmdv1()  # serve the current dir with R Markdown v1
if (interactive()) servr::rmdv2()  # or R Markdown v2
}
\references{
R Markdown v1: \url{http://cran.rstudio.com/package=markdown}. R
  Markdown v2: \url{http://rmarkdown.rstudio.com}. For Jekyll, see
  \url{http://jekyllrb.com}. The GitHub repository
  \url{https://github.com/yihui/knitr-jekyll} is an example of serving Jekyll
  websites with \code{servr::jekyll()}.
}
