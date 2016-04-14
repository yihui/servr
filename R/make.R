#' Serve files under a directory based on GNU Make
#'
#' You can define how and when to rebuild files (such as R Markdown files) using
#' Make rules, e.g. a rule \command{_posts/\%.md: _source/\%.Rmd} with a command
#' to build \file{.Rmd} to \file{.md} will be executed if and only if
#' \file{foo.Rmd} is newer than \file{foo.md}. The exit status of the command
#' \command{make -q} will decide whether to rebuild files: rebuilding occurs
#' only when the exit code is not \code{0}. When an HTML file has been rebuilt,
#' it will be automatically refreshed in the web browser.
#' @inheritParams httd
#' @inheritParams server_config
#' @references If you are not familiar with GNU Make, I recommend you to learn
#'   it from Karl Broman's minimal tutorial
#'   \url{http://kbroman.org/minimal_make/}.
#' @note You must have installed GNU Make to use this function. This is normally
#'   not a problem for Linux and OS X users (it should be available by default).
#'   For Windows users, you can either install GNU Make, or just install
#'   \href{https://cran.r-project.org/bin/windows/Rtools}{Rtools}, which also
#'   contains GNU Make.
#' @export
#' @examples # some built-in examples (if you are not familiar with make,
#' # you can take a look at the Makefile of each example)
#' servr::serve_example('make1', servr::make)
#' servr::serve_example('make2', servr::make)
make = function(dir = '.', ...) {
  dynamic_site(dir, ..., build = make_maybe)
}

# if need to make, make and return TRUE (to tell the browser to refresh itself);
# otherwise just return FALSE
make_maybe = function(...) {
  if (system('make -q') == 0L) return(FALSE)
  if (system('make') != 0) stop('Failed to run Makefile')
  return(TRUE)
}
