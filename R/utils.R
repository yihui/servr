#' @import stats utils

servrEnv = new.env(parent = emptyenv())

# turn file.info() to an HTML table
fileinfo_table = function(info) {
  info = info[order(info$isdir, decreasing = TRUE), ]
  d = info$isdir; i = !is.na(d)
  # files/dirs
  x1 = paste(basename(rownames(info)), ifelse(d & i, '/', ''), sep = '')
  x1 = escape_html(x1)
  x1[i] = sprintf('<a href="%s">%s</a>', x1[i], x1[i])
  # size
  x2 = paste(format(info$size, scientific = FALSE, big.mark = ','), 'B')
  x2[is.na(info$size) | d] = ''
  # date modified
  x3 = as.character(info$mtime)
  x3[is.na(x3)] = ''
  c('<table>',
    '<thead><tr>',
    sprintf('<th>%s</th>', c('Name', 'Size', 'Date Modified')),
    '</tr></thead>',
    apply(cbind(
      '<tr>',
      sprintf('<td>%s</td>', x1),
      sprintf('<td align="right">%s</td>', x2),
      sprintf('<td>%s</td>', x3),
      '</tr>'), 1, paste, collapse = ''),
    '</table>')
}

# make an HTML document from body and title
html_doc = function(body, title = NULL) {
  c('<!DOCTYPE html>', '<html>',
    '<head>', sprintf('<title>%s</title>', title), '</head>',
    '<body>', body, '</body>', '</html>')
}

# escape special HTML chars (copied from highr)
escape_html = function(x) {
  x = gsub('&', '&amp;', x)
  x = gsub('<', '&lt;', x)
  x = gsub('>', '&gt;', x)
  x = gsub('"', '&quot;', x)
  x
}

# sorry for being impolite, but the business of Depends/Imports/library()/R CMD
# check/... is becoming more and more confusing, although I understand the good
# intention; sometimes methods does not work even you import it in namespace
# (there are weird animals in Ref Classes), and R hates library() after you
# Depends; all in all, let's stay with what is working, and forget about the R
# CMD check fuss (whoever sees this function please stay calm and try not to
# discuss it)
damn_library = function(pkg) library(pkg, character.only = TRUE)

is_rstudio = function() Sys.getenv('RSTUDIO') == '1'

# use the RStudio viewer if possible
get_browser = function() {
  browser = if ('tools:rstudio' %in% search()) getOption('viewer') else {
    if (is_rstudio()) getFromNamespace('viewer', 'rstudioapi')
  }
  # rstudio::viewer() does not seem to work when a separate R session is
  # launched from RStudio, so we need to try() and if it fails, fall back to the
  # default web browser
  if (is.null(browser) || !is.function(browser) ||
        inherits(try(browser('http://www.rstudio.com'), silent = TRUE), 'try-error'))
    browser = getOption('browser')
  browser
}

rscript = function(code, input) {
  if (system2(file.path(R.home('bin'), 'Rscript'), code, stdout = NULL) != 0)
    stop('Failed to compile ', input, call. = FALSE)
}

in_dir = function(dir, expr) {
  owd = setwd(dir); on.exit(setwd(owd))
  expr
}

new_timeout = function(interval, max_timeout = 32) {
  old = NULL
  int = interval  # stores initial value (may need to restore later)
  function(delay = FALSE) {
    now = Sys.time()
    if (is.null(old)) old <<- now
    # when delay = TRUE, wait for twice longer until returning TRUE (like Gmail)
    if (!is.na(delay)) {
      if (delay) {
        int <<- min(2 * int, max_timeout)
        message('\n* Retrying in ', int, ' second', if (int > 1) 's', '...\n')
      } else int <<- interval
    }
    if (as.numeric(now - old) < int) return(FALSE)
    old <<- now
    TRUE
  }
}

# use the output from the system utility mimetype if available
guess_type = function(path) {
  mimetype = function(...) {
    system2('mimetype', c('-b', shQuote(path)), ...)
  }
  if (Sys.which('mimetype') == '' || mimetype(stdout = NULL) != 0)
    return(mime::guess_type(path))
  mimetype(stdout = TRUE)
}

servrEnv$daemon_list = NULL

# a hint on how to stop the daemonized server
daemon_hint = function(server) {
  if (!interactive()) return(invisible(server))
  servrEnv$daemon_list = c(servrEnv$daemon_list, server)
  message('To stop the server, run servr::daemon_stop("', server, '")',
          ' or restart your R session')
  invisible(server)
}

#' Utilities for daemonized servers
#'
#' The server functions in this package will return server handles if daemonized
#' servers were used (e.g., \code{servr::httd(daemon = TRUE)}). You can pass the
#' handles to \code{daemon_stop()} to stop the daemonized servers. Because
#' stopping a daemonized server more than once using
#' \code{httpuv::\link{stopDaemonizedServer}()} will crash the R session, this
#' function will check if a server has been stopped before really attempting to
#' stop it, so it is safer than the \code{stopDaemonizedServer()} in
#' \pkg{httpuv}.
#' @param which the server handles returned by server functions; by default, all
#'   existing handles in the current R session obtained from
#'   \code{daemon_list()}, i.e., all daemon servers will be stopped by default
#' @return  The function \code{daemon_list()} returns a list of existing server
#'   handles, and \code{daemon_stop()} returns an invisible \code{NULL}.
#' @export
#' @importFrom httpuv stopDaemonizedServer
daemon_stop = function(which = daemon_list()) {
  list = daemon_list()
  for (d in which) {
    if (!(d %in% list)) {
      warning('The server ', d, ' has been stopped!')
      next
    }
    stopDaemonizedServer(d)
    servrEnv$daemon_list = setdiff(servrEnv$daemon_list, d)
  }
}
#' @rdname daemon_stop
#' @export
daemon_list = function() servrEnv$daemon_list

# watch files with pattern, and rebuild them if necessary
build_watcher = function(pattern, build, dir = getwd()) {
  source_info = function() {
    file.info(list.files(dir, pattern, recursive = TRUE))[, 'mtime', drop = FALSE]
  }
  info = source_info()
  function(message) {
    if (missing(message) || !is.list(message)) return(FALSE)
    path = sub('^/', '', message$pathname)
    if (!is.character(path) || length(path) != 1 || !file.exists(path))
      return(FALSE)
    if (!grepl(pattern, path)) return(FALSE)
    info2 = source_info()
    yes = !(path %in% rownames(info)) || info2[path, 'mtime'] > info[path, 'mtime']
    if (yes) build(path)
    info <<- source_info()
    yes
  }
}

#' A convenience function to serve examples in this package
#'
#' Use server functions to serve built-in examples of this package.
#' @param name the directory name of the example under the directory
#'   \code{system.file('examples', package = 'servr')}
#' @param FUN a server function that takes the example path as its first
#'   argument, e.g. \code{\link{httd}}, or \code{\link{rmdv1}}
#' @param ... other arguments passed to \code{FUN}
#' @param run whether to run the example (this is mainly for \command{R CMD
#'   check} purposes: the examples will not be really served when the R session
#'   is not interactive, so they will not block \command{R CMD check})
#' @export
#' @return \code{NULL} if \code{run = FALSE}, otherwise the value returned from
#'   \code{FUN()}.
#' @examples # R Markdown v1 or v2
#' servr::serve_example('rmd', servr::rmdv1)
#' servr::serve_example('rmd', servr::rmdv2)
#'
#' # GNU Make
#' servr::serve_example('make1', servr::make)
#' servr::serve_example('make2', servr::make)
serve_example = function(name, FUN, ..., run = interactive()) {
  if (!run) return(invisible(NULL))
  dir = system.file('examples', name, package = 'servr')
  if (!file_test('-d', dir)) stop('The example ', name, ' does not exist')
  message('Serving the example ', dir)
  FUN(dir, ...)
}

# find a random available TCP port (to launch server)
random_port = function(port = 4321L, host = '127.0.0.1') {
  ports = c(port, sample(3000:8000, 20))
  port = NULL
  for (p in ports) if (port_available(p, host)) {
    port = p
    break
  }
  if (is.null(port)) stop("Cannot find an available TCP port")
  port
}

port_available = function(port, host = '127.0.0.1') {
  tmp = try(httpuv::startServer(host, port, list()), silent = TRUE)
  if (inherits(tmp, 'try-error')) return(FALSE)
  httpuv::stopServer(tmp)
  TRUE
}
