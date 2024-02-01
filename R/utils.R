#' @import stats utils
#' @importFrom xfun in_dir

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

is_rstudio = function() {
  requireNamespace('rstudioapi', quietly = TRUE) && rstudioapi::isAvailable()
}

# use the RStudio viewer if possible
get_browser = function() {
  if (is_rstudio()) rstudioapi::viewer else getOption('browser')
}

pkg_file = function(...) {
  system.file('resources', ..., package = 'servr', mustWork = TRUE)
}

rscript = function(code, input) {
  if (system2(file.path(R.home('bin'), 'Rscript'), code, stdout = NULL) != 0)
    stop('Failed to compile ', input, call. = FALSE)
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

servrEnv$daemon_list = list()

# a hint on how to stop the daemonized server
daemon_hint = function(server) {
  if (!interactive()) return(invisible(server))
  i = length(servrEnv$daemon_list) + 1
  servrEnv$daemon_list[[i]] = server
  message('To stop the server, run servr::daemon_stop(', i, ') or restart your R session')
  invisible(server)
}

#' Utilities for daemonized servers
#'
#' \code{daemon_list()} returns IDs of servers, which can be used to stop the
#' daemonized servers.
#' @param which A integer vector of the server IDs; by default, IDs of all
#'   existing servers in the current R session obtained from
#'   \code{daemon_list()}, i.e., all daemon servers will be stopped by default.
#' @return  The function \code{daemon_list()} returns a list of existing server
#'   IDs, and \code{daemon_stop()} returns an invisible \code{NULL}.
#' @export
daemon_stop = function(which = daemon_list()) {
  for (d in which) {
    if (length(s <- servrEnv$daemon_list[[d]]) == 0) next
    stopServer(s)
    servrEnv$daemon_list[[d]] = list()
  }
}
#' @rdname daemon_stop
#' @export
daemon_list = function() {
  x = seq_along(servrEnv$daemon_list)
  d = integer()
  for (i in x) if (length(servrEnv$daemon_list[[i]]) == 0) d = c(d, i)
  if (length(d)) x = x[-d]
  if (length(x)) x else invisible(x)
}

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
    m1 = info[rownames(info2), 'mtime']
    yes = anyNA(m1) || any(info2[, 'mtime'] > m1)
    on.exit(info <<- source_info(), add = TRUE)
    if (yes) build(path)
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

#' Find a random available TCP port
#'
#' Test a series of random TCP ports from 3000 to 8000 (excluding a few that are
#' considered unsafe by Chrome) and return the first available one. A web server
#' can be later started on this port.
#' @param port The preferred port(s).
#' @param n The maximum number of random ports to be tested.
#' @param exclude A vector of port numbers not to be considered.
#' @inheritParams server_config
#' @export
#' @return A port number, or an error if no ports are available.
random_port = function(
  port = 4321L, host = getOption('servr.host', '127.0.0.1'), n = 20,
  exclude = NULL
) {
  # exclude ports considered unsafe by Chrome http://superuser.com/a/188070
  ports = sample(setdiff(3000:8000, c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)), n)
  ports = setdiff(c(port, ports), exclude)
  port = NULL
  # when a port has been used on 0.0.0.0, port_available() still returns TRUE
  # when the same port is tested on 127.0.0.1, which might be a bug of httpuv;
  # so we provide an option to test the availability of a port on 0.0.0.0 when
  # we intend to serve a site on 127.0.0.1
  test0 = host == '127.0.0.1' && getOption('servr.test.0.0.0.0', TRUE)
  for (p in ports) if (port_available(p, host) && (!test0 || port_available(p, '0.0.0.0'))) {
    port = p
    break
  }
  if (is.null(port)) stop("Cannot find an available TCP port")
  port
}

port_available = function(port, host = getOption('servr.host', '127.0.0.1')) {
  tmp = try(startServer(host, port, list(), quiet = TRUE), silent = TRUE)
  if (inherits(tmp, 'try-error')) return(FALSE)
  stopServer(tmp)
  TRUE
}

# decode the requested path
decode_path = function(req) {
  path = httpuv::decodeURIComponent(req$PATH_INFO)
  Encoding(path) = 'UTF-8'
  path
}

paste2 = function(...) paste(c(...), collapse = '\n')

file_size = function(path) file.info(path)[, 'size']

# store the last browsing function, so that we can reopen a page after it has
# been closed in the browser
servrEnv$browse = function(reopen = TRUE) {
  message('It seems you have not served any content with servr yet.')
}

#' Reopen the last browsed page
#'
#' If you have launched a page in the browser via \pkg{servr} but closed it
#' later, you may call this function to reopen it.
#' @param open Whether to reopen the lastly browsed page. If \code{FALSE}, the
#'   URL of the previously browsed page will be returned.
#' @export
#' @examples servr::browse_last()
browse_last = function(open = TRUE) servrEnv$browse(open)

#' Generate Basic authentication strings
#'
#' Combine usernames with passwords with colons, and generate base64-encoded
#' strings to be used for user authentication.
#' @param user A vector of usernames.
#' @param password A vector of passwords.
#' @export
#' @return A character vector of encoded credentials.
#' @examples
#' servr::auth_basic('foo', 'B@R')
auth_basic = function(user, password) {
  p = paste(user, password, sep = ':')
  x = vapply(p, function(z) xfun::base64_encode(charToRaw(z)), character(1))
  paste('Basic', x)
}

# verify credentials
auth_verify = function(req, auth) {
  is.null(auth$scheme) || is.null(auth$creds) ||
    (is.character(a <- req$HTTP_AUTHORIZATION) && length(a) == 1 && (a %in% auth$creds))
}
