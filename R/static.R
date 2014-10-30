#' Serve the static files under a directory
#'
#' If there is an \file{index.html} under this directory, it will be displayed;
#' otherwise the list of files is displayed, with links on their names. After we
#' run this function, we can go to \url{http://localhost:port} to browse the web
#' pages either created from R or read from HTML files.
#' @inheritParams server_config
#' @param ... server configurations passed to \code{\link{server_config}()}
#' @export
#' @importFrom httpuv runServer startDaemonizedServer
#' @references \url{https://github.com/yihui/servr}
#' @examples #' see https://github.com/yihui/servr for command line usage
#' # or run inside an R session
#' if (interactive()) servr::httd()
httd = function(dir = '.', ...) {
  dir = normalizePath(dir, mustWork = TRUE)
  if (dir != '.') {
    owd = setwd(dir); on.exit(setwd(owd))
  }
  res = server_config(dir, ...)
  res$browse()
  app = list(call = serve_dir(req, dir))
  res$start_server(app)
}

#' Server configurations
#'
#' The server functions in this package are configured through this function.
#' @param dir the root directory to serve
#' @param port the TCP port number; by default it is \code{4321}, unless a
#'   command line argument of the form \code{-pNNNN} (N is a digit from 0 to 9)
#'   was passed in when R was launched, in which case \code{NNNN} will be used
#'   as the port number
#' @param browser whether to launch the default web browser; by default, it is
#'   \code{TRUE} if the R session is \code{\link{interactive}()}, or when a
#'   command line argument \code{-b} was passed to R (see
#'   \code{\link{commandArgs}()}); N.B. the RStudio viewer is used as the web
#'   browser if available
#' @param daemon whether to launch a daemonized server (the server does not
#'   block the current R session) or a blocking server; by default, it is
#'   \code{TRUE} if a command line argument \code{-d} was passed to R (through
#'   \command{Rscript}); normally it should be \code{FALSE} by default
#' @inheritParams startServer
#' @return A list of configuration information of the form \code{list(host,
#'   port, start_server = function(app) {}, ...)}.
server_config = function(dir, host = '127.0.0.1', port, browser, daemon) {
  cargs = commandArgs(TRUE)
  if (missing(browser)) browser = interactive() || '-b' %in% cargs || is_rstudio()
  if (missing(port))
    port = if (length(port <- grep('^-p[0-9]{4,}$', cargs, value = TRUE)) == 1)
      as.integer(sub('^-p', '', port)) else 4321L
  if (missing(daemon)) daemon = '-d' %in% cargs
  damn_library('methods')
  url = sprintf('http://%s:%d', host, port)
  list(
    host = host,
    port = port,
    start_server = function(app) {
      # a daemonized server; stop it using servr::daemon_stop()
      if (daemon) return(daemon_hint(startDaemonizedServer(host, port, app)))

      server = startServer(host, port, app)
      on.exit(stopServer(server), add = TRUE)

      while (TRUE) {
        service()
        Sys.sleep(0.001)
      }
    },
    browse = function() {
      if (browser) {
        browseURL(url, browser = get_browser())
      } else message('serving the directory ', dir, ' at ', url)
    }
  )
}

serve_dir = function(req, dir = '.') function(req) {
  owd = setwd(dir); on.exit(setwd(owd))
  path = paste('.', req$PATH_INFO, sep = '')  # the requested file
  body = if (file_test('-d', path)) {
    type = 'text/html'
    if (file.exists(idx <- file.path(path, 'index.html'))) readLines(idx) else {
      d = file.info(list.files(path, all.files = TRUE, full.names = TRUE))
      title = escape_html(path)
      html_doc(c(sprintf('<h1>Index of %s</h1>', title), fileinfo_table(d)),
               title = title)
    }
  } else {
    if (!file.exists(path))
      return(list(status = 404L, headers = list('Content-Type' = 'text/plain'),
                  body = paste('Not found:', path, '\r\n')))
    type = mime::guess_type(path)
    readBin(path, 'raw', file.info(path)[, 'size'])
  }
  if (is.character(body) && length(body) > 1) body = paste(body, collapse='\r\n')
  list(status = 200L, headers = list('Content-Type' = type), body = body)
}
