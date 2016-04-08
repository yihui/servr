#' Serve static files under a directory
#'
#' If there is an \file{index.html} under this directory, it will be displayed;
#' otherwise the list of files is displayed, with links on their names. After we
#' run this function, we can go to \samp{http://localhost:port} to browse the
#' web pages either created from R or read from HTML files.
#'
#' \code{httd()} is a pure static server, and \code{httw()} is similar but
#' watches for changes under the directory: if an HTML file is being viewed in
#' the browser, and any files are modified under the directory, the HTML page
#' will be automatically refreshed.
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
  app = list(call = serve_dir(dir))
  res$start_server(app)
}

#' @param pattern a regular expression passed to \code{\link{list.files}()} to
#'   determine the files to watch
#' @param all_files whether to watch all files including the hidden files
#' @param handler a function to be called every time any files are changed or
#'   added under the directory; its argument is a character vector of the
#'   filenames of the files modified or added
#' @rdname httd
#' @export
httw = function(dir = '.', pattern = NULL, all_files = FALSE, handler = NULL, ...) {
  dynamic_site(dir, ..., build = watch_dir(
    '.', pattern = pattern, all_files = all_files, handler = handler
  ))
}

watch_dir = function(dir = '.', pattern = NULL, all_files = FALSE, handler = NULL) {
  dir = normalizePath(dir, mustWork = TRUE)
  mtime = function(dir) {
    file.info(
      list.files(dir, pattern, all.files = all_files, recursive = TRUE, no.. = TRUE)
    )[, 'mtime', drop = FALSE]
  }
  info = mtime(dir)
  function(...) {
    info2 = mtime(dir)
    changed = !identical(info, info2)
    if (changed) {
      if (is.function(handler)) {
        f1 = rownames(info)
        f2 = rownames(info2)
        f3 = setdiff(f2, f1)    # new files
        f4 = intersect(f1, f2)  # old files
        f5 = f4[info[f4, 1] != info2[f4, 1]]  # modified files
        handler(c(f3, na.omit(f5)))
        info2 = mtime(dir)
      }
      info <<- info2
    }
    changed
  }
}

#' Server configurations
#'
#' The server functions in this package are configured through this function.
#' @param dir the root directory to serve
#' @param port the TCP port number; by default it is \code{4321} or a random
#'   port if \code{4321} is not available, unless a command line argument of the
#'   form \code{-pNNNN} (N is a digit from 0 to 9) was passed in when R was
#'   launched, in which case \code{NNNN} will be used as the port number
#' @param browser whether to launch the default web browser; by default, it is
#'   \code{TRUE} if the R session is \code{\link{interactive}()}, or when a
#'   command line argument \code{-b} was passed to R (see
#'   \code{\link{commandArgs}()}); N.B. the RStudio viewer is used as the web
#'   browser if available
#' @param daemon whether to launch a daemonized server (the server does not
#'   block the current R session) or a blocking server; by default, it is
#'   \code{TRUE} if a command line argument \code{-d} was passed to R (through
#'   \command{Rscript}); normally it should be \code{FALSE} by default
#' @param interval the time interval used to check if an HTML page needs to be
#'   rebuilt (by default, it is checked every second); at the moment, the
#'   smallest possible \code{interval} is set to be 1, and this may change in
#'   the future
#' @param baseurl the base URL (the full URL will be
#'   \code{http://host:port/baseurl})
#' @inheritParams httpuv::startServer
#' @return A list of configuration information of the form \code{list(host,
#'   port, start_server = function(app) {}, ...)}.
server_config = function(
  dir, host = '127.0.0.1', port, browser, daemon, interval = 1, baseurl = ''
) {
  cargs = commandArgs(TRUE)
  if (missing(browser)) browser = interactive() || '-b' %in% cargs || is_rstudio()
  if (missing(port))
    port = if (length(port <- grep('^-p[0-9]{4,}$', cargs, value = TRUE)) == 1)
      as.integer(sub('^-p', '', port)) else random_port(4321L)
  if (missing(daemon)) daemon = '-d' %in% cargs
  damn_library('methods')
  url = sprintf('http://%s:%d', host, port)
  if (baseurl != '') url = paste(url, baseurl, sep = '')
  list(
    host = host,
    port = port,
    interval = interval,
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

serve_dir = function(dir = '.') function(req) {
  owd = setwd(dir); on.exit(setwd(owd))
  path = req$PATH_INFO
  status = 200L

  if (grepl('^/', path)) {
    path = paste('.', path, sep = '')  # the requested file
  } else if (path == '') path = '.'
  body = if (file_test('-d', path)) {
    # ensure a trailing slash if the requested dir does not have one
    if (path != '.' && !grepl('/$', path)) return(list(
      status = 301L, body = '', headers = list(
        'Location' = sprintf('/%s/', req$PATH_INFO)
      )
    ))
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

    type = guess_type(path)
    range = req$HTTP_RANGE

    if (is.null(range)) {
      readBin(path, 'raw', file.info(path)[, 'size'])
    } else {
      range = strsplit(range, split = "(=|-)")
      firstByte = as.numeric(range[[1]][2])
      lastByte = as.numeric(range[[1]][3])

      if ((range[[1]][1] != "bytes") ||
          (firstByte >= lastByte) ||
          (lastByte == 0))
        return(list(status = 416L, headers = list('Content-Type' = 'text/plain'),
                    body = 'Requested range not satisfiable\r\n'))

      status = 206L  # partial content

      con = file(path, open = "rb", raw = TRUE)
      on.exit(close(con))
      seek(con, where = firstByte, origin = "start")
      readBin(con, 'raw', lastByte - firstByte)
    }
  }
  if (is.character(body) && length(body) > 1) body = paste(body, collapse='\r\n')
  list(status = status, headers = list('Content-Type' = type), body = body)
}
