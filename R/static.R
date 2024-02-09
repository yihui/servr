#' Create a server
#'
#' Create a server with a custom handler to handle the HTTP request.
#' @param ... Arguments to be passed to \code{\link{server_config}()}.
#' @param handler A function that takes the HTTP request and returns a response.
#' @param ws_open A function to be called back when a WebSocket connection is
#'   established (see \code{httpuv::\link{startServer}()}).
#' @export
#' @examplesIf interactive()
#' # always return "Success:" followed by the requested path
#' s = servr::create_server(handler = function(req) {
#'   list(status = 200L, body = paste('Success:', req$PATH_INFO))
#' })
#' s$url
#'
#' browseURL(paste0(s$url, '/hello'))
#' browseURL(paste0(s$url, '/world'))
#'
#' s$stop_server()
create_server = function(..., handler, ws_open = function(ws) NULL) {
  res = server_config(...)
  app = list(call = handler, onWSOpen = ws_open)
  res$start_server(app)
  invisible(res)
}

#' Serve static files under a directory
#'
#' If there is an \file{index.html} under this directory, it will be displayed;
#' otherwise the list of files is displayed, with links on their names. After we
#' run this function, we can go to \samp{http://localhost:port} to browse the
#' web pages either created from R or read from HTML files.
#'
#' \code{httd()} is a static file server by default (its \code{response}
#' argument can turn it into a dynamic file server), and \code{httw()} is
#' similar but watches for changes under the directory: if an HTML file is being
#' viewed in the browser, and any files are modified under the directory, the
#' HTML page will be automatically refreshed.
#' @inheritParams server_config
#' @param ... server configurations passed to \code{\link{server_config}()}
#' @param response A function of the form \code{function(path, res, ...)} that
#'   takes a file path and server response as input, and return a new response.
#'   This can be useful for post-processing the response (for experts only).
#' @export
#' @references \url{https://github.com/yihui/servr}
#' @examplesIf interactive()
#' servr::httd()
httd = function(dir = '.', ..., response = NULL) {
  dir = normalizePath(dir, mustWork = TRUE)
  if (dir != '.') {
    owd = setwd(dir); on.exit(setwd(owd))
  }
  create_server(dir, ..., handler = serve_dir(dir, response))
}

#' @param watch a directory under which \code{httw()} is to watch for changes;
#'   if it is a relative path, it is relative to the \code{dir} argument
#' @param pattern a regular expression passed to \code{\link{list.files}()} to
#'   determine the files to watch
#' @param all_files whether to watch all files including the hidden files
#' @param filter a function to filter the file paths returned from
#'   \code{list.files()} (e.g., you can exclude certain files from the watch
#'   list)
#' @param handler a function to be called every time any files are changed or
#'   added under the directory; its argument is a character vector of the
#'   filenames of the files modified or added
#' @rdname httd
#' @export
httw = function(
  dir = '.', watch = '.', pattern = NULL, all_files = FALSE, filter = NULL,
  handler = NULL, ...
) {
  dynamic_site(dir, ..., build = watch_dir(
    watch, pattern = pattern, all_files = all_files, filter = filter, handler = handler
  ))
}

watch_dir = function(
  dir = '.', pattern = NULL, all_files = FALSE, filter = NULL, handler = NULL
) {
  cwd = getwd()
  mtime = function(dir) {
    owd = setwd(cwd); on.exit(setwd(owd), add = TRUE)
    info = file.info(list.files(
      dir, pattern, all.files = all_files, full.names = TRUE, recursive = TRUE,
      no.. = TRUE
    ))[, 'mtime', drop = FALSE]
    if (is.function(filter)) info = info[filter(rownames(info)), , drop = FALSE]
    rownames(info) = gsub('^[.]/', '', rownames(info))
    info
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
        info <<- info2
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
#' @param dir The root directory to serve.
#' @param port The TCP port number. If it is not explicitly set, the default
#'   value will be looked up in this order: First, the command line argument of
#'   the form \code{-pNNNN} (N is a digit from 0 to 9). If it was passed to R
#'   when R was started, \code{NNNN} will be used as the port number. Second,
#'   the environment variable \code{R_SERVR_PORT}. Third, the global option
#'   \code{servr.port} (e.g., \code{options(servr.port = 4322)}). If none of
#'   these command-line arguments, variables, or options were set, the default
#'   port will be \code{4321}. If this port is not available, a random available
#'   port will be used.
#' @param browser Whether to launch the default web browser. By default, it is
#'   \code{TRUE} if the R session is \code{\link{interactive}()}, or when a
#'   command line argument \code{-b} was passed to R (see
#'   \code{\link{commandArgs}()}). N.B. the RStudio viewer is used as the web
#'   browser if available.
#' @param daemon Whether to launch a daemonized server (the server does not
#'   block the current R session) or a blocking server. By default, it is the
#'   global option \code{getOption('servr.daemon')} (e.g., you can set
#'   \code{options(servr.daemon = TRUE)}); if this option was not set,
#'   \code{daemon = TRUE} if a command line argument \code{-d} was passed to R
#'   (through \command{Rscript}), or the server is running in an interactive R
#'   session.
#' @param interval The time interval used to check if an HTML page needs to be
#'   rebuilt (by default, it is checked every second).
#' @param baseurl The base URL (the full URL will be
#'   \code{http://host:port/baseurl}).
#' @param initpath The initial path in the URL (e.g. you can open a specific
#'   HTML file initially).
#' @param hosturl A function that takes the host address and returns a character
#'   string to be used in the URL, e.g., \code{function(host) { if (host ==
#'   '127.0.0.1') 'localhost' else host}} to convert \code{127.0.0.1} to
#'   \code{localhost} in the URL.
#' @param auth A list of the form \code{list(scheme, creds)} containing the
#'   authentication scheme and credentials. See
#'   \url{https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication} for
#'   more info. Please note that this argument is \emph{by no means} intended
#'   for serious HTTP applications and there is \emph{no warranty} on security.
#'   You should use other dedicated software packages or services if security is
#'   important. You have been warned.
#' @param verbose Whether to print messages when launching the server.
#' @inheritParams httpuv::startServer
#' @export
#' @return A list of configuration information of the form \code{list(host,
#'   port, start_server = function(app) {}, ...)}.
#' @examplesIf interactive()
#' # an example of authentication
#' servr::httd(auth = list(scheme = 'Basic', creds = servr::auth_basic('john', 'pa$s!')))
server_config = function(
  dir = '.', host = getOption('servr.host', '127.0.0.1'), port, browser, daemon,
  interval = getOption('servr.interval', 1), baseurl = '', initpath = '',
  hosturl = identity, auth = getOption('servr.auth'), verbose = TRUE
) {
  cargs = commandArgs(TRUE)
  if (missing(browser)) browser = interactive() || '-b' %in% cargs || is_rstudio()
  if (missing(port)) port = if (length(port <- grep('^-p[0-9]{4,}$', cargs, value = TRUE)) == 1) {
    sub('^-p', '', port)
  } else {
    port = Sys.getenv('R_SERVR_PORT', NA)
    if (is.na(port)) getOption('servr.port', random_port()) else port
  }
  port = as.integer(port)
  if (missing(daemon)) daemon = getOption('servr.daemon', ('-d' %in% cargs) || interactive())
  if (!is.numeric(interval)) interval = as.numeric(interval)
  # rstudio viewer cannot display a page served at 0.0.0.0; use 127.0.0.1 instead
  host2 = if (host == '0.0.0.0' && is_rstudio()) '127.0.0.1' else host
  url = sprintf('http://%s:%d', hosturl(host2), port)
  baseurl = gsub('^/*', '/', baseurl)
  if (baseurl != '/') url = paste0(url, baseurl)
  url = paste0(url, if (initpath != '' && !grepl('^/', initpath)) '/', initpath)
  browsed = FALSE
  servrEnv$browse = browse = function(reopen = FALSE) {
    if (browsed && !reopen) return(invisible(url))
    if (browser || reopen) browseURL(url, browser = get_browser())
    browsed <<- TRUE
    if (verbose && !reopen) message('Serving the directory ', dir, ' at ', url)
  }
  server = NULL
  # realm is required for Basic auth (append one in case it was not provided)
  if (identical(tolower(auth$scheme), 'basic'))
    auth$scheme = 'Basic realm="Access restricted"'
  list(
    host = host, port = port, interval = interval, url = url, daemon = daemon,
    start_server = function(app) {
      if (is.function(app_call <- app$call)) {
        app$call = function(req) {
          # authentication
          if (!auth_verify(req, auth)) return(list(
            status = 401L, body = '', headers = list(`WWW-Authenticate` = auth$scheme)
          ))
          req = modify_path(req, baseurl)
          app_call(req)
        }
      }
      id = startServer(host, port, app)
      if (verbose && daemon) daemon_hint(id); browse()
      server <<- id
      if (!daemon) while (TRUE) {
        httpuv::service(); Sys.sleep(0.001)
      }
      invisible(id)
    },
    stop_server = function() {
      if (is.null(server)) stop('The server has not been started yet.')
      stopServer(server)
    },
    browse = browse
  )
}

# modify PATH_INFO in the request when baseurl is provided (remove baseurl)
modify_path = function(req, baseurl) {
  if (baseurl == '/') return(req)
  path = decode_path(req)
  if (startsWith(path, baseurl)) {
    path = substr(path, nchar(baseurl) + 1, nchar(path))
    req$PATH_INFO = httpuv::encodeURIComponent(path)
  }
  req
}

serve_dir = function(dir = '.', response = NULL) function(req) {
  owd = setwd(dir); on.exit(setwd(owd), add = TRUE)
  path = decode_path(req)
  status = 200L

  if (grepl('^/', path)) {
    path = paste('.', path, sep = '')  # the requested file
  } else if (path == '') path = '.'
  body = if (file_test('-d', path)) {
    # ensure a trailing slash if the requested dir does not have one
    if (path != '.' && !grepl('/$', path)) return(redirect(sprintf('%s/', req$PATH_INFO)))
    type = 'text/html'
    if (file.exists(idx <- file.path(path, 'index.html'))) {
      readLines(idx, warn = FALSE)
    } else {
      d = file.info(list.files(path, all.files = TRUE, full.names = TRUE))
      title = escape_html(path)
      html_doc(c(sprintf('<h1>Index of %s</h1>', title), fileinfo_table(d)),
               title = title)
    }
  } else {
    # use the custom 404.html only if the path looks like a directory or .html
    try_404 = function(path) {
      file.exists('404.html') && grepl('(/|[.]html)$', path, ignore.case = TRUE)
    }
    # FIXME: using 302 here because 404.html may contain relative paths, e.g. if
    # /foo/bar/hi.html gives 404, I cannot just read 404.html and display it,
    # because it will be treated as /foo/bar/404.html; if 404.html contains
    # paths like ./css/style.css, I don't know how to let the browser know that
    # it means /css/style.css instead of /foo/bar/css/style.css
    if (!file.exists(path))
      return(if (try_404(path)) list(
        status = 302L, body = '', headers = list('Location' = '/404.html')
      ) else list(
        status = 404L, headers = list('Content-Type' = 'text/plain'),
        body = paste2('Not found:', path)
      ))

    type = guess_type(path)
    range = req$HTTP_RANGE

    if (is.null(range)) {
      xfun::read_bin(path)
    } else {
      range = strsplit(range, split = "(=|-)")[[1]]
      b2 = as.numeric(range[2])
      if (length(range) == 2 && range[1] == "bytes") {
        # open-ended range request
        # e.g. Chrome sends the range request 'bytes=0-'
        # http://stackoverflow.com/a/18745164/559676
        range[3] = file_size(path) - 1
      }
      b3 = as.numeric(range[3])
      if (length(range) < 3 || (range[1] != "bytes") || (b2 >= b3))
        return(list(
          status = 416L, headers = list('Content-Type' = 'text/plain'),
          body = 'Requested range not satisfiable\r\n'
        ))

      status = 206L  # partial content
      # type may also need to be changed
      # e.g. to "multipart/byteranges" if multipart range support is added at a later date
      # or possibly to "application/octet-stream" for binary files

      con = file(path, open = "rb", raw = TRUE)
      on.exit(close(con))
      seek(con, where = b2, origin = "start")
      readBin(con, 'raw', b3 - b2 + 1)
    }
  }
  if (is.character(body) && length(body) > 1) body = paste2(body)
  res = list(
    status = status, body = body,
    headers = c(list('Content-Type' = type), if (status == 206L) list(
      'Content-Range' = paste0("bytes ", range[2], "-", range[3], "/", file_size(path))
      ),
      'Accept-Ranges' = 'bytes') # indicates that the server supports range requests
  )
  if (is.function(response)) response(path, res) else res
}
