#' Serve the static files under a directory
#'
#' If there is an \file{index.html} under this directory, it will be displayed;
#' otherwise the list of files is displayed, with links on their names. After we
#' run this function, we can go to \url{http://localhost:port} to browse the web
#' pages either created from R or read from HTML files.
#' @param dir the directory to serve
#' @param port the TCP port number; by default it is \code{4321}, unless a
#'   command line argument of the form \code{-pNNNN} (N is a digit from 0 to 9)
#'   was passed in when R was launched, in which case \code{NNNN} will be used
#'   as the port number
#' @param launch.browser whether to launch the default web browser; by default,
#'   it is \code{TRUE} if the R session is \code{\link{interactive}()}, or when
#'   a command line argument \code{-b} was passed to R (see
#'   \code{\link{commandArgs}()}); N.B. the RStudio viewer is used as the web
#'   browser if available
#' @export
#' @importFrom httpuv runServer
#' @references \url{https://github.com/yihui/servr}
#' @examples #' see https://github.com/yihui/servr for command line usage
#' # or run inside an R session
#' if (interactive()) servr::httd()
httd = function(dir = '.', port, launch.browser) {
  if (dir != '.') {
    owd = setwd(dir); on.exit(setwd(owd))
  }
  res = config(dir, port, launch.browser)
  res$browse()
  runServer('0.0.0.0', res$port, list(call = serve_dir))
}

# some configurations for the server
config = function(dir, port, launch.browser) {
  cargs = commandArgs(TRUE)
  if (missing(launch.browser))
    launch.browser = interactive() || '-b' %in% cargs || is_rstudio()
  if (missing(port))
    port = if (length(port <- grep('^-p[0-9]{4,}$', cargs, value = TRUE)) == 1)
      as.integer(sub('^-p', '', port)) else 4321L
  damn_library('methods')
  url = sprintf('http://localhost:%d', port)
  list(
    port = port,
    browse = function() {
      if (launch.browser) {
        browseURL(url, browser = get_browser())
      } else message('serving the directory ', dir, ' at ', url)
    }
  )
}

serve_dir = function(req) {
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
