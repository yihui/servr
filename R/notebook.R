#' Serve R notebooks
#'
#' Serve R notebook files created from the \pkg{rnotebook} package.
#'
#' An R notebook is basically a JSON file, and the JSON data is inserted to a
#' predefined template in this package (\code{system.file('resources',
#' 'rnotebook.html', package = 'servr')}), and rendered through the JavaScript
#' implementation of \href{http://commonmark.org}{CommonMark} at the moment (the
#' renderer may change in the future).
#' @inheritParams httd
#' @inheritParams server_config
#' @export
#' @keywords internal
#' @examples # see ?rnotebook::newnb for examples
notebook = function(dir = '.', ...) {
  dir = normalizePath(dir, mustWork = TRUE)
  refresh = getFromNamespace('refresh', 'rnotebook')
  in_dir(dir, {
    build = build_watcher('[.]Rnb$', refresh)
  })
  dynamic_site(
    dir, ..., site.dir = '.',
    build = build,
    response = function(req) {
      path = sub('^/', '', req$PATH_INFO)
      if (!grepl('[.]Rnb$', path) || !file.exists(path)) return(serve_dir()(req))
      json  = readLines(path, encoding = 'UTF-8')
      tmpl  = readLines(system.file('resources', 'rnotebook.html', package = 'servr'))
      token = '%RNOTEBOOK_DATA%'
      if (length(i <- which(token == tmpl)) != 1) stop('Wrong notebook template')
      tmpl[i] = paste(json, collapse = '\n')
      tmpl = paste(tmpl, collapse = '\r\n')
      list(status = 200L, headers = list('Content-Type' = 'text/html'), body = tmpl)
    }
  )
}
