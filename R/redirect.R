#' Create a redirect server
#'
#' Create a server that simply redirect a requested path to another path.
#' @param dest A function that takes the requested path as input and returns a
#'   destination path.
#' @param ... Arguments to be passed to \code{\link{server_config}()}.
#' @export
#' @examplesIf interactive()
#' # redirect any request to httpbin.org
#' s = servr::redirect(dest = function(path) {
#'   paste0('https://httpbin.org/anything?path=', path)
#' })
#' s$url
#'
#' browseURL(paste0(s$url, '/hello'))
#' browseURL(paste0(s$url, '/world'))
#'
#' s$stop_server()
redirect = function(dest, ...) {
  create_server(..., serve_fun = function(req) list(
    status = 301L, body = '', headers = list('Location' = dest(req$PATH_INFO))
  ))
}
