#' Create a redirect response
#'
#' Create a response to redirect to a destination.
#' @param dest A destination path.
#' @param status The status code (usually \code{301} or \code{302}).
#' @export
#' @examples
#' servr::redirect('https://www.r-project.org')
redirect = function(dest, status = 301L) {
  list(status = status, body = '', headers = list('Location' = dest))
}
