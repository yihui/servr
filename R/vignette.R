#' Serve R Markdown/HTML package vignettes
#'
#' Serve package vignettes under the \file{vignettes/} directory. Because the
#' HTML output files should not be included in the source package, this function
#' renders R Markdown/HTML vignettes, displays them in the web browser, and
#' deletes the HTML output files. You will see the HTML output when you click
#' the links on the \file{.Rmd} or \file{.Rhtml} files (unlike the static HTTP
#' server, the compiled output instead of the source document is displayed).
#'
#' When developing R packages, you may want to preview your vignettes once in a
#' while. You can certainly click the button in RStudio to do it, but that
#' requires you to install the package and rebuild the vignettes. With this
#' function, your vignette will be rebuilt automatically when you update the
#' source document. Moreover, because the compilation takes place in the current
#' R session, you can take advantage of \code{devtools::load_all()} (which has a
#' keyboard shortcut in the RStudio IDE) to reload your package and see the
#' updated vignette in the web browser.
#' @inheritParams httd
#' @export
#' @note You are supposed to call this function from the root directory of your
#'   package. If that is not the case, you should provide the correct path to
#'   the \file{vignettes/} directory of your package to the \code{dir} argument.
vign = function(dir = '.', ...) {
  build_vign = function(path) {
    owd = getwd(); opts = options(knitr.chunk.purl = FALSE)
    on.exit({ setwd(owd); options(opts) })
    setwd(d <- dirname(path))
    o = tools::buildVignette(basename(path), latex = FALSE, tangle = FALSE)
    paste(d, o, sep = '/')
  }
  build_fun = build_watcher('[.](Rmd|Rhtml|js|css)$', build_vign)
  clean = function(path) {
    for (p in path) {
      if (!grepl('[.]html$', p)) next
      # remove .html/.md only if source document exists
      for (ext in c('.Rmd', '.Rhtml')) {
        if (file.exists(xfun::with_ext(p, ext))) {
          unlink(c(p, xfun::with_ext(p, '.md')))
        }
      }
    }
  }
  dynamic_site(
    dir, ..., initpath = 'vignettes/',
    build = build_fun,
    pre_process = function(req) {
      path = sub('^/', '', decode_path(req))
      if (!grepl('[.]R(md|html)$', path)) return(req)
      req$PATH_INFO = paste0('/', build_vign(path))
      req
    },
    post_process = function(req) {
      path = sub('^/', '', decode_path(req))
      clean(path)
      req
    }
  )
}
