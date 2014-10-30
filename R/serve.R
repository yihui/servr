#' Serve R Markdown based websites
#'
#' R Markdown documents (with the filename extension \file{.Rmd}) are
#' re-compiled using \pkg{knitr} or \pkg{rmarkdown} when necessary (source files
#' are newer than output files), and the HTML pages will be automatically
#' refreshed in the web browser accordingly.
#'
#' The function \code{jekyll()} sets up a web server to serve a Jekyll-based
#' website. A connection is established between R and the HTML pages through
#' WebSockets so that R can notify the HTML pages to refresh themselves if any R
#' Markdown documents have been re-compiled.
#' @param dir the root directory of the website
#' @param input the input directories that contain R Markdown documents (the
#'   directories must be relative instead of absolute; same for \code{output}
#'   directories)
#' @param output the output directories corresponding to \code{input}; for an
#'   input document \file{foo.Rmd} under the directory \code{input[i]}, its
#'   output document \file{foo.md} (or \file{foo.html}) is generated under
#'   \code{output[i]} if the output document is older than the input document
#' @param script the name of an R script to re-build R Markdown documents; it
#'   will be executed via command line of the form \command{Rscript build.R arg1
#'   arg2} where \code{build.R} is the script specified by this argument,
#'   \code{arg1} is the input filename, and \code{arg2} is the output filename;
#'   inside the R script, you can use \code{\link{commandArgs}(TRUE)} to capture
#'   \code{c(arg1, arg2)}, e.g. \code{knitr::knit(commandArgs(TRUE)[1],
#'   commandArgs(TRUE)[2])}
#' @param baseurl the base URL for Jekyll (will be read from \file{_config.yml}
#'   if missing)
#' @inheritParams httd
#' @rdname dynamic_site
#' @note Apparently \code{jekyll()} and \code{rmdv1()} require the \pkg{knitr}
#'   package, and \code{rmdv2()} requires \pkg{rmarkdown}. You have to install
#'   them before calling the server functions here.
#'
#'   All R Markdown documents are compiled in separate R sessions. If you have
#'   any R Markdown documents that should not be compiled as standalone
#'   documents (e.g. child documents), you can use different filename
#'   extensions, such as \file{.Rmarkdown}.
#' @references R Markdown v1: \url{http://cran.rstudio.com/package=markdown}. R
#'   Markdown v2: \url{http://rmarkdown.rstudio.com}. For Jekyll, see
#'   \url{http://jekyllrb.com}. The GitHub repository
#'   \url{https://github.com/yihui/knitr-jekyll} is an example of serving Jekyll
#'   websites with \code{servr::jekyll()}.
#' @examples
#' if (interactive()) servr::rmdv1()  # serve the current dir with R Markdown v1
#' if (interactive()) servr::rmdv2()  # or R Markdown v2
#' @importFrom httpuv startServer service stopServer
#' @export
jekyll = function(
  dir = '.', input = c('.', '_source', '_posts'), output = c('.', '_posts', '_posts'),
  script = 'build.R', baseurl, port, launch.browser
) {
  if (missing(baseurl) && file.exists(config <- file.path(dir, '_config.yml'))) {
    x = iconv(readLines(config, encoding = 'UTF-8'), 'UTF-8')
    p = '^baseurl:\\s*([^#[:space:]]+).*$'
    x = grep(p, x, value = TRUE)
    if (length(x) == 1) baseurl = gsub('"', '', sub(p, '\\1', x))
  } else baseurl = ''
  dynamic_site(
    dir, port, launch.browser,
    build = function() {
      jekyll_build = function() {
        if (system2('jekyll', 'build') != 0) stop('Failed to run Jekyll')
      }
      if (!file_test('-d', '_site')) jekyll_build()
      update = knit_maybe(input, output, script, method = 'jekyll')
      if (update) jekyll_build()
      update
    },
    site.dir = '_site',
    baseurl
  )
}

#' @details The functions \code{rmdv1()} and \code{rmdv2()} are similar to
#'   \code{jekyll()}, and the only difference is the way to compile R Markdown
#'   documents: \code{rmdv1()} uses the \pkg{markdown} package (a.k.a R Markdown
#'   v1) via \code{\link[knitr]{knit2html}()}, and \code{rmdv2()} calls
#'   \code{\link[rmarkdown]{render}()} in the \pkg{rmarkdown} package (a.k.a R
#'   Markdown v2).
#' @rdname dynamic_site
#' @export
rmdv2 = function(dir = '.', script = 'build.R', port, launch.browser) {
  dynamic_rmd(dir, script, port, launch.browser, method = 'rmdv2')
}

#' @rdname dynamic_site
#' @export
rmdv1 = function(dir = '.', script = 'build.R', port, launch.browser) {
  dynamic_rmd(dir, script, port, launch.browser, method = 'rmdv1')
}

dynamic_rmd = function(dir, script, port, launch.browser, method) {
  dynamic_site(
    dir, port, launch.browser,
    build = function() {
      # exclude .hidden dirs
      dirs = grep('^[.].', list.dirs(), value = TRUE, invert = TRUE)
      knit_maybe(dirs, dirs, script, method = method)
    },
    site.dir = '.'
  )
}

# serve a dynamic site (dynamic in the sense that the site contains documents
# that need to be compiled to generate HTML files); we use WebSockets to notify
# the HTML pages whether they need to refresh themselves, which is determined by
# the value returned from the build() function
dynamic_site = function(
  dir = '.', port, launch.browser, build = function() FALSE,
  site.dir = dir, baseurl = ''
) {
  owd = setwd(dir); on.exit(setwd(owd))
  build()

  js  = readLines(system.file('resources', 'ws-reload.html', package = 'servr'))
  res = config(dir, port, launch.browser)
  res$browse()

  server = startServer('0.0.0.0', res$port, list(
    call = function(req) {
      owd = setwd(site.dir); on.exit(setwd(owd))
      if (baseurl != '') {
        path = req$PATH_INFO
        if (substr(path, 1, nchar(baseurl)) == baseurl)
          req$PATH_INFO = substr(path, nchar(baseurl) + 1, nchar(path))
      }
      res = serve_dir(req)
      if (res$headers[['Content-Type']] != 'text/html') return(res)
      # post-process HTML content: inject the websocket code
      body = res$body
      if (is.raw(body)) body = rawToChar(body)
      body = sub(
        '</head>', paste(c(js, '</head>'), collapse = '\r\n'), body,
        fixed = TRUE, useBytes = TRUE
      )
      res$body = body
      res
    },
    onWSOpen = function(ws) {
      # the client keeps on sending messages to ws, and ws needs to decide when
      # to update output from source files
      ws$onMessage(function(binary, message) {
        # notify the client that the output has been updated
        if (build()) ws$send(message)
      })
    }
  ))
  on.exit(stopServer(server), add = TRUE)

  while (TRUE) {
    service()
    Sys.sleep(0.001)
  }
}

#' Determin if R Markdown files need to be re-built
#' @param input the input dirs
#' @param output the output dirs
#' @param script the R script to build the R Markdown files
#' @param outext the file extension of output files (e.g. .md, .html)
#' @param method if no \code{script} was provided, fall back to internal methods
#'   to build R Markdown files, so I need to know if you are Jekyll, R Markdown
#'   v2, or something else
#' @noRd
knit_maybe = function(input, output, script, method = 'jekyll') {
  outext = switch(method, jekyll = '.md', '.html')
  # check if R Markdown files need to be recompiled
  res = mapply(
    obsolete_out, input, output,
    MoreArgs = list(outext = outext), SIMPLIFY = FALSE
  )
  update = FALSE
  # recompile source documents
  lapply(res, function(r) {
    if (length(r) == 0) return()
    update <<- TRUE
    # compile each posts in a separate R session
    for (i in seq_len(nrow(r))) {
      # run script if exists, passing input and output filenames to Rscript
      if (file.exists(script)) {
        rscript(shQuote(c(script, r[i, 1], r[i, 2])), r[i, 1])
        next
      }
      # otherwise run default code
      build = getFromNamespace(paste('build', method, sep = '_'), 'servr')
      build(r[i, 1], r[i, 2])
    }
  })
  update
}

# which output files (.md) are obsolete compared to their sources (.Rmd)?
obsolete_out = function(input, output, pattern = '[.]Rmd$', outext = '.md') {
  if (!file_test('-d', input)) return()
  src = list.files(input, pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(src) == 0) return()
  out = sub(pattern, outext, src, ignore.case = TRUE)
  if (input != output)
    out = paste(output, substring(out, nchar(input) + 1, nchar(out)), sep = '')
  # return sources that are newer than output files
  idx = !file.exists(out) | file_test('-nt', src, out)
  if (any(idx)) cbind(src[idx], out[idx])
}

build_jekyll = function(input, output) {
  code = c(
    sprintf(
      c("knitr::opts_chunk$set(fig.path = 'figure/%s/', ",
        "cache.path = 'cache/%s/')"),
      gsub('^_|[.][a-zA-Z]+$', '', input)
    ),
    "knitr::opts_knit$set(base.url = '/')",
    sprintf(
      "knitr::knit('%s', '%s', quiet = TRUE, encoding = 'UTF-8')",
      input, output
    )
  )
  rscript(c(rbind('-e', shQuote(code))), input)
}

build_rmd = function(input, output, template) {
  owd = setwd(dirname(input)); on.exit(setwd(owd))
  input = basename(input)
  code = sprintf(template, input)
  rscript(c(rbind('-e', shQuote(code))), file.path(owd, input))
}

build_rmdv2 = function(...) {
  build_rmd(..., template = "rmarkdown::render('%s', encoding = 'UTF-8', quiet = TRUE)")
}

build_rmdv1 = function(...) {
  build_rmd(..., template = "knitr::knit2html('%s', encoding = 'UTF-8', quiet = TRUE)")
}
