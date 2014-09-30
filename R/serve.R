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
  script = 'build.R', port, launch.browser
) {
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
    site.dir = '_site'
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
    site.dir = dir
  )
}

# serve a dynamic site (dynamic in the sense that the site contains documents
# that need to be compiled to generate HTML files); we use WebSockets to notify
# the HTML pages whether they need to refresh themselves, which is determined by
# the value returned from the build() function
dynamic_site = function(
  dir = '.', port, launch.browser, build = function() FALSE, site.dir = dir
) {
  owd = setwd(dir); on.exit(setwd(owd))
  build()

  js  = readLines(system.file('resources', 'ws-reload.html', package = 'servr'))
  res = config(dir, port, launch.browser)
  res$browse()

  server = startServer('0.0.0.0', res$port, list(
    call = function(req) {
      owd = setwd(site.dir); on.exit(setwd(owd))
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

rscript = function(code, input) {
  if (system2('Rscript', code, stdout = NULL) != 0)
    stop('Failed to compile ', input, call. = FALSE)
}
