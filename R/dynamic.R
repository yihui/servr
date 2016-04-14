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
#' @param script a Makefile (see \code{\link{make}}), or (if Makefile not found)
#'   the name of an R script to re-build R Markdown documents, which will be
#'   executed via command line of the form \command{Rscript build.R arg1 arg2}
#'   where \code{build.R} is the script specified by this argument, \code{arg1}
#'   is the input filename, and \code{arg2} is the output filename; inside the R
#'   script, you can use \code{\link{commandArgs}(TRUE)} to capture
#'   \code{c(arg1, arg2)}, e.g. \code{knitr::knit(commandArgs(TRUE)[1],
#'   commandArgs(TRUE)[2])}; if this R script is not found, either, internal
#'   compiling methods will be used, which are basically
#'   \code{\link[knitr]{knit}()}, \code{\link[knitr]{knit2html}()}, or
#'   \code{\link[rmarkdown]{render}()}
#' @param serve whether to serve the website; if \code{FALSE}, the R Markdown
#'   documents and the website will be compiled but not served
#' @param command a command to build the Jekyll website; by default, it is
#'   \command{jekyll build}, and you can use alternative commands, such as
#'   \command{bundle exec jekyll build}
#' @inheritParams httd
#' @rdname dynamic_site
#' @note Apparently \code{jekyll()} and \code{rmdv1()} require the \pkg{knitr}
#'   package, and \code{rmdv2()} requires \pkg{rmarkdown}. You have to install
#'   them before calling the server functions here.
#'
#'   All R Markdown documents are compiled in separate R sessions by default. If
#'   you have any R Markdown documents that should not be compiled as standalone
#'   documents (e.g. child documents), you can use different filename
#'   extensions, such as \file{.Rmarkdown}.
#'
#'   The \code{baseurl} argument does not work in \code{jekyll()}, and the base
#'   URL setting will be read from \file{_config.yml} (the \samp{baseurl} field)
#'   of the website if present. You should not pass \code{baseurl} to the
#'   function \code{jekyll()} directly.
#' @references R Markdown v1: \url{https://cran.r-project.org/package=markdown}.
#'   R Markdown v2: \url{http://rmarkdown.rstudio.com}. For Jekyll, see
#'   \url{http://jekyllrb.com}. The GitHub repository
#'   \url{https://github.com/yihui/knitr-jekyll} is an example of serving Jekyll
#'   websites with \code{servr::jekyll()}.
#' @examples
#' if (interactive()) servr::rmdv1()  # serve the current dir with R Markdown v1
#' if (interactive()) servr::rmdv2()  # or R Markdown v2
#'
#' # built-in examples
#' servr::serve_example('rmd', servr::rmdv1)
#' servr::serve_example('rmd', servr::rmdv2)
#' @importFrom httpuv startServer service stopServer
#' @export
jekyll = function(
  dir = '.', input = c('.', '_source', '_posts'), output = c('.', '_posts', '_posts'),
  script = c('Makefile', 'build.R'), serve = TRUE, command = 'jekyll build', ...
) {
  baseurl = jekyll_config(dir, 'baseurl', '')
  destination = jekyll_config(dir, 'destination', '_site')
  jekyll_build = function() {
    if (system(command) != 0) stop('Failed to run: ', command)
  }
  build_all = function() knit_maybe(input, output, script, method = 'jekyll')

  if (!serve) {
    in_dir(dir, {
      build_all()
      jekyll_build()
    })
    return()
  }
  dynamic_site(
    dir, ...,
    build = function(...) {
      update = build_all()
      if (update || !file_test('-d', destination)) jekyll_build()
      update
    },
    site.dir = destination,
    baseurl = baseurl
  )
}

# in theory, I should use the yaml package, but I do not want to introduce a
# dependency at the moment, so here goes the naive way
jekyll_config = function(dir, field, default) {
  if (!file.exists(config <- file.path(dir, '_config.yml'))) return(default)
  x = iconv(readLines(config, encoding = 'UTF-8'), 'UTF-8')
  p = sprintf('^%s:\\s*([^#[:space:]]+).*$', field)
  x = grep(p, x, value = TRUE)
  if (length(x) == 1) gsub('"', '', sub(p, '\\1', x)) else default
}

#' @details The functions \code{rmdv1()} and \code{rmdv2()} are similar to
#'   \code{jekyll()}, and the only difference is the way to compile R Markdown
#'   documents: \code{rmdv1()} uses the \pkg{markdown} package (a.k.a R Markdown
#'   v1) via \code{\link[knitr]{knit2html}()}, and \code{rmdv2()} calls
#'   \code{\link[rmarkdown]{render}()} in the \pkg{rmarkdown} package (a.k.a R
#'   Markdown v2).
#' @rdname dynamic_site
#' @param in_session whether to render the R Markdown documents in the current R
#'   session (\code{TRUE}) or in a separate new R session (\code{FALSE}); if the
#'   former, the argument \code{script} can be a function with two arguments,
#'   the filenames of the source document and the output document, respectively;
#'   an internal function (basically \code{rmarkdown::render()} or
#'   \code{knitr::knit2html()}) will be used if the \code{script} argument is
#'   not a function and \code{in_session = TRUE}
#' @note For the sake of reproducibility, you are recommended to compile each
#'   source document in a separate R session (i.e., use the default
#'   \code{in_session = FALSE}) to make sure they can compile on their own,
#'   otherwise the current workspace may affect the evaluation of the code
#'   chunks in these source documents. Sometimes it might be useful to compile a
#'   document in the current R session. For example, if reading data is
#'   time-consuming and it is not convenient to cache it (using the \pkg{knitr}
#'   chunk option \code{cache = TRUE}), you may read the data once, temporarily
#'   turn off the evaluation of that code chunk, and keep on working on the rest
#'   of code chunks so that data will not be read over and over again.
#' @export
rmdv2 = function(dir = '.', script = c('Makefile', 'build.R'), in_session = FALSE, ...) {
  dynamic_rmd(dir, script, ..., method = 'rmdv2', in_session = in_session)
}

#' @rdname dynamic_site
#' @export
rmdv1 = function(dir = '.', script = c('Makefile', 'build.R'), in_session = FALSE, ...) {
  dynamic_rmd(dir, script, ..., method = 'rmdv1', in_session = in_session)
}

dynamic_rmd = function(dir, script, ..., method, in_session = FALSE) {
  dynamic_site(
    dir, ...,
    build = function(message) {
      # exclude .hidden dirs
      dirs = grep('^[.].', list.dirs(), value = TRUE, invert = TRUE)
      knit_maybe(dirs, dirs, script, method, in_session)
    },
    site.dir = '.'
  )
}

# serve a dynamic site (dynamic in the sense that the site contains documents
# that need to be compiled to generate HTML files); we use WebSockets to notify
# the HTML pages whether they need to refresh themselves, which is determined by
# the value returned from the build() function
dynamic_site = function(
  dir = '.', ..., build = function(...) FALSE, site.dir = dir, baseurl = '',
  pre_process = identity, post_process = identity, response = serve_dir()
) {
  dir = normalizePath(dir, mustWork = TRUE)
  in_dir(dir, build())

  js  = readLines(system.file('resources', 'ws-reload.html', package = 'servr'))
  res = server_config(dir, ..., baseurl = baseurl)
  timeout = new_timeout(res$interval)
  res$browse()

  app = list(
    call = function(req) {
      owd = setwd(dir); on.exit(setwd(owd))
      setwd(site.dir)
      if (baseurl != '') {
        path = req$PATH_INFO
        if (substr(path, 1, nchar(baseurl)) == baseurl)
          req$PATH_INFO = substr(path, nchar(baseurl) + 1, nchar(path))
      }
      req = pre_process(req)
      res = response(req)
      req = post_process(req)
      if (res$status != 200L || res$headers[['Content-Type']] != 'text/html')
        return(res)
      # post-process HTML content: inject the websocket code
      body = res$body
      if (is.raw(body)) body = rawToChar(body)
      if (length(grep(
        '<!-- DISABLE-SERVR-WEBSOCKET -->', body, fixed = TRUE, useBytes = TRUE
      ))) return(res)
      body = if (length(grep('</head>', body))) sub(
        '</head>', paste(c(js, '</head>'), collapse = '\r\n'), body,
        fixed = TRUE, useBytes = TRUE
      ) else if (length(grep('</html>', body)) == 0) {
        # there is no </head> or </html>, just append js after the document
        paste(c(body, js), collapse = '\r\n')
      } else body
      res$body = body
      res
    },
    onWSOpen = function(ws) {
      # the client keeps on sending messages to ws, and ws needs to decide when
      # to update output from source files
      error = FALSE  # when an error occurred, stop sending messages
      ws$onMessage(function(binary, message) {
        # if the last build errored, wait for 1, 2, 4, 8, 16, ... seconds,
        # otherwise restore the default time interval
        if (!timeout(error)) {
          error <<- NA  # in an indetermined state (wait and see)
          return()
        }
        owd = setwd(dir); on.exit(setwd(owd))
        error <<- FALSE
        # notify the client that the output has been updated
        tryCatch(
          if (build(jsonlite::fromJSON(message))) ws$send('reload'),
          error = function(e) {
            error <<- TRUE; print(e)
          }
        )
      })
    }
  )
  res$start_server(app)
}

#' Determine if R Markdown files need to be re-built
#' @param input the input dirs
#' @param output the output dirs
#' @param script the R script, or an R function to build the R Markdown files
#' @param method if no \code{script} was provided, fall back to internal methods
#'   to build R Markdown files, so I need to know if you are Jekyll, R Markdown
#'   v2, or something else
#' @noRd
knit_maybe = function(input, output, script, method = 'jekyll', in_session = FALSE) {
  # if the script is a Makefile, check `make -q` to see if we need to
  # re-generate any files; if we do not, there is nothing to do, otherwise run
  # make, and tell the client that the files have been updated
  if (is.character(script)) {
    if (('Makefile' %in% script) && file.exists('Makefile')) {
      if (in_session) warning('You cannot use in_session = TRUE with Makefile')
      return(make_maybe())
    }
    script = setdiff(script, 'Makefile')
    if (length(script) != 1) stop("The length of the 'script' argument must be 1")
  }

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
      if (!in_session && file.exists(script)) {
        rscript(shQuote(c(script, r[i, 1], r[i, 2])), r[i, 1])
        next
      }
      if (in_session && is.function(script)) {
        script(r[i, 1], r[i, 2])
        next
      }
      # otherwise run default code
      build = getFromNamespace(paste('build', method, sep = '_'), 'servr')
      build(r[i, 1], r[i, 2], in_session)
    }
    if (any(i <- !file.exists(r[, 2])))
      stop('Some output files were not successfully generated: ', paste(r[i, 2], collapse = ', '))
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

build_jekyll = function(input, output, ...) {
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

build_rmd = function(input, output, template, in_session) {
  owd = setwd(dirname(input)); on.exit(setwd(owd))
  input = basename(input)
  code = sprintf(template, input)
  if (in_session) {
    eval(parse(text = code), envir = globalenv())
  } else {
    rscript(c(rbind('-e', shQuote(code))), file.path(owd, input))
  }
}

build_rmdv2 = function(...) {
  build_rmd(..., template = "rmarkdown::render('%s', encoding = 'UTF-8', quiet = TRUE)")
}

build_rmdv1 = function(...) {
  build_rmd(..., template = "knitr::knit2html('%s', encoding = 'UTF-8', quiet = TRUE)")
}
