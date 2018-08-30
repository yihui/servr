# servr

[![Build Status](https://travis-ci.org/yihui/servr.svg)](https://travis-ci.org/yihui/servr) ![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/servr)

A simple HTTP server to serve files under a given directory based on the
[**httpuv**](https://cran.r-project.org/package=httpuv) package.

You can install this package from
[CRAN](https://cran.r-project.org/package=servr) (stable version) or
[XRAN](https://xran.yihui.name) (development version):

```r
install.packages('servr')  # stable version; use a CRAN mirror, or
install.packages('servr', repos = 'https://xran.yihui.name')  # devel version
```

This package is licensed under GPL.

## Serve static files

To some degree, this package is like `python -m SimpleHTTPServer` or `python -m
http.server`. It may be used to serve:

- R Markdown applications
    - [**bookdown**](https://github.com/rstudio/bookdown) books
    - [**blogdown**](https://github.com/rstudio/bookdown) websites
    - [**xaringan**](https://github.com/rstudio/bookdown) presentations
- [**googleVis**](https://cran.r-project.org/package=googleVis) charts (to solve
  the Flash security problem)
- D3 charts which need to load JSON from local files (see also the
  [**animint**](https://github.com/tdhock/animint) package)
- [**rCharts**](http://rcharts.io) and
  [**samatha**](https://github.com/DASpringate/samatha), etc

You can either run `servr::httd()` in an interactive R session, or run from
command line:

```bash
# default: port 4321, do not launch browser
Rscript -e 'servr::httd()'

# open a web browser
Rscript -e 'servr::httd()' -b

# listen on port 4000
Rscript -e 'servr::httd()' -p4000

# pass arguments to the httd() function
Rscript -e 'servr::httd(,4000,TRUE)'
```

There is also a shell script under `system.file('bin', package = 'servr')`;
if it is added to `PATH`, you can simply run

```bash
servr  # serve the current directory
servr -b  # launch the browser
servr -b -p4000  # change port to 4000
```

## Serve and watch a directory

Similar to `httd()`, the function `httw()` can both serve and watch a directory.
If you are viewing an HTML file in the browser, it will be automatically
refreshed whenever there are any changes in the directory (e.g. you added,
deleted, or modified certain files in the directory).

## Serve dynamic documents

Besides `httd()`, there are functions `jekyll()`, `rmdv1()`, and `rmdv2()` in
this package to serve HTML files generated from R Markdown documents (via
[**knitr**](https://yihui.name/knitr/) or
[**rmarkdown**](https://rmarkdown.rstudio.com)). R Markdown documents can be
automatically re-compiled when their HTML output files are older than the
corresponding source files, and HTML pages in the web browser can be
automatically refreshed accordingly, so you can focus on writing R Markdown
documents, and results will be updated on the fly in the web browser. This is
even more useful when you write R Markdown documents in the RStudio IDE, because
the HTML files are displayed in the RStudio viewer pane, and you can put the
source document and its output side by side.

![Jekyll with servr and knitr](http://i.imgur.com/gKVGhiP.png)

## Serve package vignettes

The function `vign()` can be used to serve R Markdown/HTML package vignettes.
The HTML output files are generated and displayed in the web browser so you can
preview the vignettes, and they will be cleaned up after they are loaded in the
web browser to make sure your source package is clean.

## Daemonized server

All server functions can be used in the daemonized mode, i.e., they can be
non-blocking in the R session, which allows you to continue working in the R
console after the server is launched. This mode can be set via the argument
`daemon = TRUE` in most server functions. See `?server_config` for more
information.
