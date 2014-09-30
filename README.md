# servr

[![Build Status](https://travis-ci.org/yihui/servr.svg)](https://travis-ci.org/yihui/servr)

A simple HTTP server to serve files under a given directory based on the
[**httpuv**](http://cran.r-project.org/package=httpuv) package.

You can install this package from
[CRAN](http://cran.r-project.org/package=servr) (stable version) or
[RForge](http://www.rforge.net/servr/) (development version):

```r
install.packages('servr')  # stable version; use a CRAN mirror, or
install.packages('servr', repos = 'http://rforge.net')  # devel version
```

This package is licensed under GPL.

## Serve static files

To some degree, this package is like `python -m SimpleHTTPServer` or `python -m
http.server`. It may be used to serve:

- [**googleVis**]() charts (to solve the Flash security problem)
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

## Serve dynamic documents

Besides `httd()`, there are functions `jekyll()`, `rmdv1()`, and `rmdv2()` in
this package to serve HTML files generated from R Markdown documents (via
[**knitr**](http://yihui.name/knitr) or
[**rmarkdown**](http://rmarkdown.rstudio.com)). R Markdown documents can be
automatically re-compiled when their HTML output files are older than the
corresponding source files, and HTML pages in the web browser can be
automatically refreshed accordingly, so you can focus on writing R Markdown
documents, and results will be updated on the fly in the web browser. This is
even more useful when you write R Markdown documents in the RStudio IDE, because
the HTML files are displayed in the RStudio viewer pane, and you can put the
source document and its output side by side.

![Jekyll with servr and knitr](http://i.imgur.com/gKVGhiP.png)
