# servr

[![Build Status](https://travis-ci.org/yihui/servr.png)](https://travis-ci.org/yihui/servr)

A simple HTTP server to serve files under a given directory based on the
[**httpuv**](http://cran.r-project.org/package=httpuv) package.

For now, you can install from [RForge](http://www.rforge.net/servr/):

```s
install.packages('servr', repos = 'http://www.rforge.net/')
```

This is only a little exercise of mine while I was learning **httpuv**, and
may not be really useful. To some degree, it is like `python -m
SimpleHTTPServer` or `python -m http.server`. This package may be used to
serve:

- [**googleVis**]() charts (to solve the Flash security problem)
- D3 charts which need to load JSON from local files (see also the
  [**animint**](https://github.com/tdhock/animint) package)

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

This package is licensed under GPL.
