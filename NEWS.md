# CHANGES IN servr VERSION 0.29

- Added a function `httr()` to run R scripts and show output as HTML pages when serving a directory.

- Added a new argument `response` to `httd()` to post-process the response.

# CHANGES IN servr VERSION 0.28

- Added support for HTTP authentication (thanks, @statquant, #63). For all server functions in this package, you can use the `auth` argument to provide the authentication scheme and credentials. See the help page `?servr::server_config` for more info.

# CHANGES IN servr VERSION 0.27

- Fixed an issue with `vign()` that when an error occurs in the vignette, `vign()` wouldn't stop rebuilding the vignette.

- Fixed a character encoding issue #62 (thanks, @eternal-flame-AD).

# CHANGES IN servr VERSION 0.26

- `vign()` also watches for changes in js/css files now.

- Stop using the `encoding` argument of `knitr::knit()` and `rmarkdown::render()`.

- Removed the experimental function `notebook()` from several years ago.

# CHANGES IN servr VERSION 0.25

- Added a function `create_server()` to create a server with a custom HTTP request handler and optionally a WebSocket handler.

- Added a function `redirect()` to return a redirect response.

- Added a new argument `exclude` to `random_port()` to exclude certain port numbers when generating a random available port.

- The `baseurl` argument works for all server functions now, such as `httd()`.

# CHANGES IN servr VERSION 0.24

- When searching for an available random port via `servr::random_port()` on the host `127.0.0.1`, the availability of the port is also tested on `0.0.0.0` by default. This is to avoid the situation where a port has been used on `0.0.0.0` but **httpuv** still thinks it is available on `127.0.0.1`. If you want to skip this additional testing, you may set `options(servr.test.0.0.0.0 = FALSE)`.

# CHANGES IN servr VERSION 0.23

- Updated the list of ports considered unsafe by Chrome (thanks, @RLesur, #56).

# CHANGES IN servr VERSION 0.22

- When the global option `options(servr.test.0.0.0.0 = TRUE)`, `random_port()` tests the availability of a port at the host address `0.0.0.0` when the requested host address is `127.0.0.1`.

# CHANGES IN servr VERSION 0.21

- Added a new argument `hosturl` to `server_config()`.

- Added a new argument `filter` to `httw()` to allow users to filter file paths on the watch list (thanks, @ARawles, #51).

# CHANGES IN servr VERSION 0.20

- `server_config()` will add a leading `/` to `baseurl` if it has not already been included.

# CHANGES IN servr VERSION 0.19

- `server_config()` also returns the `daemon` value in the list.

# CHANGES IN servr VERSION 0.18

- Fixed the malformed Accept-Ranges header (thanks, @JasonPunyon, #47).

- `servr::random_port()` looks for an available port quietly via `httpuv::startServer(quiet = TRUE)`. This requires **httpuv** >= v1.5.2.

# CHANGES IN servr VERSION 0.17

- Added an argument `open` to `servr::browse_last()` so users can decide whether to reopen the lastly browsed page.

# CHANGES IN servr VERSION 0.16

- Refinements to HTTP range request responses. Open-ended range requests (including "Range: bytes=0-") should now be correctly handled (thanks, @raymondben, #41).

# CHANGES IN servr VERSION 0.15

- When the environment variable `R_SERVR_PORT` is set, `server_config()` may issue a superfluous warning "createTcpServer: address already in use" due to an unnecessary call to `random_port()` (thanks, @itcarroll, #39).

# CHANGES IN servr VERSION 0.14

- The argument `host` in `server_config()` can be configured through the global option `servr.host` now, e.g., `options(servr.host = '0.0.0.0')`.

# CHANGES IN servr VERSION 0.13

- Added a `verbose` argument to `server_config()`.

- The `interval` argument of `server_config()` can be set via the global option `servr.interval` now. For example, `options(servr.interval = 0.5)`.

- Server functions such as `httd()` now return the config object created by `server_config()` instead of the server handle returned by `httpuv::startServer()`. The config object contains various information about the server and methods to start/stop the server.

# CHANGES IN servr VERSION 0.12

- Exported the function `random_port()` to return an available random TCP port.

- `daemon_list()` returns server IDs instead of handles, so that it will work with httpuv >= v1.4.5.1 (rstudio/blogdown#365).

# CHANGES IN servr VERSION 0.11

- The default value of the `port` argument of `server_config()` can be set via the environment variable `R_SERVR_PORT`. If the environment variable does not exist, the global option `servr.port` will be used if set, e.g., `options(servr.port = 4322)`. See `?servr::server_config` for details.

# CHANGES IN servr VERSION 0.10

- Added a function `browse_last()` to reopen the last browsed page.

- The `daemon` argument in `server_config()` now defaults to `interactive()`, i.e., **servr** starts a daemonized server that does not block your interactive R session by default.

# CHANGES IN servr VERSION 0.9

- The web browser may be opened too early (before the server is ready) (originally reported at rstudio/rstudio#2475).

# CHANGES IN servr VERSION 0.8

- added another implementation of the daemonized server based on the later package, since the previous implmentation based on `httpuv::startDaemonizedServer()` could crash the R session on Windows

# CHANGES IN servr VERSION 0.7

- added a new argument `watch` to `servr::httw()`

- exported the function `server_config()`

- files/directories that contain multibyte characters in path names cannot be served correctly (thanks, Hao Peng)

# CHANGES IN servr VERSION 0.6

- on 404 (page not found), 404.html will be displayed if it exists under the root directory

- improved the support for HTTP Range requests, e.g. servr can correctly serve MP4 videos now in major browsers including Safari

- servr should decode requested paths before reading them (https://github.com/rstudio/blogdown/issues/85)

# CHANGES IN servr VERSION 0.5

- added an argument `initpath` to `server_config()` so you can open a specific path initially in the web browser

- the `daemon` argument of `server_config()` takes the default value from the global option `getOption('servr.daemon')` now, e.g., you can set `options(servr.daemon = TRUE)` so that the daemonized server is always used

# CHANGES IN servr VERSION 0.4.1

- excluded ports considered unsafe by Chrome http://superuser.com/a/188070 when selecting a random port automatically

- fixed a bug in 301 redirection when serving a directory without the trailing slash

# CHANGES IN servr VERSION 0.4

- you can disable websocket listening on an HTML page using a special HTML comment `<!-- DISABLE-SERVR-WEBSOCKET -->` when servr is serving and watching a directory, so that this page will not communicate with R (e.g. when it is updated, R will not send signals to refresh it) (thanks @hafen, #25)

- a random TCP port will be used if the port 4321 is not available

# CHANGES IN servr VERSION 0.3

- added a function `httw()` to watch for changes under a directory and refresh an HTML page automatically (if it is being viewed in the browser) when any files are modified

- servr accepts HTTP Range requests now (thanks, @rekado, #21)

- servr did not work with RStudio Server (#20)

# CHANGES IN servr VERSION 0.2

- when running inside RStudio, the RStudio web browser will be used if available (requires RStudio >= 0.98.439)

- added three server functions jekyll(), rmdv1(), and rmdv2() to serve Jekyll websites, R Markdown v1, and R Markdown v2 documents, respectively

- added a server function vign() to serve R Markdown/HTML package vignettes

- added a server function make() to serve a directory and update files automatically via Makefile (#2)

- the URL pathname foo will be redirected to foo/ automatically if foo is a directory (#5)

- in case of errors when serving dynamic documents, the server functions will double the delay to check for updates and wait for the next build until the error is cleared (like Gmail)

# CHANGES IN servr VERSION 0.1

- the main function httd() to start an HTTP server for a local directory
