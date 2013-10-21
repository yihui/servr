# copied from the markdown package: http://cran.r-project.org/package=markdown
.MIMEMAP <- list(

  ## most common web types
  htm = 'text/html',
  html = 'text/html',
  css = 'text/css',
  gif = 'image/gif',
  jpg = 'image/jpeg',
  jpeg = 'image/jpeg',
  jpe = 'image/jpeg',
  png = 'image/png',
  js = 'application/x-javascript',
  pdf = 'application/pdf',
  svg = 'image/svg+xml',
  swf = 'application/x-shockwave-flash',

  ## markdown types
  md = 'text/x-markdown',
  mdtxt = 'text/x-markdown',
  markdown = 'text/x-markdown',

  ## other types we are likely to serve
  xml = 'text/xml',
  csv = 'text/csv',
  ico = 'image/x-icon',
  zip = 'application/zip',
  bz = 'application/x-bzip',
  bz2 = 'application/x-bzip2',
  gz = 'application/x-gzip',
  tar = 'application/x-tar',

  ## yet more types...

  shtml = 'text/html',
  tsv = 'text/tab-separated-values',
  tab = 'text/tab-separated-values',
  dcf = 'text/debian-control-file',
  txt = 'text/plain',
  mml = 'text/mathml',

  tif = 'image/tiff',
  tiff = 'image/tiff',
  bmp = 'image/bmp',
  ps = 'application/postscript',
  eps = 'application/postscript',
  dvi =   'application/x-dvi',

  atom = 'application/atom+xml',
  rss = 'application/rss+xml',

  doc = 'application/msword',
  docx = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
  odt = 'application/vnd.oasis.opendocument.text',
  rtf = 'application/rtf',
  xls = 'application/vnd.ms-excel',
  xlsx = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
  ods = 'application/x-vnd.oasis.opendocument.spreadsheet',
  ppt = 'application/vnd.ms-powerpoint',
  pps = 'application/vnd.ms-powerpoint',
  pptx = 'application/vnd.openxmlformats-officedocument.presentationml.presentation',

  sit = 'application/x-stuffit',
  sxw = 'application/vnd.sun.xml.writer',

  iso = 'application/octet-stream',
  dmg = 'application/octet-stream',
  exe = 'application/octet-stream',
  dll = 'application/octet-stream',
  deb = 'application/octet-stream',
  otf = 'application/octet-stream',
  xpi = 'application/x-xpinstall',

  mp2 = 'audio/mpeg',
  mp3 = 'audio/mpeg',

  mpg = 'video/mpeg',
  mpeg = 'video/mpeg',
  flv = 'video/x-flv'
)

guess_mime = function(file) {
  ext = tolower(tools::file_ext(file))
  # assume files without extension are plain text files, so are R, Rd
  if (ext %in% c('', 'r', 'rd')) return('text/plain')
  if (is.null(.MIMEMAP[[ext]])) 'application/octet-stream' else .MIMEMAP[[ext]]
}

# turn() file.info() to an HTML table
fileinfo_table = function(info) {
  info = info[order(info$isdir, decreasing = TRUE), ]
  d = info$isdir; i = !is.na(d)
  # files/dirs
  x1 = paste(basename(rownames(info)), ifelse(d & i, '/', ''), sep = '')
  x1 = escape_html(x1)
  x1[i] = sprintf('<a href="%s">%s</a>', x1[i], x1[i])
  # size
  x2 = paste(format(info$size, scientific = FALSE, big.mark = ','), 'B')
  x2[is.na(info$size) | d] = ''
  # date modified
  x3 = as.character(info$mtime)
  x3[is.na(x3)] = ''
  c('<table>',
    '<thead><tr>',
    sprintf('<th>%s</th>', c('Name', 'Size', 'Date Modified')),
    '</tr></thead>',
    apply(cbind(
      '<tr>',
      sprintf('<td>%s</td>', x1),
      sprintf('<td align="right">%s</td>', x2),
      sprintf('<td>%s</td>', x3),
      '</tr>'), 1, paste, collapse = ''),
    '</table>')
}

# make an HTML document from body and title
html_doc = function(body, title = NULL) {
  c('<!DOCTYPE html>', '<html>',
    '<head>', sprintf('<title>%s</title>', title), '</head>',
    '<body>', body, '</body>', '</html>')
}

# escape special HTML chars (copied from highr)
escape_html = function(x) {
  x = gsub('&', '&amp;', x)
  x = gsub('<', '&lt;', x)
  x = gsub('>', '&gt;', x)
  x = gsub('"', '&quot;', x)
  x
}

# sorry for being impolite, but the business of Depends/Imports/library()/R CMD
# check/... is becoming more and more confusing, although I understand the good
# intention; sometimes methods does not work even you import it in namespace
# (there are weird animals in Ref Classes), and R hates library() after you
# Depends; all in all, let's stay with what is working, and forget about the R
# CMD check fuss (whoever sees this function please stay calm and try not to
# discuss it)
damn_library = function(pkg) library(pkg, character.only = TRUE)

# use the RStudio viewer if possible
get_browser = function() {
  browser = if ('tools:rstudio' %in% search()) getOption('viewer')
  if (is.null(browser) || !is.function(browser)) browser = getOption('browser')
  browser
}
