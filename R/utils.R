# turn file.info() to an HTML table
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
