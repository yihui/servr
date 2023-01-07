require("servr")

writeLines("\n\n\n *** Starting Servr *** \n\n\n")

s = servr::create_server(port = 2222, host = '0.0.0.0', handler = function(req) {
  list(status = 200L, body = paste("Success:", req$PATH_INFO))
})
print(s$url)
