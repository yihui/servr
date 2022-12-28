require("servr")

writeLines("\n\n\n *** Starting Servr *** \n\n\n")

servr::create_server(port = 2222, handler = function(req) {
    list(status = 200L, body = paste("Success:", req$PATH_INFO))
  })