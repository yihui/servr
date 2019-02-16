ws.onmessage = function(evt) {
  // fire a servr:reload event
  Event && document.dispatchEvent(new Event('servr:reload'));
  location.reload();
};
// keep on sending messages to the server, and the server will send the
// message back when necessary
setInterval(function() {
  if (ws.readyState === ws.OPEN)
    ws.send(JSON.stringify({
      "pathname": location.pathname,
      "search": location.search
    }));
}, !!SERVR_INTERVAL);
