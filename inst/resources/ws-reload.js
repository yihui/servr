let flag;
ws.onmessage = e => {
  flag = true;
  if (e.data === 'false' || e.data === 'null') return;
  // fire a servr:reload event
  Event && document.dispatchEvent(new Event('servr:reload'));
  location.reload();
};
setInterval(function() {
  if (flag === false || ws.readyState !== ws.OPEN) return;
  flag = false;  // prevent ws message if R hasn't responded yet
  ws.send(JSON.stringify({ "pathname": location.pathname }));
}, !!SERVR_INTERVAL);
