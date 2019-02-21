var flag;
ws.onmessage = function(evt) {
  flag = true;
  if (evt.data === 'false' || evt.data === 'null') return;
  // fire a servr:reload event
  Event && document.dispatchEvent(new Event('servr:reload'));
  location.reload();
};
setInterval(function() {
  if (flag === false || ws.readyState !== ws.OPEN) return;
  flag = false;  // prevent ws message if R hasn't responded yet
  ws.send('{}');
}, !!SERVR_INTERVAL);
