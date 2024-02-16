// automatically refresh the page when necessary (R will send a message to ws)
((interval, path, ws) => {
let flag;
ws.onmessage = e => {
  flag = true;
  if (e.data === 'false' || e.data === 'null') return;
  // fire a servr:reload event
  Event && document.dispatchEvent(new Event('servr:reload'));
  location.reload();
};
setInterval(() => {
  if (flag === false || ws.readyState !== ws.OPEN) return;
  flag = false;  // prevent ws message if R hasn't responded yet
  ws.send(path);
}, interval);
})
