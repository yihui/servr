((interval, path, ws) => {
let flag;
ws.onmessage = e => {
  flag = true;
  if (e.data === '') return;
  // alert error message
  if (/^Error:/.test(e.data)) {
    alert(e.data); return;
  }
  // replace the document body
  document.body.innerHTML = e.data;
  // re-run JS
  document.querySelectorAll('script[src]').forEach(el => {
    const d = document.createElement('script');
    el.after(d);
    d.defer = true;
    d.src = el.src;
    el.remove();
  });
  // reload images by forcing a query param
  document.querySelectorAll('img').forEach(el => {
    el.src = el.src + '?t=' + (+new Date());
  });
};
setInterval(() => {
  if (flag === false || ws.readyState !== ws.OPEN) return;
  flag = false;  // prevent ws message if R hasn't responded yet
  ws.send(path);
}, interval);
})
