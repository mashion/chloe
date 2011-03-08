var chloe = new Chloe({host: 'localhost', port: 8888, transport: 'XHR'});

chloe.onmessage(function (message) {
  console.log('I got a message too: ' + message);
});

chloe.onclose(function () {
  console.log("And... we're closed.");
});

chloe.connect(function () {
  console.log('Holy shit, connected!');
  chloe.send('A');
});

