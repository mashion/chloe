var chloe = new Chloe({host: 'localhost', port: 8888});

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

// TODO: We seem to be sending data down the socket too quickly and
//       causing an error. Need to figure out why.
//    What we see in Chrome:
//       Uncaught Error: INVALID_STATE_ERR: DOM Exception 11
setTimeout(function () {
  chloe.subscribe('pumpkin', function (message) {
    console.log('Someone was eating pumpkins: ' + message);
  });
}, 1);
