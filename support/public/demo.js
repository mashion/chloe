var chloe = new Chloe({host: 'localhost', port: 8888});

chloe.onmessage(function (message) {
  console.log('I got a message too: ' + message);
});

chloe.onclose(function () {
  console.log("And... we're closed.");
});

chloe.connect(function () {
  console.log('Holy shit, connected!');
  chloe.send('Ohai Chloe!');
  chloe.send('your mom');
  chloe.send('blasdf');

  chloe.subscribe('pumpkin', function (message) {
    console.log('Someone was eating pumpkins: ' + message);
  });
});

