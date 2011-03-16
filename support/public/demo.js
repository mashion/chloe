var chloe = new Chloe({host: 'localhost', port: 8901, transport: 'jsonp'});

chloe.onmessage(function (message) {
  console.log('I got a message too: ' + message);
});

chloe.onclose(function () {
  console.log("And... we're closed.");
});

chloe.connect(function () {
  console.log('Holy crap, connected!');
  chloe.send('Ohai Chloe!');

//  chloe.subscribe('pumpkin', function (message) {
//    console.log('Someone was eating pumpkins: ' + message);
//  });
});

