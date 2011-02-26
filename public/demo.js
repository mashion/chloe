var chloe = new Chloe();
chloe.onmessage(function (message) {
  console.log('I got a message too: ');
  console.dir(message);
});

chloe.connect(function () {
  console.log('Holy shit, connected!');
  chloe.send('ZOMG I sent a message!');
});


