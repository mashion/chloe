socket = new io.Socket();

socket.on('connect', function () {
  socket.send('hi');
});

socket.on('message', function (data) {
  console.log(data);
});

socket.on('disconnect', function () {
});

socket.connect();
