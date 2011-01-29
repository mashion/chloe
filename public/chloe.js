Chloe = function () {
};

Chloe.init = function (initFunction) {
  var includeScript = function (scriptName) {
    var head   = document.getElementsByTagName('head')[0],
        script = document.createElement('script');
    script.type = 'text/javascript';
    // TODO: Include a way to specify host for js
    script.src  = scriptName + '.js';
    head.appendChild(script);
  };

  includeScript('bert');
  initFunction();
};
