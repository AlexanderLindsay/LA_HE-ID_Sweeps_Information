import './parseCare.js'

var app = Elm.Main.init({
  node: document.querySelector("#root")
});

app.ports.plotSweeps.subscribe(data => {
  console.log(data);
});