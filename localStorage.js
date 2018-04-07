var elmDiv = document.getElementById('elm-app');
var app = Elm.Main.embed(elmDiv);

app.ports.saveLocation.subscribe(function(locationString) {
  var items = window.localStorage.getItem('elmLocations') || "";
  var newItems;
  if (items === "") {
    newItems = locationString;
  } else {
    newItems = items + ":" + locationString;
  }
  window.localStorage.setItem('elmLocations', newItems);
});

app.ports.requestLocations.subscribe(function() {
  var locations = window.localStorage.getItem('elmLocations') || "";
  app.ports.receiveLocations.send(locations);
});
