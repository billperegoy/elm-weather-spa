var elmDiv = document.getElementById('elm-app');
var app = Elm.Main.embed(elmDiv, {apiKey: "c83a6598d579714d"});

app.ports.saveLocation.subscribe(function(locationString) {
  var items = window.localStorage.getItem('elmLocations') || "";
  var itemsCleaned = items.replace(/ /g,'');
  var locationStringCleaned = locationString.replace(/ /g,'');

  var newItems;
  if (itemsCleaned === "") {
    newItems = locationString;
  } else {
    if (itemsCleaned.includes(locationStringCleaned)) {
      newItems = itemsCleaned;
    } else {
      newItems = itemsCleaned + ":" + locationStringCleaned;
    }
  }
  window.localStorage.setItem('elmLocations', newItems);
});

app.ports.deleteLocation.subscribe(function(locationString) {
  var items = window.localStorage.getItem('elmLocations') || "";
  var itemsCleaned = items.replace(/ /g,'');
  var locationStringCleaned = locationString.replace(/ /g,'');
  var regex = new RegExp(locationStringCleaned);
  var newItems = itemsCleaned.replace(regex, '')
                             .replace(/^:/, '')
                             .replace(/:$/, '')
                             .replace(/::/g, ':');

  window.localStorage.setItem('elmLocations', newItems);
});

app.ports.requestLocations.subscribe(function() {
  var locations = window.localStorage.getItem('elmLocations') || "";
  app.ports.receiveLocations.send(locations);
});
