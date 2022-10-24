var map = L.map('map').setView([51.505, -0.09], 5);

L.tileLayer('https://api.mapbox.com/styles/v1/{id}/tiles/{z}/{x}/{y}?access_token={accessToken}', {
    attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
    maxZoom: 18,
    id: 'mapbox/streets-v11',
    tileSize: 512,
    zoomOffset: -1,
    accessToken: 'pk.eyJ1IjoidGVzdGluZ2lzYWFjIiwiYSI6ImNsMGtkOGRkYTA5ejYzZG8zbHdkZnhpemgifQ.Ue-vST7TBuEQ9JIkSLZHRw'
}).addTo(map);

map.pm.addControls({
  drawMarker: false,
  drawPolygon: true,
  drawRectangle: false,
  drawCircle: false,
  drawCircleMarker: false,
  editMode: false,
  drawPolyline: false,
  removalMode: false, //Make true soon
  cutPolygon: false,
  rotateMode: false,
  dragMode: false,
  drawText: false,
  position: 'topright'
});

var polygon = ["POLYGON(("];
var numPolygons = 0
var firstEntry = true
var firstVector = 0
var secondVector = 0

map.on('pm:drawstart', (e) => {
    if (numPolygons >= 1) {
        if (numPolygons >= 2) {
            polygon.pop()
            polygon.push("))")
        }
        polygon[0] = "MULTIPOLYGON((("
        polygon.push(",((")
    }
});

map.on('pm:drawstart', ({ workingLayer }) => {
  workingLayer.on('pm:vertexadded', (e) => {
    polygon.push(e.latlng.lng);
    polygon.push(" ")
    polygon.push(e.latlng.lat);
    polygon.push(",")

    if (firstEntry == true) {
        firstVector = e.latlng.lng
        secondVector = e.latlng.lat
        firstEntry = false
    }

    elementsInFirstPolygon += 4
  });
});

map.on('pm:drawend', (e) => {
    polygon.push(firstVector)
    polygon.push(" ")
    polygon.push(secondVector)
    if (numPolygons >= 1) {
        polygon.push(")))")
    }
    else {
        polygon.push("))")
    }
    numPolygons += 1
    firstEntry = true
    Shiny.setInputValue("polygon", polygon.join(''));
});
