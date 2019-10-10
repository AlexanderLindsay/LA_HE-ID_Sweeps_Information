// import './parseCare.js'
import L from 'leaflet'
import 'leaflet-control-geocoder'

// const container = DOM.element('div', { style: `width:${width}px;height:${width / 1.6}px` })

const map = L.map('map').setView([51.505, -0.09]).setZoom(13)

L.tileLayer(
  'https://api.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token={accessToken}',
  {
    attribution:
      'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="https://www.mapbox.com/">Mapbox</a>',
    maxZoom: 18,
    id: 'mapbox.streets',
    accessToken: process.env.MAPBOX_TOKEN,
  },
).addTo(map)

const geocoder = L.Control.Geocoder.nominatim()


const app = Elm.Main.init({
  node: document.querySelector('#root'),
})

app.ports.plotSweeps.subscribe((data) => {
  console.log(data)
  if (data.length) {
    geocoder.geocode(data[0], (result) => {
      if (result.length) {
        const { center } = result[0]
        console.log(center)
        L.marker(center, { title: data[0] }).addTo(map)
        map.setView(center)
      }
    })
  }
})

/*
L.marker([50.5, 30.5], { title: '' }).addTo(map);

const popup = L.popup().setContent()
marker.bindPopup(popup);

marker.bindTooltip("my tooltip text").openTooltip();
*/
