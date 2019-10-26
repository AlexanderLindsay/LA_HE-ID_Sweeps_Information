// import './parseCare.js'
import L from 'leaflet'
import { BingProvider } from 'leaflet-geosearch'
import { Elm } from './Main.elm'

const log = (...args) => {
  if (process.env.NODE === 'development') {
    // eslint-disable-next-line no-console
    console.log(...args)
  }
}

const app = Elm.Main.init({
  node: document.querySelector('#root'),
})

// const container = DOM.element('div', { style: `width:${width}px;height:${width / 1.6}px` })

const mapEl = document.querySelector('#map')
const map = L.map('map').setView([34.0536909, -118.2427666]).setZoom(13)

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

const provider = new BingProvider({
  params: {
    key: process.env.BING_KEY,
  },
})

// rate limit my requests by 50ms
const reqQ = []
setInterval(() => { if (reqQ.length) reqQ.shift()() }, 50)

// memoize
const geocodeCache = new Map()
const geocode = (address) => {
  if (geocodeCache.has(address)) {
    return Promise.resolve(geocodeCache.get(address))
  }

  return new Promise((resolve, reject) => {
    reqQ.push(() => provider.search({ query: address })
      .then((result) => {
        if (!result.length) throw new Error('no point found for ', address)
        const { x, y } = result[0]
        const point = L.latLng(y, x)
        geocodeCache.set(address, point)
        resolve(point)
      })
      .catch((err) => reject(err)))
  })
}

const validAddress = (address) => /\d{5}$/.test(address)

let layerGroup
app.ports.plotSweeps.subscribe((data) => {
  log('sweep data from elm', data)

  if (data.length) {
    mapEl.style.display = 'block'
    if (layerGroup) layerGroup.remove()
    layerGroup = L.layerGroup().addTo(map)

    const geocoded = data
      .filter(validAddress)
      .map((address) => geocode(address)
        .then((point) => {
          const marker = L.marker(point)
          marker.bindTooltip(address)
          marker.addTo(layerGroup)
          // map.setView(point)
          return point
        })
        .catch(console.error))

    Promise.all(geocoded)
      .then((points) => map.fitBounds(points))
  } else {
    mapEl.style.display = 'none'
  }
})

/*
const popup = L.popup().setContent()
marker.bindPopup(popup);
*/
