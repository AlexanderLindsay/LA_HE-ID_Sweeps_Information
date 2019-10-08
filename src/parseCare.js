import XLSX from 'xlsx'

const url = 'https://archive.org/download/lasanitationencampmentsweepconfirmationsheets/CARE%20Program%20Confirmation%20Sheet%2010.7.xlsx'

const workbook = fetch(url)
  .then((res) => {
    if (!res.ok) throw new Error('fetch failed')
    return res.arrayBuffer()
  })
  .then((ab) => {
    const data = new Uint8Array(ab)
    return XLSX.read(data, { type: 'array' })
  })
  .then(console.log)
