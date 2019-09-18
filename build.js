const fs = require('fs');
const moment = require('moment');
const pdfjsLib = require('pdfjs-dist');

const changeMarkerFilePath = './data/lastChanged.txt';

module.exports = function run() {
  console.log("Building...");
  let lastChanged = null;
  try {
    const changeFileStats = fs.statSync(changeMarkerFilePath);
    const ms = changeFileStats.birthtimeMs;
    lastChanged = moment(ms);
  } catch(e) {
    console.log(`error with change file: ${e}`);
  }

  var content = fs.readFileSync('.glitch-assets', 'utf8');
  var rows = content.split("\n");
  var assets = rows.map((row) => {
    try {
      return JSON.parse(row);
    } catch (e) {}
  }).filter(asset => asset);

  const deleted =
      assets
        .filter(asset => asset.deleted === true)
        .map(asset => asset.uuid)
        .reduce((m, a) => {
          m.set(a, true);
          return m;
        }, new Map());

  const activeAssets =
      assets
        .filter(asset => !deleted.has(asset.uuid))
        .filter(asset => {
          if(lastChanged === null) {
            return true;
          }

          const uploadedDate = moment(asset.date, moment.ISO_8601);
          return uploadedDate.isAfter(lastChanged);
        });

  const sectionHeaders = /(Team .*)|OHS - Central Division/;

  function withInDelta(delta, source, value) {
    return value >= (source - delta) && value <= (source + delta);
  }

  const withIn = withInDelta.bind(null, 3);

  function columnContains(mode, column, cell) {
    let padding = 20;

    if(mode === 'maintenance') {
      if(column.text === 'ID TEAM') {
        padding = 5;
      }
      
      if(column.text === 'Address') {
        padding = 75;
      }

      if(column.text === 'Comments/Survey') {
        padding = 50;
      }
    }

    if(column.text === 'Est. Clean Time') {
      padding = 3;
    }

    if(column.text === 'Status') {
      padding = 30;
    }

    return (column.start - padding) <= cell.start && ( column.end + padding) >= cell.end
  }

  function mapRowToColumns(mode, obj, indexMap, columns, row) {
    columns.forEach((column, index) => {
      for(let cell of row) {
        if(columnContains(mode, column, cell)) {
          obj[indexMap[index]] += cell.text;
        }
      }
    });
  }

  function isValidDivision(division) {

    if(division.address === '') {
      return false;
    }

    return true;
  }

  function isValidMaintenance(maintenance) {
    if(maintenance.address === '') {
      return false;
    }

    return true;
  }

  activeAssets
    .map(asset => {
      const loadingTask = pdfjsLib.getDocument(asset.url);
      loadingTask.promise.then(function(doc) {
        var numPages = doc.numPages;

      const rows = [];

      var loadPage = function (pageNum) {
        return doc.getPage(pageNum).then(function (page) {
          return page.getTextContent().then(function (content) {

            const sameCell = content.items.reduce((acc, item) => {
              if (acc.previous === null) {
                return { results: acc.results, row: acc.row, previous: item, rowY: item.transform[5] };
              }

              const currentY = item.transform[5];
              const currentX = item.transform[4];

              const previousX = acc.previous.transform[4];
              if(previousX === currentX || withInDelta(2, previousX + acc.previous.width, currentX)) {
                  return { results: acc.results, row: acc.row, previous: { str: acc.previous.str + item.str, transform: acc.previous.transform, width: acc.previous.width }, rowY: acc.rowY };
              } else if(withIn(acc.rowY, currentY) || currentY > acc.rowY) {           
                  acc.row.push({ text: acc.previous.str, start: previousX, end: previousX + acc.previous.width });

                  return { results: acc.results, row: acc.row, previous: item, rowY: acc.rowY };
              } else {
                acc.row.push({ text: acc.previous.str, start: previousX, end: previousX + acc.previous.width });
                acc.results.push(acc.row);
                return { results: acc.results, row: [], previous: item, rowY: currentY };
              }
            }, { results: [], row: [], previous: null, rowY: null });

            if(sameCell.row.length > 0) {
              sameCell.results.push(sameCell.row);
            }
            const newRows = sameCell.results;
            rows.push.apply(rows, newRows);
          });
        });
      };
        let lastPromise = loadPage(1);
        for (var i = 2; i <= numPages; i++) {
          lastPromise = lastPromise.then(loadPage.bind(null, i));
        }

        function parseData(rows) {
          const rawDate = (rows[1] || [])[0].text || '';
          const momentDate = moment(rawDate, 'dddd, MMMM Do [@] h:mm');
          const dateId = momentDate.valueOf();
          const date = momentDate.toISOString();

          let mode = '';
          let headers = [];
          let data = [];
          let invalidRows = [];

          const divisionIndexMap = [ 'authNumber', 'address', 'crossStreetOne', 'crossStreetTwo', 'location', 'comments', 'cleaningTime', 'division', 'status' ];
          const maintenanceIndexMap = [ 'idTeam', 'address', 'location', 'comments', 'division', 'status' ];

          for(let row of rows) {
              if(row.length === 1) {
                continue;
              }

              if(row[0].text === 'Auth#') {
                mode = 'division';
                headers = row;
                continue;
              } else if (row[0].text === 'ID TEAM') {
                mode = 'maintenance';
                headers = row;
                continue;
              }

              if(mode === 'division') {

                const division = 
                      { authNumber: ''
                      , address: ''
                      , crossStreetOne: ''
                      , crossStreetTwo: ''
                      , location: ''
                      , comments: ''
                      , cleaningTime: ''
                      , division: ''
                      , status: ''
                      };

                mapRowToColumns(mode, division, divisionIndexMap, headers, row);

                if(isValidDivision(division)) {
                  data.push(division);
                } else {
                  invalidRows.push(division);
                }
                continue;
              }

              if(mode === 'maintenance') {

                const maintenance = 
                      { idTeam: ''
                      , address: ''
                      , location: ''
                      , comments: ''
                      , division: ''
                      , status: '' 
                      };

                mapRowToColumns(mode, maintenance, maintenanceIndexMap, headers, row);

                if(isValidMaintenance(maintenance)) {
                  data.push(maintenance);
                } else {
                  invalidRows.push(maintenance);
                }
              }
          }

          return { dateId, date, activities: data, invalid: invalidRows };
        }

        lastPromise.then(() => {
          const data = parseData(rows);

          const fileData = JSON.stringify({ date: data.date, activities: data.activities, url: asset.url, name: asset.name });

          fs.writeFile('./data/' + data.dateId + '.json', 
               fileData, 
               (err) => {
                  if (err) throw err;
                  console.log('The file has been saved!');
              });

          const fileDataInvalid = JSON.stringify({ date: data.date, invalid: data.invalid});

          fs.writeFile('./invalid/' + data.dateId + '-invalid.json', 
               fileDataInvalid, 
               (err) => {
                  if (err) throw err;
                  console.log('The file of invalid records has been saved!');
              });

          fs.closeSync(fs.openSync(changeMarkerFilePath, 'w'));
        });

        return lastPromise;
      }).then(function () {
        console.log('# End of Document');
      }, function (err) {
        console.error('Error: ' + err);
      });
    });
}
