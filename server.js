// server.js
// where your node app starts

// init project
const express = require('express');
const moment = require('moment-timezone');
const app = express();
const fs = require('fs');

const build = require('./build');
build();

app.use(express.static('public'));

app.get('/', function(request, response) {
  response.sendFile(__dirname + '/views/index.html');
});

app.get('/api/days', function(request, response) {
  const today = new Date();
  
  fs.readdir('./data', (err, files) => {
    const dates = 
      files
        .filter(f => f !== 'lastChanged.txt')
        .map(f => {
        const parts = f.split('.');
        const date = parts[0];
        return { id: moment.tz(date, 'YYYY-MM-DD', 'America/Los_Angeles').valueOf() };
      });
    
    response.json(dates);
  });
});

function isValidDateFormat(date) {
  const checker = /\d{4}-\d{2}-\d{2}/;
  return checker.test(date);
}

function getDataFromFile(path, date) {
  const promise = new Promise((resolve, reject) => {
    if(fs.existsSync(path)) {
      fs.readFile(path, (err, text) => {
        if (err) {
          console.log('error reading file', err);
          reject(err);
        }
        
        const data = JSON.parse(text);
        resolve(data);
      });
      
    } else {
      const isoDate = moment.tz(date, 'YYYY-MM-DD', 'America/Los_Angeles').toISOString();
      resolve({ date: isoDate, activities: [], url: '', name: '' });
    }
  });
  
  return promise;
}

function createFile(data) {
  if(data.name === '' || data.url === '') {
    return undefined;
  }
  
  return { url: data.url, name: data.name };
}

app.get('/api/sweeps/:date', function(request, response) {
  const checker = /\d{4}-\d{2}-\d{2}/;
  const date = request.params.date;
  if(!checker.test(date)) {
    response.status(400).send('Invalid request, date must be in the YYYY-MM-DD format');
  }
  
  const path = `data/${date}.json`;
  const futurePath = `data/${date}-future.json`;
  
  const currentDataPromise = getDataFromFile(path, date);
  const futureDataPromise = getDataFromFile(futurePath, date);
  
  Promise.all([currentDataPromise, futureDataPromise]).then(values => {
    const current = values[0];
    const future = values[1];
    
    const currentFile = createFile(current);
    const futureFile = createFile(future);
    
    const result = { date: current.date, activities: current.activities.concat(future.activities), currentFile, futureFile };
    
    response.json(result);
  }).catch(err => {
    response.status(400).send(`Error getting data: ${err}`);
  });
  
});

app.get('/api/csv/:date', function(request, response) {
  const date = request.params.date
  if(!isValidDateFormat(date)) {
    response.status(400).send('Invalid request, date must be in the YYYY-MM-DD format');
  }
  
  const path = `data/${date}.json`;
  const futurePath = `data/${date}-future.json`;
  
  const currentDataPromise = getDataFromFile(path, date);
  const futureDataPromise = getDataFromFile(futurePath, date);
  
  Promise.all([currentDataPromise, futureDataPromise]).then(values => {
    const current = values[0];
    const future = values[1];
    
    const csv = current.activities
      .concat(future.activities.map(a => { a.status = `Unconfirmed - ${a.status}`; return a;}))
      .map((activity) => {
        //{"authNumber":"","address":"","crossStreetOne":"","crossStreetTwo":"","location":"","comments":"","cleaningTime":"","division":"","status":""}
        //{"idTeam":"","address":"","location":"","comments":"":"","status":""}
        return [activity.authNumber, activity.idTeam, activity.address, activity.crossStreetOne, activity.crossStreetTwo, activity.location, activity.comments, activity.cleaningTime, activity.division, activity.status]
          .map(v => `"${v||''}"`)
          .join(',');
      });

    csv.unshift('authNumber,idTeam,address,crossStreetOne,crossStreetTwo,location,comments,cleaningTime,division,status');
    
    let baseName = current.name;
    if(baseName === '') {
      baseName = future.name;
    }
    const nameParts = baseName.split('.');
    let secondPart = nameParts[1] || '';
    if(secondPart === 'pdf') {
      secondPart = '';
    } else {
      secondPart = '-' + secondPart;
    }
    const csvName = `${nameParts[0]}${secondPart}.csv`;
          
    response.setHeader('Content-Type', 'text/csv');
    response.setHeader('Content-Disposition', `attachment;filename="${csvName}"`);
    response.status(200).send(csv.join('\n'));
  }).catch(err => {
    response.status(404).send("Activities for that date do not exist.");
  });
});

// listen for requests :)
const listener = app.listen(process.env.PORT || 5555, function() {
  console.log('Your app is listening on port ' + listener.address().port);
});
