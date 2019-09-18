// server.js
// where your node app starts

// init project
const express = require('express');
var moment = require('moment-timezone');
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
        return { id: +parts[0] };
      });
    
    response.json(dates);
  });
});

app.get('/api/sweeps/today', function(request, response) {
  const requestedDate = moment.tz('America/Los_Angeles');
  const dateId = requestedDate.startOf('date').hours(8).minute(30).utcOffset(0, true).valueOf();
  const path = `data/${dateId}.json`;
  
  if(fs.existsSync(path)) {  
    const readable = fs.createReadStream(path);
    response.setHeader('Content-Type', 'application/json');
    readable.pipe(response);
  } else {
    response.status(404).send('Not Found');
  }
});

app.get('/api/sweeps/:date', function(request, response) {
  const requestedDate = moment(+request.params.date);
  const path = 'data/' + requestedDate.valueOf() + '.json';
  
  if(fs.existsSync(path)) {  
    const readable = fs.createReadStream(path);
    response.setHeader('Content-Type', 'application/json');
    readable.pipe(response);
  } else {
    response.status(404).send('Not Found');
  }
});

// listen for requests :)
const listener = app.listen(process.env.PORT, function() {
  console.log('Your app is listening on port ' + listener.address().port);
});
