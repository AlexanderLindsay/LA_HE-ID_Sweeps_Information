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
        const date = parts[0];
        return { id: moment.tz(date, 'YYYY-MM-DD', 'America/Los_Angeles').valueOf() };
      });
    
    response.json(dates);
  });
});

app.get('/api/sweeps/:date', function(request, response) {
  const checker = /\d{4}-\d{2}-\d{2}/;
  const date = request.params.date;
  if(!checker.test(date)) {
    response.status(400).send('Invalid request, date must be in the YYYY-MM-DD format');
  }
  const path = 'data/' + date + '.json';
  
  if(fs.existsSync(path)) {  
    const readable = fs.createReadStream(path);
    response.setHeader('Content-Type', 'application/json');
    readable.pipe(response);
  } else {
    const isoDate = moment.tz(date, 'YYYY-MM-DD', 'America/Los_Angeles').toISOString();
    response.json({ date: isoDate, activities: [], url: '', name: '' });
  }
});

// listen for requests :)
const listener = app.listen(process.env.PORT, function() {
  console.log('Your app is listening on port ' + listener.address().port);
});
