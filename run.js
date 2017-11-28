'use strict'

var dateToSeconds = function(date) {
  return Math.floor(dateFns.getTime(date) / 1000)
}

var apiKey = '174e471a5d6d47b8c4ca009045595409'
var node = document.getElementById('app-container')
var startOfMonth = dateFns.startOfMonth

// Get month ranges from now on to the launch of Last.fm.
var months = []
var lastFmLaunchDate = startOfMonth(dateFns.parse('2002-03-20'))
var currentDate = startOfMonth(new Date())
var currentMonth = null

while (dateFns.isAfter(currentDate, lastFmLaunchDate) || dateFns.isEqual(currentDate, lastFmLaunchDate)) {
  var endOfMonth = dateFns.endOfMonth(currentDate)

  var formattedMonth = dateFns.format(currentDate, 'YYYY-MM')

  var month = {
    month: formattedMonth,
    start: dateToSeconds(currentDate),
    end: dateToSeconds(endOfMonth),
  }
  months.push(month)

  if (!currentMonth) {
    currentMonth = month
  }

  currentDate = dateFns.subMonths(currentDate, 1)
}

var app = Elm.Main.embed(node, {
  months: months,
  currentMonth: currentMonth,
  apiKey: apiKey,
})
