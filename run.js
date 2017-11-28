'use strict'

var dateToSeconds = function(date) {
  return Math.floor(dateFns.getTime(date) / 1000)
}

var node = document.getElementById('app-container')
var startOfMonth = dateFns.startOfMonth

// Get month ranges from now on to the launch of Last.fm.
var months = []
var lastFmLaunchDate = startOfMonth(dateFns.parse('2002-03-20'))
var currentDate = startOfMonth(new Date())

while (dateFns.isAfter(currentDate, lastFmLaunchDate) || dateFns.isEqual(currentDate, lastFmLaunchDate)) {
  var endOfMonth = dateFns.endOfMonth(currentDate)

  var formattedMonth = dateFns.format(currentDate, 'YYYY-MM')

  months.push({
    month: formattedMonth,
    start: dateToSeconds(currentDate),
    end: dateToSeconds(endOfMonth),
  })

  currentDate = dateFns.subMonths(currentDate, 1)
}

var app = Elm.Main.embed(node, {
  months: months
})
