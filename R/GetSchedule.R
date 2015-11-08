#' Schedule and Results.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param type either "regular season" or "playoffs"
#' @return data frame with schedule and results for each game in that season
#' @keywords schedule
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetSchedule(2014, 'playoffs')

GetSchedule <- function(year, type = 'regular season') {
  
  options(stringsAsFactors = FALSE)
  
  url <- paste("http://www.basketball-reference.com/leagues/NBA_", year, "_games.html", sep = "")
  tables <- readHTMLTable(url)
  
  if (type == 'playoffs') {
    schedule <- tables[['games_playoffs']]
  } else if (type == 'regular season') {
    schedule <- tables[['games']]
  } else {
    schedule <- rbind(tables[['games']], tables[['games_playoffs']])
  }

  # Remove extra columns
  schedule <- schedule[, c(1, 3, 4, 5, 6, 7, 8)]
  schedule$Date <- as.Date(strptime(schedule$Date, format = "%a, %b %d, %Y"))
  schedule[, 3] <- as.numeric(schedule[, 3])
  schedule[, 5] <- as.numeric(schedule[, 5])
  
  colnames(schedule) <- c("date", "away.name", "away.points", "home.name", "home.points", "overtime", "notes")
  return(schedule)
}

#' Schedule and results (multi-year).
#'
#' @param year.start season (e.g. 2008 for 2007-08 season)
#' @param year.end season (e.g. 2014 for 2013-14 season)
#' @param type either "regular season" or "playoffs" or "both"
#' @return data frame with schedule and results for each game in all the seasons
#' @keywords schedul
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetLinesRange(2012, 2015, "playoffs")

GetLinesRange <- function(year.start, year.end, type = "both") {
  schedule <- data.frame()
  
  for (year in year.start:year.end) {
    temp <- GetSchedule(year, type)
    temp$season <- year
    schedule <- rbind(schedule, temp)
  }
  
  return(schedule)
}