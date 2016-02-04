#' Schedule and Results.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type either 'regular', 'playoffs', or 'both'
#' @param info either 'scores' or 'advanced'
#' @return data frame with schedule and results for each game in that season
#' @keywords schedule
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetSchedule(2014, 'playoffs')

GetSchedule <- function(year, season.type = 'regular', info = 'scores') {
  
  options(stringsAsFactors = FALSE)
  
  if (info == 'scores') {
    url <- paste('http://www.basketball-reference.com/leagues/NBA_', year, '_games.html', sep = '')
    tables <- readHTMLTable(url)
    
    if (season.type == 'playoffs' & length(tables) > 1) {
      schedule <- tables[['games_playoffs']]
      schedule$type <- 'playoff'
    } else {
      schedule <- tables[['games']]
      schedule$type <- 'regular season'
    } 
    
    if (season.type == 'both' & length(tables) > 1) {
      temp <- tables[['games_playoffs']]
      temp$type <- 'playoff'
      schedule <- rbind(schedule, temp)
    }
    
    # Remove extra columns
    schedule <- schedule[, c(1, 4, 5, 6, 7, 10)]
    schedule$Date <- as.Date(strptime(schedule$Date, format = '%a, %b %d, %Y'))
    schedule[, 3] <- as.numeric(schedule[, 3])
    schedule[, 5] <- as.numeric(schedule[, 5])
    
    colnames(schedule) <- c('date', 'away.name', 'away.points', 'home.name', 'home.points', 'type')
    schedule$home.margin <- schedule$home.points - schedule$away.points
    
  } else if (info == 'advanced') {
    team.ids <- GetTeamIDs(year, 'Basketball-Reference')
    
    df <- data.frame()
    for (team.id in team.ids$id) {
      
    }
  }
  
  return(schedule)
}

#' Schedule and results (multi-year).
#'
#' @param year.start season (e.g. 2008 for 2007-08 season)
#' @param year.end season (e.g. 2014 for 2013-14 season)
#' @param season.type either 'regular' or 'playoffs' or 'both'
#' @return data frame with schedule and results for each game in all the seasons
#' @keywords schedule
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetScheduleRange(2012, 2015, 'playoffs')

GetScheduleRange <- function(year.start, year.end, season.type = 'both') {
  schedule <- data.frame()
  
  for (year in year.start:year.end) {
    temp <- GetSchedule(year, type)
    temp$season <- year
    schedule <- rbind(schedule, temp)
  }
  
  return(schedule)
}