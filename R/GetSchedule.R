#' Schedule and Results.
#'
#' @param sport 'NBA' or 'NCAAB'
#' @param year season (e.g. 2008 for the 2007-08 season)
#' @param season.type either 'regular', 'playoffs', or 'both'
#' @param info either 'scores' or 'advanced'
#' @return data frame with schedule and results for each game in that season
#' @keywords schedule
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetSchedule(2014, 'playoffs')

GetSchedule <- function(sport = 'nba', year, season.type = 'regular', info = 'scores') {
  if (sport == 'NBA') {
    return(.GetScheduleNBA(year, season.type, info))
  } else if (sport == 'NCAAB') {
    return(.GetScheduleNCAAB(year, season.type, info))
  }
}

#' Schedule and results (multi-year).
#'
#' @param sport 'NBA' or 'NCAAB'
#' @param year.start season (e.g. 2008 for 2007-08 season)
#' @param year.end season (e.g. 2014 for 2013-14 season)
#' @param season.type either 'regular' or 'playoffs' or 'both'
#' @param info either 'scores' or 'advanced'
#' @return data frame with schedule and results for each game in all the seasons
#' @keywords schedule
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetScheduleRange(2012, 2015, 'playoffs')

GetScheduleRange <- function(sport = 'NBA', year.start, year.end, season.type = 'both', info = 'scores') {
  schedule <- data.frame()
  
  for (year in year.start:year.end) {
    temp <- GetSchedule(sport, year, type, info)
    temp$season <- year
    schedule <- rbind(schedule, temp)
  }
  
  return(schedule)
}

.GetScheduleNBA <- function(year, season.type = 'regular', info = 'scores') {
  
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
    
    schedule <- data.frame()
    for (team.id in team.ids$id) {
      temp <- GetTeamSpecificStats(team.id, 'advanced gamelog', year)
      temp$Team <- team.id
      schedule <- rbind(schedule, temp)
    }
    
    schedule <- schedule[schedule$Home == 1, c(2, 26, 4, 6:25)]
    colnames(schedule) <- c('date', 'home.name', 'away.name', 'home.score', 'away.score',
                            'home.ortg', 'away.ortg', 'pace', 'home.ftr', 'home.3par',
                            'home.ts', 'home.trb', 'home.ast', 'home.stl', 'home.blk', 
                            'home.efg', 'home.tov', 'home.orb', 'home.ft.fga', 'away.efg', 
                            'away.tov', 'away.drb', 'away.ft.fga')
  }
  
  return(schedule)
}

.GetScheduleNCAAB <- function(year, season.type = 'regular', info = 'scores') {
  
  options(stringsAsFactors = FALSE)
  
  team.ids <- GetTeamIDs(sport = 'NCAAB', year, source = 'Sports-Reference')
  schedule <- data.frame()
  
  if (info == 'scores') {
    for (i in 1:nrow(team.ids)) {
      url <- paste0('http://www.sports-reference.com/cbb/schools/', team.ids[i, 'id'], '/', year, '-schedule.html')
      
      temp <- readHTMLTable(url)[['schedule']]
      temp$team <- team.ids[i, 'name']
      schedule <- rbind(schedule, temp)
      
      cat(i, '\n')
    }
    
    schedule$Opponent <- gsub('\\([0-9]*)', '', schedule$Opponent)
    
    # Remove extra columns
    schedule <- schedule[, c(2, 6, 7, 10, 11, 17)]
    schedule$Date <- as.Date(strptime(schedule$Date, format = '%a, %b %d, %Y'))
    schedule[, 4] <- as.numeric(schedule[, 4])
    schedule[, 5] <- as.numeric(schedule[, 5])
    
    # Create winner and loser columns
    schedule$winner.points <- apply(schedule, 1, function(x) max(x[4], x[5]))
    schedule$loser.points <- apply(schedule, 1, function(x) min(x[4], x[5]))
    schedule$winner.name <- apply(schedule, 1, function(x) ifelse(x[4] == x[7], x[6], x[3]))
    schedule$loser.name <- apply(schedule, 1, function(x) ifelse(x[4] == x[7], x[3], x[6]))
    
    # Create home column
    schedule$home <- 0
    schedule[schedule[, 2] == '', 'home'] <- 1
    schedule[schedule[, 2] == '@', 'home'] <- -1
    schedule[which(schedule$team == schedule$loser.name), 'home'] <- -schedule[which(schedule$team == schedule$loser.name), 'home']
    
    # Fill in NAs (games that haven't happened yet)
    schedule[is.na(schedule$winner.name), 'winner.name'] <- schedule[is.na(schedule$winner.name), 'team']
    schedule[is.na(schedule$loser.name), 'loser.name'] <- schedule[is.na(schedule$loser.name), 'Opponent']
    
    # Clean up columns
    schedule <- schedule[, c(1, 9, 10, 11, 7, 8)]
    colnames(schedule) <- c('date', 'winner.name', 'loser.name', 'home', 'winner.points', 'loser.points')
    schedule$margin <- schedule$winner.points - schedule$loser.points
    
    # Remove duplicate rows
    schedule <- schedule[!duplicated(schedule), ]
  }
  
  return(schedule)
}