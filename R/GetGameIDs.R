#' Get Game IDs.
#'
#' @param year Season (e.g. 2008 for the 2007-08 season)
#' @param season.type Either "Regular Season", "Playoffs", or "Both"
#' @param date If provided, target date for game IDs
#' @return data.frame
#' @keywords Game
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetGameIDs(2014, 'Regular Season')

GetGameIDs <- function(year = CurrentYear(), 
                       season.type = 'Regular Season',
                       date) {
  
  options(stringsAsFactors = FALSE)
  
  if (missing(date)) {
    all.games <- data.frame()
    teams <- GetTeamIDs(year = year)
    
    for (id in teams$id) {
      temp <- GetGameIDsTeam(team = id, year, season.type)
      all.games <- rbind(all.games, temp)
    }
    
    return(unique(all.games))
  } else {
    if (class(date) == 'character') {
      date <- as.Date(date, origin = '1970-01-01')      # Convert epoch (days) to date object
    }
    return(GetGameIDsDay(date))
  }
}

#' Get Game IDs for a team
#'
#' @param team target date
#' @param year Season (e.g. 2008 for the 2007-08 season)
#' @param season.type Either "Regular Season", "Playoffs", or "Both"
#' @return data.frame
#' @keywords Game
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetGameIDsTeam('Houston Rockets', 2017, 'Regular Season')

GetGameIDsTeam <- function(team, year, season.type = 'Regular Season') {
  
  options(stringsAsFactors = FALSE)
  
  if (season.type == 'Both') {
    ids.regular <- GetGameIDsTeam(team, year, 'Regular Season')
    ids.playoffs <- GetGameIDsTeam(team, year, 'Playoffs')
    return(rbind(ids.regular, ids.playoffs))
  }
  
  request <- GET(
    "http://stats.nba.com/stats/teamgamelog",
    query = list(
      LeagueId = '00',
      Season = YearToSeason(year),
      SeasonType = season.type,
      TeamID = TeamNameToID(team)
    ),
    add_headers('Referer' = 'http://stats.nba.com/team/',
                'User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  
  if (length(content$rowSet) > 0) {
    
    # Create raw data frame
    game.list <- ContentToDF(content)
    
    # Clean data frame
    game.list <- game.list[, c(3, 2, 4)]      # Drop useless columns
    colnames(game.list) <- c('date', 'game.id', 'matchup')
    game.list$date <- as.Date(game.list$date, format = '%b %d, %Y')
    
    # Figure out team and opponent
    team <- gsub('([^ ]*).*', '\\1', game.list[1, 3])
    game.list$opponent <- gsub('.*[\\.!@] (.*)', '\\1', game.list$matchup)
    
    # Create separate home and away columns
    game.list[grep('@', game.list$matchup), 'home'] <- game.list[grep('@', game.list$matchup), 'opponent']
    game.list[grep('vs', game.list$matchup), 'home'] <- team
    game.list[grep('@', game.list$matchup), 'away'] <- team
    game.list[grep('vs', game.list$matchup), 'away'] <- game.list[grep('vs', game.list$matchup), 'opponent']
    
    game.list <- game.list[, c(1, 2, 5, 6)]
    game.list$season.type <- season.type
    return(game.list)
  }
}

#' Get Game IDs for a day
#'
#' @param date target date
#' @return data.frame
#' @keywords Game
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetGameIDsDay('2017-01-24')

GetGameIDsDay <- function(date) {
  
  options(stringsAsFactors = FALSE)
  
  request <- GET(
    "http://stats.nba.com/stats/scoreboardV2",
    query = list(
      DayOffset = 0,
      LeagueId = '00',
      gameDate = date
    ),
    add_headers('Referer' = 'http://stats.nba.com/scores/',
                'User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  
  if (length(content$rowSet) > 0) {
    
    # Create raw data frame
    game.list <- ContentToDF(content)
    
    # Clean data frame
    game.list <- game.list[, c(1, 3, 5, 7, 8, 12)]      # Drop useless columns
    colnames(game.list) <- c('date', 'game.id', 'status', 'home.team.id', 'away.team.id', 'national.tv')
    game.list$date <- date
    
    return(game.list)
  }
}