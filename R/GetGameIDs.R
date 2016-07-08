#' Get Game IDs.
#'
#' @param year Season (e.g. 2008 for the 2007-08 season)
#' @param source Either 'Basketball-Reference' or 'NBA'
#' @param season.type Either "Regular Season", "Playoffs", or "Both"
#' @param method Either "Team" or "Date"
#' @return vector if from 'Basketball-Reference' or data frame if from 'NBA'
#' @keywords gameid
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetGameIDs(2014, 'regular season')

GetGameIDs <- function(year = .CurrentYear(), 
                       source = 'NBA', 
                       season.type = 'Regular Season',
                       method = 'Team') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'Basketball-Reference') {
    ids <- .GetBRefGameIDs(year, season.type)
  } else if (source == 'NBA') {
    ids <- .GetNBAGameIDs(year, season.type, method)
  } else {
    return(NULL)
  }
  
  return(ids)
}

# Input:    year season (e.g. 2008 for the 2007-08 season)
#           season.type Either 'Regular Season', 'Playoffs', or 'Both'
# Output:   Vector with game IDs for that season from Basketball-Reference
.GetBRefGameIDs <- function(year, season.type) {
  
  url <- paste0('http://www.basketball-reference.com/leagues/NBA_', year, '_games.html')
  html <- readLines(url)
  
  if (season.type == 'Regular Season') {
    
    html <- html[grep('Regular Season</h2>', html):length(html)]   # Chop HTML to include from regular season on
    if (length(grep('Playoffs</h2>', html)) == 1) {    # Check if playoffs occured that season
      html <- html[1:grep('Playoffs</h2>', html)]      # Chop HTML till playoffs
    }
    
    ids <- html[grep('/boxscores/.*html', html)]
    ids <- gsub('.*boxscores/([^\\.]*).*', '\\1', ids)
    
  } else if (season.type == 'Playoffs') {
    
    if (length(grep('Playoffs</h2>', html)) == 0) {    # Check if playoffs occured that season
      return(NULL)
    }
    
    html <- html[grep('Playoffs</h2>', html):length(html)]   # Chop HTML to include from playoffs on
    ids <- html[grep('/boxscores/.*html', html)]
    ids <- gsub('.*boxscores/([^\\.]*).*', '\\1', ids)
    
  } else {
    
    html <- html[grep('Regular Season</h2>', html):length(html)]   # Chop HTML to include from regular season on
    ids <- html[grep('/boxscores/.*html', html)]
    ids <- gsub('.*boxscores/([^\\.]*).*', '\\1', ids)
    
  }
  
  return(ids)
}

# Input:    Team (ex. '1610612745' or 'Houston')
#           year (ex. 2008 for 2007-08 season)
#           season.type either 'Regular Season', 'Playoffs', or 'Both'
# Output:   Data frame with info for games that day from stats.nba.com
#           game.id, date, home, away, season.type
.GetNBAGameIDsTeam <- function(team, year, season.type) {
  
  if (season.type == 'Both') {
    ids.regular <- .GetNBAGameIDsTeam(team, year, 'Regular Season')
    ids.playoffs <- .GetNBAGameIDsTeam(team, year, 'Playoffs')
    return(rbind(ids.regular, ids.playoffs))
  }
  
  # If team is name, convert it to team id
  if (grepl('[a-z]', team)) {
    team.ids <- GetTeamIDs(year, source = 'NBA')
    if (team %in% team.ids$name) {
      team <- team.ids[which(team.ids$name == team), 'id']
    } else {
      return(NULL)
    }
  }
  
  request <- GET(
    "http://stats.nba.com/stats/teamgamelog",
    query = list(
      LeagueId = '00',
      Season = .YearToSeason(year),
      SeasonType = season.type,
      TeamID = team
    ),
    add_headers('Referer' = 'http://stats.nba.com/team/')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  
  if (length(content$rowSet) > 0) {
    
    # Create raw data frame
    game.list <- .ContentToDF(content)
    
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

# Input:    Date (ex. '2013-11-23')
# Output:   Data frame with info for games that day from stats.nba.com
#           date, game.id, status, home.team.id, away.team.id, national.tv
.GetNBAGameIDsDay <- function(date) {
  
  request <- GET(
    "http://stats.nba.com/stats/scoreboardV2",
    query = list(
      DayOffset = 0,
      LeagueId = '00',
      gameDate = date
    ),
    add_headers('Referer' = 'http://stats.nba.com/scores/')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  
  if (length(content$rowSet) > 0) {
    
    # Create raw data frame
    game.list <- .ContentToDF(content)
    
    # Clean data frame
    game.list <- game.list[, c(1, 3, 5, 7, 8, 12)]      # Drop useless columns
    colnames(game.list) <- c('date', 'game.id', 'status', 'home.team.id', 'away.team.id', 'national.tv')
    game.list$date <- date
    
    return(game.list)
  }
}

# Input:    year (e.g. 2008 for the 2007-08 season)
#           season.type ('Regular Season', 'Playoffs', or 'Both')
# Output:   Data frame with info for games that day from stats.nba.com
#           date, game.id, status, home.team.id, away.team.id, national.tv if method = 'Date'
#           date, game.id, home, away if method = 'Team'
.GetNBAGameIDs <- function(year, season.type, method) {
  
  if (method == 'Date') {
    schedule <- GetSchedule(sport = 'NBA', year = year, season.type = season.type)
    start.date <- min(schedule$date)
    end.date <- max(schedule$date)
    
    all.games <- data.frame()                            # Create empty data frame
    
    # Iterate through days and add games to all.games data frame 
    for (date in seq(from = as.Date(start.date), to = as.Date(end.date), by = 1)) {
      date <- as.Date(date, origin = '1970-01-01')      # Convert epoch (days) to date object
      temp <- .GetNBAGameIDsDay(date)
      all.games <- rbind(all.games, temp)
    }
    
  } else {
    all.games <- data.frame()
    
    teams <- GetTeamIDs(year = year)
    for (id in teams$id) {
      temp <- .GetNBAGameIDsTeam(team = id, year, season.type)
      all.games <- rbind(all.games, temp)
    }
    
    all.games <- unique(all.games)
  }
  
  return(all.games)
}