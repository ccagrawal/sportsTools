#' Get Game IDs.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param source either 'Basketball-Reference' or 'NBA'
#' @param type either "regular season" or "playoffs"
#' @return vector if from 'Basketball-Reference' or data frame if from 'NBA'
#' @keywords gameid
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' GetGameIDs(2014, 'regular season')

GetGameIDs <- function(year, source = 'Basketball-Reference', season.type = 'regular') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'Basketball-Reference') {
    ids <- .GetBRefGameIDs(year, season.type)
  } else if (source == 'NBA') {
    ids <- .GetNBAGameIDs(year, season.type, method = 'team')
  } else {
    return(NULL)
  }
  
  return(ids)
}

# Input:    year (e.g. 2008 for the 2007-08 season)
#           season.type ('regular', 'playoffs', or 'both')
# Output:   Vector with game IDs for that season from Basketball-Reference
.GetBRefGameIDs <- function(year, season.type = 'regular') {
  
  url <- paste0('http://www.basketball-reference.com/leagues/NBA_', year, '_games.html')
  html <- readLines(url)
  
  if (season.type == 'regular') {
    
    html <- html[grep('Regular Season</h2>', html):length(html)]   # Chop HTML to include from regular season on
    if (length(grep('Playoffs</h2>', html)) == 1) {    # Check if playoffs occured that season
      html <- html[1:grep('Playoffs</h2>', html)]      # Chop HTML till playoffs
    }
    
    ids <- html[grep('/boxscores/.*html', html)]
    ids <- gsub('.*boxscores/([^\\.]*).*', '\\1', ids)
    
  } else if (season.type == 'playoffs') {
    
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
#           season.type either 'regular', 'playoffs', or 'both'
# Output:   Data frame with info for games that day from stats.nba.com
#           game.id, date, team
.GetNBAGameIDsTeam <- function(team, year, season.type = 'regular') {
  
  # If team is name, convert it to team id
  if (grepl('[a-z]', team)) {
    team.ids <- GetTeamIDs(year, source = 'NBA')
    if (team %in% team.ids$name) {
      team <- team.ids[which(team.ids$name == team), 'id']
    } else {
      return(NULL)
    }
  }
  
  # Create "season" identifier from year
  season <- paste0(year - 1, '-', year %% 100)
  
  # Start with base url
  base.url <- 'http://stats.nba.com/stats/teamgamelog?LeagueID=00&Season=YYYY&SeasonType=TTTT&TeamID=IIII'
  base.url <- gsub('IIII', team, base.url)
  base.url <- gsub('YYYY', season, base.url)
  
  # Create NBA URL based on season type
  if ((season.type == 'regular') | (season.type == 'both')) {
    url <- gsub('TTTT', 'Regular+Season', base.url)
  } else {
    url <- gsub('TTTT', 'Playoffs', base.url)
  }
  
  json <- fromJSON(file = url)[[3]]                  # (3) contains the actual info for the day
  
  # Check if games exist for the given day
  temp <- json[[1]]                                  # (1) contains game list info
  game.list <- temp[[3]]                             # (3) contains the actual rows
  
  # Also grab playoff games if both were requested
  if (season.type == 'both') {
    url <- gsub('TTTT', 'Playoffs', base.url)
    json <- fromJSON(file = url)[[3]]                  # (3) contains the actual info for the day
    temp <- json[[1]]                                  # (1) contains game list info
    game.list <- c(game.list, temp[[3]])               # (3) contains the actual rows
  }
  
  if (length(game.list) > 0) {
    # Create raw data frame
    game.list <- lapply(game.list, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
    game.list <- data.frame(matrix(unlist(game.list), nrow = length(game.list), byrow = TRUE)) # Turn list to data frame
    
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
    return(game.list)
  }
}

# Input:    Date (ex. '2013-11-23')
# Output:   Data frame with info for games that day from stats.nba.com
#           date, game.id, status, home.team.id, away.team.id, national.tv
.GetNBAGameIDsDay <- function(date) {
  
  # Create NBA URL and scrape the JSON for the input day
  url.date <- format(as.Date(date), format = '%m%%2F%d%%2F%Y')  # NBA.com uses a weird date format: "mm%2Fdd%2FYYYY"
  url <- gsub('DATE', url.date, 'http://stats.nba.com/stats/scoreboard/?LeagueID=00&gameDate=DATE&DayOffset=0')
  json <- fromJSON(file = url)[[3]]                  # (3) contains the actual info for the day
  
  # Check if games exist for the given day
  temp <- json[[1]]                                   # (1) contains game list info
  game.list <- temp[[3]]                               # (3) contains the actual rows
  
  if (length(game.list) > 0) {
    
    # Create raw data frame
    game.list <- lapply(game.list, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
    game.list <- data.frame(matrix(unlist(game.list), nrow = length(game.list), byrow = TRUE)) # Turn list to data frame
    
    # Clean data frame
    game.list <- game.list[, c(1, 3, 5, 7, 8, 12)]      # Drop useless columns
    colnames(game.list) <- c('date', 'game.id', 'status', 'home.team.id', 'away.team.id', 'national.tv')
    game.list$date <- date
    
    # Convert factors to characters (don't use numeric on game.id because it chops leading 0s!)
    game.list[, 'home.team.id'] <- as.numeric(game.list[, 'home.team.id'])
    game.list[, 'away.team.id'] <- as.numeric(game.list[, 'away.team.id'])
    
    return(game.list)
  }
}

# Input:    Start Date, End Date (ex. '2013-11-23')
# Output:   Data frame with info for games that day from stats.nba.com
#           date, game.id, status, home.team.id, away.team.id, national.tv
.GetNBAGameIDsRange <- function(start.date, end.date) {
  
  all.games <- data.frame()                            # Create empty data frame
  
  # Iterate through days and add games to all.games data frame 
  for (date in seq(from = as.Date(start.date), to = as.Date(end.date), by = 1)) {
    date <- as.Date(date, origin = '1970-01-01')      # Convert epoch (days) to date object
    temp <- .GetNBAGameIDsDay(date)
    all.games <- rbind(all.games, temp)
  }
  
  return(all.games)
}

# Input:    year (e.g. 2008 for the 2007-08 season)
#           season.type ('regular', 'playoffs', or 'both')
# Output:   Data frame with info for games that day from stats.nba.com
#           date, game.id, status, home.team.id, away.team.id, national.tv if method = 'date'
#           date, game.id, home, away if method = 'team'
.GetNBAGameIDs <- function(year, season.type = 'regular', method = 'team') {
  
  if (method == 'date') {
    schedule <- GetSchedule(year, season.type)
    start.date <- min(schedule$date)
    end.date <- max(schedule$date)
    
    all.games <- .GetNBAGameIDsRange(start.date, end.date)
  } else {
    all.games <- data.frame()
    
    teams <- GetTeamIDs(year)$id
    for (id in teams) {
      temp <- .GetNBAGameIDsTeam(team = id, year, season.type)
      all.games <- rbind(all.games, temp)
    }
    
    all.games <- unique(all.games)
  }
  
  return(all.games)
}