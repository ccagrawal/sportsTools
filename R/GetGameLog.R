#' Get game logs.
#'
#' @param player NBA player (e.g. "James Harden")
#' @param team NBA team (e.g. "Houston Rockets")
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type either 'Preseason', 'Regular Season', or 'Playoffs'
#' @param ids dataframe with player or team ids
#' @return data frame with player's or team's stats by game
#' @keywords player team gamelog
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetGameLog('James Harden', 2015)

GetGameLog <- function(player, team, 
                       year = CurrentYear(), 
                       season.type = 'Regular Season', 
                       ids = NA) {
  
  options(stringsAsFactors = FALSE)
  
  if (missing(team)) {
    return(.GetPlayerGameLog(player, year, season.type, ids))
  } else {
    return(.GetTeamGameLog(team, year, season.type, ids))
  }
}

.GetPlayerGameLog <- function(player, 
                              year = CurrentYear(), 
                              season.type = 'Regular Season', 
                              player.ids = NA) {
  
  request <- GET(
    "http://stats.nba.com/stats/playergamelog?",
    query = list(
      DateFrom = '',
      DateTo = '',
      LeagueId = '00',
      PlayerId = PlayerNameToID(player, year, player.ids),
      Season = YearToSeason(year),
      SeasonType = season.type
    ),
    add_headers('Referer' = 'http://stats.nba.com/player/',
                'User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  
  if (length(content$rowSet) > 0) {
    
    # Create raw data frame
    game.list <- ContentToDF(content)
    
    # Clean data frame
    game.list <- game.list[, -c(1, 10, 13, 16, 27)]      # Drop useless columns
    game.list$GAME_DATE <- as.Date(game.list$GAME_DATE, format = '%b %d, %Y')
    game.list[, 6:22] <- sapply(game.list[, 6:22], as.numeric)
    
    # Figure out team and opponent
    team <- gsub('([^ ]*).*', '\\1', game.list[1, 4])
    game.list$opponent <- gsub('.*[\\.!@] (.*)', '\\1', game.list$MATCHUP)
    
    # Create separate home and away columns
    game.list[grep('@', game.list$MATCHUP), 'home'] <- game.list[grep('@', game.list$MATCHUP), 'opponent']
    game.list[grep('vs', game.list$MATCHUP), 'home'] <- team
    game.list[grep('@', game.list$MATCHUP), 'away'] <- team
    game.list[grep('vs', game.list$MATCHUP), 'away'] <- game.list[grep('vs', game.list$MATCHUP), 'opponent']
    
    game.list$season.type <- season.type
    return(game.list)
  }
}

.GetTeamGameLog <- function(team, 
                            year = CurrentYear(), 
                            season.type = 'Regular Season', 
                            team.ids = NA) {
  
  request <- GET(
    "http://stats.nba.com/stats/teamgamelog?",
    query = list(
      DateFrom = '',
      DateTo = '',
      LeagueId = '00',
      Season = YearToSeason(year),
      SeasonType = season.type,
      TeamID = TeamNameToID(team, year, team.ids)
    ),
    add_headers('Referer' = 'http://stats.nba.com/team/',
                'User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  game.list <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(game.list) %in% CHARACTER.COLUMNS)
  game.list[, -char.cols] <- sapply(game.list[, -char.cols], as.numeric)
  game.list$GAME_DATE <- as.Date(game.list$GAME_DATE, format = "%b %d, %Y")
  
  return(game.list)
}