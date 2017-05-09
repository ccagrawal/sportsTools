#' Get game logs.
#'
#' @param player NBA player (e.g. "James Harden")
#' @param team NBA team (e.g. "Houston Rockets")
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param measure.type 'Basic', or 'Advanced'
#' @param season.type either 'Preseason', 'Regular Season', or 'Playoffs'
#' @param ids dataframe with player or team ids
#' @param source 'NBA' or 'Basketball-Reference'
#' @return data frame with player's or team's stats by game
#' @keywords player team gamelog
#' @importFrom httr GET content add_headers
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetGameLog('James Harden', 2015)

GetGameLog <- function(player, team, 
                       year = CurrentYear(), 
                       measure.type = 'Basic',
                       season.type = 'Regular Season', 
                       ids = NA, source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'NBA') {
    if (missing(team)) {
      return(.GetPlayerGameLogNBA(player, year, season.type, ids))
    } else {
      return(.GetTeamGameLogNBA(team, year, season.type, ids))
    }
  } else if (source == 'Basketball-Reference') {
    if (missing(team)) {
      return(.GetPlayerGameLogBRef(player, year, season.type, ids))
    } else {
      return(.GetTeamGameLogBRef(team, year, measure.type, season.type, ids))
    }
  }
}

.GetPlayerGameLogNBA <- function(player, 
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

.GetTeamGameLogNBA <- function(team, 
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

.GetTeamGameLogBRef <- function(team, 
                                year = CurrentYear(), 
                                measure.type = 'Advanced',
                                season.type = 'Regular Season', 
                                team.ids = NA) {
  
  url <- 'http://www.basketball-reference.com/teams/TEAM/YEAR/gamelog-advanced/'
  url <- gsub('TEAM', team, url)
  url <- gsub('YEAR', year, url)
  
  request <- GET(url, add_headers('User-Agent' = 'Mozilla/5.0'))
  game.list <- readHTMLTable(rawToChar(request$content))[[1]]
  
  # Clean data frame
  game.list$Date <- as.Date(game.list$Date)
  colnames(game.list)[4] <- 'Home'
  
  char.cols <- which(colnames(game.list) %in% CHARACTER.COLUMNS.BREF)
  game.list[, -char.cols] <- sapply(game.list[, -char.cols], as.numeric)
  
  game.list <- game.list[, colSums(is.na(game.list)) < nrow(game.list)]
  colnames(game.list)[c(7, 8, 19, 20, 21, 22, 23, 24, 25, 26)] <- 
    c('Tm.Score', 'Opp.Score', 
      'Tm.eFG', 'Tm.TOV', 'Tm.ORB', 'Tm.FT.FGA',
      'Opp.eFG', 'Opp.TOV', 'Opp.ORB', 'Opp.FT.FGA')
  
  game.list <- game.list[!is.na(game.list$Rk), ]
  return(game.list)
}