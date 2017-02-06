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
                       ids) {
  
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
                              player.ids) {
  
  # If player is name, convert it to player id
  if (grepl('[a-z]', player)) {
    if (missing(player.ids)) {
      player.ids <- GetPlayerIDs(year = year)
    }
    
    if (player %in% player.ids$DISPLAY_FIRST_LAST) {
      player <- player.ids[which(player.ids$DISPLAY_FIRST_LAST == player), 'PERSON_ID']
    } else {
      return(NULL)
    }
  }
  
  request <- GET(
    "http://stats.nba.com/stats/playergamelog?",
    query = list(
      DateFrom = '',
      DateTo = '',
      LeagueId = '00',
      PlayerId = player,
      Season = YearToSeason(year),
      SeasonType = season.type
    ),
    add_headers('Referer' = 'http://stats.nba.com/player/')
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
                            team.ids) {
  
  # If team name was provided, get team ID
  if (is.na(as.numeric(team))) {
    if (missing(team.ids)) {
      team.ids <- GetTeamIDs(year = year)
    }
    team <- team.ids[which(team.ids$name == team), 'id']
  }
  
  request <- GET(
    "http://stats.nba.com/stats/teamgamelog?",
    query = list(
      DateFrom = '',
      DateTo = '',
      LeagueId = '00',
      Season = YearToSeason(year),
      SeasonType = season.type,
      TeamID = team
    ),
    add_headers('Referer' = 'http://stats.nba.com/team/')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  game.list <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(game.list) %in% CHARACTER.COLUMNS)
  game.list[, -char.cols] <- sapply(game.list[, -char.cols], as.numeric)
  game.list$GAME_DATE <- as.Date(game.list$GAME_DATE, format = "%b %d, %Y")
  
  return(game.list)
}