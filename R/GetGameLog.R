#' Get individual game stats.
#'
#' @param player NBA player (e.g. "Harden, James")
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type either 'Preseason', 'Regular Season', or 'Playoffs'
#' @param player.ids dataframe with player ids if scraping for multiple players
#' @return data frame with player's stats
#' @keywords player gamelog
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetGameLog('Harden, James', 2015)

GetGameLog <- function(player, 
                       year = as.numeric(format(Sys.Date(), "%Y")), 
                       season.type = 'Regular Season', 
                       player.ids) {
  
  options(stringsAsFactors = FALSE)
  
  # If team is name, convert it to team id
  if (grepl('[a-z]', player)) {
    if (missing(player.ids)) {
      player.ids <- GetPlayerIDs(year = year, source = 'NBA')
    }
    
    if (player %in% player.ids$name) {
      player <- player.ids[which(player.ids$name == player), 'id']
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
      Season = .YearToSeason(year),
      SeasonType = season.type
    ),
    add_headers('Referer' = 'http://stats.nba.com/player/')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  
  if (length(content$rowSet) > 0) {
    
    # Create raw data frame
    game.list <- .ContentToDF(content)
    
    # Clean data frame
    game.list <- game.list[, -c(1, 10, 13, 16, 27)]      # Drop useless columns
    game.list$GAME_DATE <- as.Date(game.list$GAME_DATE, format = '%b %d, %Y')
    game.list[, 6:22] <- sapply(game.list[, 6:22], as.numeric)
    
    # Figure out team and opponent
    team <- gsub('([^ ]*).*', '\\1', game.list[1, 3])
    game.list$opponent <- gsub('.*[\\.!@] (.*)', '\\1', game.list$matchup)
    
    # Create separate home and away columns
    game.list[grep('@', game.list$matchup), 'home'] <- game.list[grep('@', game.list$matchup), 'opponent']
    game.list[grep('vs', game.list$matchup), 'home'] <- team
    game.list[grep('@', game.list$matchup), 'away'] <- team
    game.list[grep('vs', game.list$matchup), 'away'] <- game.list[grep('vs', game.list$matchup), 'opponent']
    
    game.list$season.type <- season.type
    return(game.list)
  }
}