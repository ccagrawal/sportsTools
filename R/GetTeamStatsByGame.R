#' Team stats by game.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type Either 'regular' or 'playoffs'
#' @return data frame with stats by game
#' @keywords team stats game
#' @importFrom XML readHTMLTable htmlParse xpathSApply xmlAttrs
#' @importFrom RCurl getURL
#' @export
#' @examples
#' GetTeamStatsByGame(2014, 'regular')

GetTeamStatsByGame <- function(year, season.type = 'regular') {
  
  options(stringsAsFactors = FALSE)
  
  # Get list of teams that season
  summary.url <- 'http://www.basketball-reference.com/leagues/NBA_YYYY.html'
  summary.url <- gsub('YYYY', year, summary.url)
  
  # Get all unique team urls and get their team code (e.g. HOU)
  teams <- htmlParse(getURL(summary.url), asText=T)
  teams <- xpathSApply(teams,"//*/a[contains(@href,'/teams/')]", xmlAttrs)[-1]
  teams <- unique(as.character(gsub('/teams/([^/]*)/.*', '\\1', teams)))
  
  # Create empty data frame to hold game stats
  game.stats <- data.frame()
  
  # Create base url for team game logs
  base.url <- 'http://www.basketball-reference.com/teams/TTT/YYYY/gamelog/'
  
  # Loop through each team to collect game stats
  for (team in teams) {
    
    # Create team-specific and year-specific url and get tables
    url <- gsub('YYYY', year, base.url)
    url <- gsub('TTT', team, url)
    tables <- readHTMLTable(url)
    
    if (season.type == 'playoffs') {
      if ("tgl_basic_playoffs" %in% names(tables)) {
        temp <- tables[['tgl_basic_playoffs']]
        temp$Team <- team
        game.stats <- rbind(game.stats, temp)
      }
    } else {
      temp <- tables[['tgl_basic']]
      temp$Team <- team
      game.stats <- rbind(game.stats, temp)
    }
  }
  
  # Remove repeat header rows
  if (sum(is.na(game.stats$Opp)) > 0) {
    game.stats <- game.stats[-which(is.na(game.stats$Opp)), ]
  }
  
  if (sum(game.stats$Rk == 'Rk') > 0) {
    game.stats <- game.stats[-which(game.stats$Rk == 'Rk'), ]
  }
  
  # Remove useless columns
  game.stats <- game.stats[, -c(1, 2, 25)]
  
  # Fix home marker column
  colnames(game.stats)[2] <- 'home'
  
  # Rearrange data frame into Home and Away stats
  game.stats[, c('Home.Team', 'Home.Score', 'Home.FG', 'Home.FGA', 'Home.FG%', 'Home.3P', 
                 'Home.3PA', 'Home.3P%', 'Home.FT', 'Home.FTA', 'Home.FT%', 'Home.ORB', 
                 'Home.TRB', 'Home.AST', 'Home.STL', 'Home.BLK', 'Home.TOV', 'Home.PF',
                 'Away.Team', 'Away.Score', 'Away.FG', 'Away.FGA', 'Away.FG%', 'Away.3P', 
                 'Away.3PA', 'Away.3P%', 'Away.FT', 'Away.FTA', 'Away.FT%', 'Away.ORB', 
                 'Away.TRB', 'Away.AST', 'Away.STL', 'Away.BLK', 'Away.TOV', 'Away.PF')] <- 0
  
  game.stats[which(game.stats$home == ''), 
             c('Home.Team', 'Home.Score', 'Home.FG', 'Home.FGA', 'Home.FG%', 'Home.3P', 
               'Home.3PA', 'Home.3P%', 'Home.FT', 'Home.FTA', 'Home.FT%', 'Home.ORB', 
               'Home.TRB', 'Home.AST', 'Home.STL', 'Home.BLK', 'Home.TOV', 'Home.PF')] <- 
    game.stats[which(game.stats$home == ''), 
               c(39, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)]
  
  game.stats[which(game.stats$home == ''), 
             c('Away.Team', 'Away.Score', 'Away.FG', 'Away.FGA', 'Away.FG%', 'Away.3P', 
               'Away.3PA', 'Away.3P%', 'Away.FT', 'Away.FTA', 'Away.FT%', 'Away.ORB', 
               'Away.TRB', 'Away.AST', 'Away.STL', 'Away.BLK', 'Away.TOV', 'Away.PF')] <- 
    game.stats[which(game.stats$home == ''), 
               c(3, 6, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38)]
  
  game.stats[which(game.stats$home == '@'), 
             c('Home.Team', 'Home.Score', 'Home.FG', 'Home.FGA', 'Home.FG%', 'Home.3P', 
               'Home.3PA', 'Home.3P%', 'Home.FT', 'Home.FTA', 'Home.FT%', 'Home.ORB', 
               'Home.TRB', 'Home.AST', 'Home.STL', 'Home.BLK', 'Home.TOV', 'Home.PF')] <- 
    game.stats[which(game.stats$home == '@'), 
               c(3, 6, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38)]
  
  game.stats[which(game.stats$home == '@'), 
             c('Away.Team', 'Away.Score', 'Away.FG', 'Away.FGA', 'Away.FG%', 'Away.3P', 
               'Away.3PA', 'Away.3P%', 'Away.FT', 'Away.FTA', 'Away.FT%', 'Away.ORB', 
               'Away.TRB', 'Away.AST', 'Away.STL', 'Away.BLK', 'Away.TOV', 'Away.PF')] <- 
    game.stats[which(game.stats$home == '@'), 
               c(39, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)]
  
  # Remove the useless columns and duplicates
  game.stats <- game.stats[, -c(2:39)]
  game.stats <- game.stats[!duplicated(game.stats), ]
  
  # Fix the column types
  game.stats[, -c(1, 2, 20)] <- lapply(game.stats[, -c(1, 2, 20)], as.numeric)
  
  return(game.stats)
}