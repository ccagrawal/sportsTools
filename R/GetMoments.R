#' Get Moments
#'
#' @param game.id Game ID
#' @param event.id Event ID from the game
#' @return data frame of moments
#' @keywords moment
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' GetMoments('0021500100', '7')

GetMoments <- function(game.id, event.id, keep.ids = FALSE) {
  
  options(stringsAsFactors = FALSE)
  
  # Get json
  url <- gsub('<E>', event.id, 'http://stats.nba.com/stats/locations_getmoments/?eventid=<E>&gameid=<G>')
  url <- gsub('<G>', game.id, url)
  json <- fromJSON(file = url)
  
  # Create data frame with home info
  home.info <- json$home$players
  home.info <- data.frame(matrix(unlist(home.info), nrow = length(home.info), byrow = TRUE)) # Turn list to data frame
  colnames(home.info) <- c('last.name', 'first.name', 'player.id', 'jersey', 'position')
  home.info$team <- 'home'
  
  # Create data frame with away info
  away.info <- json$visitor$players
  away.info <- data.frame(matrix(unlist(away.info), nrow = length(away.info), byrow = TRUE)) # Turn list to data frame
  colnames(away.info) <- c('last.name', 'first.name', 'player.id', 'jersey', 'position')
  away.info$team <- 'away'
  
  # Combine home and away dfs
  players <- rbind(home.info, away.info)
  players$name <- paste(players$first.name, players$last.name)
  
  # Create data frame for all moments
  moments.list <- json$moments
  
  # Create blank data frame to store each moment
  moment <- data.frame(matrix(nrow = 11, ncol = 8))
  colnames(moment) <- c('num', 'game.clock', 'shot.clock', 'team', 'player', 'x', 'y', 'z')
  
  # Create large empty moments data frame and loop through moments
  moments <- data.frame()
  for (i in 1:length(moments.list)) {
    raw <- moments.list[[i]]
    temp <- moment
    
    temp$num <- i
    temp$game.clock <- raw[[3]]
    temp$shot.clock <- raw[[4]]
    
    temp[, 4:8] <- data.frame(matrix(unlist(raw[[6]]), nrow = length(raw[[6]]), byrow = TRUE))
    
    moments <- rbind(moments, temp)
  }
  
  # If user is fine with team and player IDs, just return the df
  if (keep.ids == FALSE) {
    
    for (id in unique(moments$player)[2:11]) {
      moments[which(moments$player == id), 'team'] <- players[which(players$player.id == id), 'team']
      moments[which(moments$player == id), 'player'] <- players[which(players$player.id == id), 'name']
    }
  
    moments[which(moments$player == -1), c('player', 'team')] <- 'Ball'
  }
  
  return(moments)
}