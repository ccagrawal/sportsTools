#' Player specific stats in a season.
#'
#' @param player NBA player (e.g. "James Harden")
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param stat Which stat do you want (e.g. "on-off")
#' @return data frame with player's stats
#' @keywords player
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetPlayerSpecificStats('Anthony Davis', 'on-off', 2015)

GetPlayerSpecificStats <- function(player, stat, year = as.numeric(format(Sys.Date(), "%Y")), player.ids) {
  
  options(stringsAsFactors = FALSE)
  
  if (missing(player.ids)) {
    player.ids <- GetPlayerIDs(year = year)
  }
  
  id <- player.ids[which(player.ids$name == player), 'id']
  base.url <- paste0('http://www.basketball-reference.com/players', id)
  
  if (stat == 'on-off') {
    url <- paste0(base.url, '/on-off/', year, '/')
    
    table <- readHTMLTable(url)[[1]]
    table[, -c(1, 2)] <- sapply(table[, -c(1, 2)], as.numeric)
  } else if (stat == 'per game') {
    url <- paste0(base.url, '.html')
    
    table <- readHTMLTable(url)[['per_game']]
    table[, -c(1, 3, 4, 5)] <- sapply(table[, -c(1, 3, 4, 5)], as.numeric)
    table$Season <- as.numeric(substr(table$Season, 1, 4)) + 1
  } else if (stat == 'totals') {
    url <- paste0(base.url, '.html')
    
    table <- readHTMLTable(url)[['totals']]
    table[, -c(1, 3, 4, 5)] <- sapply(table[, -c(1, 3, 4, 5)], as.numeric)
    table$Season <- as.numeric(substr(table$Season, 1, 4)) + 1
  } else if (stat == 'shooting') {
    url <- paste0(base.url, '/shooting/', year, '/')
    
    table <- readHTMLTable(url)[[1]]
    table <- table[3:nrow(table), ]
    colnames(table) <- table[1, ]
    table <- table[-which(table$Split == 'Split'), ]
    table[, 3:11] <- sapply(table[, 3:11], as.numeric)
  } else if (stat == 'play by play') {
    url <- paste0(base.url, '.html')
    
    table <- readHTMLTable(url)[['advanced_pbp']]
    table[, c(8:12)] <- sapply(table[, c(8:12)], function(x) gsub('%', '', x))
    table[, -c(1, 3, 4, 5)] <- sapply(table[, -c(1, 3, 4, 5)], as.numeric)
    table[, c(8:12)] <- table[, c(8:12)] / 100
    table$Season <- as.numeric(substr(table$Season, 1, 4)) + 1
  } else if (stat == 'advanced') {
    url <- paste0(base.url, '.html')
    
    table <- readHTMLTable(url)[['advanced']]
    table <- table[, -c(20, 25)]
    table[, -c(1, 3, 4, 5)] <- sapply(table[, -c(1, 3, 4, 5)], as.numeric)
    table$Season <- as.numeric(substr(table$Season, 1, 4)) + 1
  } else if (stat == 'playoffs totals') {
    url <- paste0(base.url, '.html')
    
    table <- readHTMLTable(url)[['totals']]
    table[, -c(1, 3, 4, 5)] <- playoffs_totals(table[, -c(1, 3, 4, 5)], as.numeric)
    table$Season <- as.numeric(substr(table$Season, 1, 4)) + 1
  }
  
  return(table)
}