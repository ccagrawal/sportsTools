#' Player stats per season.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param type Either 'per game', 'totals', or 'advanced'
#' @return data frame with players stats
#' @keywords player
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetPlayerStats(2014, 'totals')

GetPlayerStats <- function(year, type = 'totals') {
  
  options(stringsAsFactors = FALSE)
  
  base.url <- paste0("http://www.basketball-reference.com/leagues/NBA_", year, "_TYPE.html")
  
  if (type == 'totals') {
    url <- gsub("TYPE", "totals", base.url)
  } else if (type == 'per game') {
    url <- gsub("TYPE", "per_game", base.url)
  } else if (type == 'advanced') {
    url <- gsub("TYPE", "advanced", base.url)
  }
  
  table <- readHTMLTable(url)[[1]]
  
  # Remove repeat header rows
  table <- table[-which(table$Rk == 'Rk'), ]
  
  if (type %in% c('totals', 'per game')) {
    # Remove the useless columns
    table <- table[, c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 19, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30)]
    
    # Fix the column types
    table[, -c(1, 2, 4)] <- lapply(table[, -c(1, 2, 4)], as.numeric)
    
    # Add percentages
    table$'FG%' <- table$FG / table$FGA
    table$'3P%' <- table$'3P' / table$'3PA'
    table$'FT%' <- table$FT / table$FTA
    
    # Rearrange columns
    table <- table[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 23, 10, 11, 24, 12, 13, 25, 14, 15, 16, 17, 18, 19, 20, 21, 22)]
  } else if (type %in% c('advanced')) {
    # Remove the useless columns
    table <- table[, -c(1, 20, 25)]
    
    # Fix the column types
    table[, -c(1, 2, 4)] <- lapply(table[, -c(1, 2, 4)], as.numeric)
  }
  
  # Remove astericks from player name
  table$Player <- gsub('\\*', '', table$Player)
  
  return(table)
}