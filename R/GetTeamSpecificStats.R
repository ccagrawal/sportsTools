#' Team specific stats in a season.
#'
#' @param team Team ID (e.g. "HOU")
#' @param stat Which stat do you want (e.g. "advanced gamelog")
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @return data frame with team's stats
#' @keywords team
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetTeamSpecificStats('GSW', 2015, 'advanced gamelog')

GetTeamSpecificStats <- function(team, stat, year = as.numeric(format(Sys.Date(), "%Y"))) {
  
  options(stringsAsFactors = FALSE)

  base.url <- paste0('http://www.basketball-reference.com/teams/', id)
  
  if (stat == 'advanced gamelog') {
    url <- paste0(base.url, '/', year, '/gamelog/')
    
    table <- readHTMLTable(url)[[2]]
    table <- table[-which(table$Rk %in% c('', 'Rk')), c(2:18, 20:23, 25:28)]
    
    table[, -c(2:5)] <- sapply(table[, -(2:5)], as.numeric)
    table[, 2] <- as.Date(table[, 2], format = "%Y-%m-%d")
    
    colnames(table)[3] <- 'Home'
    table[table$Home == '@', 'Home'] <- 0
    table[table$Home == '', 'Home'] <- 1
  } else if (stat == 'basic gamelog') {
    url <- paste0(base.url, '/', year, '/gamelog/')
    
    table <- readHTMLTable(url)[[1]]
    table <- table[-which(table$Rk %in% c('', 'Rk')), c(2:24, 26:41)]
    
    table[, -c(2:5)] <- sapply(table[, -(2:5)], as.numeric)
    table[, 2] <- as.Date(table[, 2], format = "%Y-%m-%d")
    
    colnames(table)[3] <- 'Home'
    table[table$Home == '@', 'Home'] <- 0
    table[table$Home == '', 'Home'] <- 1
  }
  
  return(table)
}