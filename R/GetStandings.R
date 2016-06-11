#' Standings.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @return data frame with wins and losses for that season
#' @keywords schedule
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetStandings(2014)

GetStandings <- function(year) {
  
  options(stringsAsFactors = FALSE)
  url <- 'http://www.basketball-reference.com/leagues/NBA_YYYY_standings.html'
  url <- gsub('YYYY', year, url)
  tables <- readHTMLTable(url)
  
  # Get table and remove extra columns
  standings <- tables[['expanded-standings']]
  standings <- standings[, c('Team', 'Overall', 'Home', 'Road')]
  
  # Calculate wins and losses
  win.loss <- t(sapply(standings$Overall, function(x) strsplit(x, '-')[[1]]))
  win.loss <- data.frame(win.loss)
  win.loss <- sapply(win.loss, as.numeric)
  standings$Wins <- win.loss[, 1]
  standings$Losses <- win.loss[, 2]
  
  # Calculate home wins and losses
  win.loss <- t(sapply(standings$Home, function(x) strsplit(x, '-')[[1]]))
  win.loss <- data.frame(win.loss)
  win.loss <- sapply(win.loss, as.numeric)
  standings$Home.Wins <- win.loss[, 1]
  standings$Home.Losses <- win.loss[, 2]
  
  # Calculate away wins and losses
  win.loss <- t(sapply(standings$Road, function(x) strsplit(x, '-')[[1]]))
  win.loss <- data.frame(win.loss)
  win.loss <- sapply(win.loss, as.numeric)
  standings$Away.Wins <- win.loss[, 1]
  standings$Away.Losses <- win.loss[, 2]
  
  standings <- standings[, -c(2, 3, 4)]
  return(standings)
}