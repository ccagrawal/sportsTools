#' Team Stats.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param stat.type 'regular' or 'advanced'
#' @param season.type 'regular' or 'playoffs'
#' @return data frame with wins and losses for that season
#' @keywords team
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetTeamStats(2014)

GetTeamStats <- function(year, stat.type = 'regular', season.type = 'regular') {
  
  options(stringsAsFactors = FALSE)
  
  if (season.type == 'playoffs') {
    url <- 'http://www.basketball-reference.com/playoffs/NBA_YYYY.html'
  } else {
    url <- 'http://www.basketball-reference.com/leagues/NBA_YYYY.html'
  }
  
  url <- gsub('YYYY', year, url)
  tables <- readHTMLTable(url)
  
  # Get table, convert appropriate columns to numeric, remove asterisks from team names
  if (stat.type == 'advanced') {
    stats <- tables[['misc']]
    stats[, -c(2, 23, 24)] <- lapply(stats[, -c(2, 23, 24)], as.numeric)
    stats$Attendance <- as.numeric(gsub(',', '', stats$Attendance))
    stats$Team <- gsub('\\*', '', stats$Team)
  } else {
    stats <- tables[['team']]
    stats[, -2] <- lapply(stats[, -2], as.numeric)
    stats$Team <- gsub('\\*', '', stats$Team)
  }
  
  stats <- stats[-which(is.na(stats$Rk)), -1]   # Remove league average row and rank column
  return(stats)
}