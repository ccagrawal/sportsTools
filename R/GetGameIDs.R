#' Get Game IDs.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param type either "regular season" or "playoffs"
#' @return vector of Gamd IDs
#' @keywords gameid
#' @export
#' @examples
#' GetGameIDs(2014, 'regular season')

GetGameIDs <- function(year, season.type = 'regular') {
  
  options(stringsAsFactors = FALSE)
  
  url <- paste("http://www.basketball-reference.com/leagues/NBA_", year, "_games.html", sep = "")
  html <- readLines(url)
  
  if (season.type == 'regular') {
    html <- html[grep('Regular Season</h2>', html):length(html)]   # Chop HTML to include from regular season on
    if (length(grep('Playoffs</h2>', html)) == 1) {    # Check if playoffs occured that season
      html <- html[1:grep('Playoffs</h2>', html)]      # Chop HTML till playoffs
    }
    
    ids <- html[grep('/boxscores/.*html', html)]
    ids <- gsub('.*boxscores/([^\\.]*).*', '\\1', ids)
  } else {
    if (length(grep('Playoffs</h2>', html)) == 0) {    # Check if playoffs occured that season
      return(NULL)
    }
    
    html <- html[grep('Playoffs</h2>', html):length(html)]   # Chop HTML to include from regular season on
    ids <- html[grep('/boxscores/.*html', html)]
    ids <- gsub('.*boxscores/([^\\.]*).*', '\\1', ids)
  }
  
  return(ids)
}