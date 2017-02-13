#' Player IDs on websites
#' 
#' @param year NBA season for which you want player IDs (e.g. 2008 for the 2007-08 season)
#' @return data frame of names and IDs
#' @keywords player IDs
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerIDs(2008)

GetPlayerIDs <- function(year = CurrentYear()) {
  
  options(stringsAsFactors = FALSE)
  
  request = GET(
    "http://stats.nba.com/stats/commonallplayers",
    query = list(
      IsOnlyCurrentSeason = 0,
      LeagueID = "00",
      Season = YearToSeason(year)
    ),
    add_headers('Referer' = 'http://stats.nba.com/player/',
                'User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  player.ids <- ContentToDF(content)
  
  return(player.ids)
}