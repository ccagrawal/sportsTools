#' Player IDs on websites
#' 
#' @param year NBA season for which you want player IDs (e.g. 2008 for the 2007-08 season)
#' @param source Either 'NBA' or 'Basketball-Reference'
#' @return data frame of names and IDs
#' @keywords player IDs
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerIDs(2008)

GetPlayerIDs <- function(year = CurrentYear(), source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'Basketball-Reference') {
    
    url <- paste0("http://www.basketball-reference.com/leagues/NBA_", year, "_totals.html")
    html <- readLines(url)
    
    player.urls <- unique(html[grep('href=\"/players/[a-z]', html)])
    player.urls <- player.urls[grep('csk', player.urls)]
    player.urls <- gsub('.*href=\"/players([^."]*)\\.html">([^<]*).*', '\\1,\\2', player.urls)
    player.ids <- data.frame(matrix(unlist(strsplit(player.urls, ',')), ncol = 2, byrow=T))
    colnames(player.ids) <- c('id', 'name')
    
  } else if (source == 'NBA') {
    
    request = GET(
      "http://stats.nba.com/stats/commonallplayers",
      query = list(
        IsOnlyCurrentSeason = 1,
        LeagueID = "00",
        Season = YearToSeason(year)
      ),
      add_headers('Referer' = 'http://stats.nba.com/player/')
    )
    
    content <- content(request, 'parsed')[[3]][[1]]
    player.ids <- ContentToDF(content)
  }
  
  return(player.ids)
}