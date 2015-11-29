#' Player IDs on websites
#' 
#' @param year NBA season for which you want player IDs (e.g. 2008 for the 2007-08 season)
#' @param source website that is being used (e.g. 'Basketball-Reference')
#' @return data frame of names and IDs
#' @keywords player IDs
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' GetPlayerIDs(2008)

GetPlayerIDs <- function(year = 2016, source = 'Basketball-Reference') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'Basketball-Reference') {
    
    url <- paste0("http://www.basketball-reference.com/leagues/NBA_", year, "_totals.html")
    html <- readLines(url)
    
    player.urls <- unique(html[grep('href=\"/players/[a-z]', html)])
    player.urls <- player.urls[grep('csk', player.urls)]
    player.urls <- gsub('.*href=\"/players([^."]*)\\.html">([^<]*).*', '\\1,\\2', player.urls)
    player.ids <- data.frame(matrix(unlist(strsplit(player.urls, ',')), ncol = 2, byrow=T))
    colnames(player.ids) <- c('id', 'name')
    
  } else {
    
    season <- paste0(year - 1, '-', year %% 100)    # e.g. 2015 becomes 2014-15
    url <- paste0('http://stats.nba.com/stats/commonallplayers?IsOnlyCurrentSeason=1&LeagueID=00&Season=', season)
    
    json <- fromJSON(file = url)[[3]]                  # (3) contains the actual info for the day
    player.ids <- json[[1]]$rowSet                    # (1) rowSet has the actual list of players
    
    # Create raw data frame
    player.ids <- lapply(player.ids, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
    player.ids <- data.frame(matrix(unlist(player.ids), nrow = length(player.ids), byrow = TRUE)) # Turn list to data frame
    
    # Clean data frame
    player.ids <- player.ids[, c(1, 2, 7)]           # Drop useless columns
    colnames(player.ids) <- c('id', 'name', 'team.id')
  }
  
  return(player.ids)
}