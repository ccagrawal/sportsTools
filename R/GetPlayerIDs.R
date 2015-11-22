#' Player IDs on websites
#' 
#' @param year NBA season for which you want player IDs (e.g. 2008 for the 2007-08 season)
#' @param provider website that is being used (e.g. 'Basketball-Reference')
#' @return data frame of names and IDs
#' @keywords ID
#' @export
#' @examples
#' GetPlayerIDs(2008)

GetPlayerIDs <- function(year = 2016, provider = 'Basketball-Reference') {
  
  options(stringsAsFactors = FALSE)
  
  url <- paste0("http://www.basketball-reference.com/leagues/NBA_", year, "_totals.html")
  html <- readLines(url)
  
  player.urls <- unique(html[grep('href=\"/players/[a-z]', html)])
  player.urls <- player.urls[grep('csk', player.urls)]
  player.urls <- gsub('.*href=\"/players([^."]*)\\.html">([^<]*).*', '\\1,\\2', player.urls)
  player.ids <- data.frame(matrix(unlist(strsplit(player.urls, ',')), ncol = 2, byrow=T))
  colnames(player.ids) <- c('id', 'name')
  
  return(player.ids)
}