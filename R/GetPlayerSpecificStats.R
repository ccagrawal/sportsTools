#' Player's stat in a season.
#'
#' @param player NBA player (e.g. "James Harden")
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param stat Which stat do you want (e.g. "on-off")
#' @return data frame with player's stats
#' @keywords player
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetPlayerSpecificStats('Anthony Davis', 2015, 'on-off)

GetPlayerSpecificStats <- function(player, year, stat) {
  
  options(stringsAsFactors = FALSE)
  
  player.ids <- GetPlayerIDs(year = year)
  id <- player.ids[which(player.ids$name == player), 'id']
  
  base.url <- paste0("http://www.basketball-reference.com/players", id, "/")
  
  # On-off stat ratings
  if (stat == 'on-off') {
    url <- paste0(base.url, 'on-off/', year, '/')
    
    table <- readHTMLTable(url)[[1]]
    table[, -c(1, 2)] <- sapply(table[, -c(1, 2)], as.numeric)
    
    return(table)
  }
}