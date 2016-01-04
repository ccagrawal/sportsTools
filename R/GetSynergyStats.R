#' Synergy stats on players or teams
#' 
#' @param stat statistic to pull (e.g. 'Postup', 'Isolation', 'PRRollMan')
#' @param side either 'offensive' or 'defensive'
#' @param type either 'player' or 'team'
#' @return data frame of stats
#' @keywords synergy
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' GetSynergyStats('Postup')

GetSynergyStats <- function(stat, side = 'offensive', type = 'player') {
  
  options(stringsAsFactors = FALSE)
  
  stat.key <- paste0(type, '_', stat)
  url <- paste0('http://stats.nba.com/js/data/playtype/', stat.key, '.js')
  
  json <- fromJSON(file = url)[[3]]
  
  if (side == 'offensive') {
    json <- json[[1]]
  } else {
    json <- json[[2]]
  }
  
  stats <- json$rowSet
  
  # Create raw data frame
  stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats) <- json$headers
  
  # Clean data frame
  if (stat.key %in% c('player_Postup', 'player_PRRollMan')) {
    char.cols <- c('PlayerFirstName', 'PlayerLastName', 'P', 'TeamName', 'TeamNameAbbreviation', 'TeamShortName')
    char.cols <- which(colnames(stats) %in% char.cols)
    stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  }
  
  return(stats)
}