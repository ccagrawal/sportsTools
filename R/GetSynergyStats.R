#' Synergy stats on players or teams
#' 
#' @param season 2015 for 2014-15 season
#' @param stat statistic to pull (e.g. 'Postup', 'Isolation', 'PRRollMan', 'PRBallHandler', 'Cut', 'OffRebound')
#' @param side either 'Offensive' or 'Defensive'
#' @param type either 'Player' or 'Team'
#' @param season.type either 'Regular Season' or 'Playoffs'
#' @return data frame of stats
#' @keywords synergy
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetSynergyStats('Postup')

GetSynergyStats <- function(season = .CurrentYear(), stat, side = 'Offensive', type = 'Player', season.type = 'Regular Season') {
  
  options(stringsAsFactors = FALSE)
  
  side <- tolower(side)
  type <- tolower(type)
  season.type <- tolower(season.type)
  
  if (season.type == 'playoffs') {
    season.type <- 'Post'
  } else {
    season.type <- 'Reg'
  }
  
  request <- GET(
    paste0('http://stats-prod.nba.com/wp-json/statscms/v1/synergy/', type, '/'),
    query = list(
      category = stat,
      limit = 500,
      name = side,
      q = 1,
      season = season,
      seasonType = season.type
    ),
    add_headers('Referer' = paste0('http://stats.nba.com/league/', type, '/'))
  )
  
  json <- content(request, 'parsed')[[2]]
  
  # Create raw data frame
  stats <- lapply(json, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats) <- names(json[[1]])
  
  # Clean data frame
  if (type == 'player') {
    colnames(stats) <- c(colnames(stats)[2:31], colnames(stats)[1], colnames(stats)[32:34])
    char.cols <- c('PlayerIDSID', 'PlayerFirstName', 'PlayerLastName', 'P', 'TeamIDSID', 'TeamName', 'TeamNameAbbreviation', 'TeamShortName', 'name', 'seasonType')
    char.cols <- which(colnames(stats) %in% char.cols)
    stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  } else {
    char.cols <- c('TeamIDSID', 'TeamName', 'TeamNameAbbreviation', 'TeamShortName', 'name', 'seasonType')
    char.cols <- which(colnames(stats) %in% char.cols)
    stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  }
  
  return(stats)
}