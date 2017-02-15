#' Synergy stats on players or teams
#' 
#' @param year 2015 for 2014-15 season
#' @param play.type statistic to pull (e.g. 'Postup', 'Isolation', 'PRRollMan', 'PRBallHandler', 'Cut', 'OffRebound')
#' @param side either 'Offensive' or 'Defensive'
#' @param player.or.team either 'Player' or 'Team'
#' @param season.type either 'Regular Season' or 'Playoffs'
#' @return data frame of stats
#' @keywords synergy
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetSynergyStats('Postup')

GetSynergyStats <- function(year = CurrentYear(), 
                            play.type, 
                            side = 'Offensive', 
                            player.or.team = 'Player', 
                            season.type = 'Regular Season') {
  
  options(stringsAsFactors = FALSE)
  
  side <- CleanParam(side)
  player.or.team <- CleanParam(player.or.team)
  season.type <- tolower(season.type)
  
  if (season.type == 'playoffs') {
    season.type <- 'Post'
  } else {
    season.type <- 'Reg'
  }
  
  request <- GET(
    paste0('http://stats-prod.nba.com/wp-json/statscms/v1/synergy/', player.or.team, '/'),
    query = list(
      category = play.type,
      limit = 500,
      names = side,
      q = 1,
      season = (year - 1),
      seasonType = season.type
    ),
    add_headers('Referer' = paste0('http://stats.nba.com/league/', player.or.team, '/'),
                'User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[2]]
  
  if (length(content) == 0) {
    return(NA)
  }
  
  # Create raw data frame
  stats <- lapply(content, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats) <- names(content[[1]])
  
  # Clean data frame
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}