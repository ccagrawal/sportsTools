#' NBA Player Tracking
#' 
#' @param stat statistic to pull (e.g. 'Passing', 'Possessions')
#' @return data frame of stats
#' @keywords synergy
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' GetTrackingStats('Postup')

GetTrackingStats <- function(stat) {
  
  options(stringsAsFactors = FALSE)
  
  url <- paste0('http://stats.nba.com/stats/leaguedashptstats?College=&',
                'Conference=&',
                'Country=&',
                'DateFrom=&',
                'DateTo=&',
                'Division=&',
                'DraftPick=&',
                'DraftYear=&',
                'GameScope=&',
                'Height=&',
                'LastNGames=0&',
                'LeagueID=00&',
                'Location=&',
                'Month=0&',
                'OpponentTeamID=0&',
                'Outcome=&',
                'PORound=0&',
                'PerMode=Totals&',
                'PlayerExperience=&',
                'PlayerOrTeam=Player&',
                'PlayerPosition=&',
                'PtMeasureType=', stat, '&',
                'Season=2015-16&',
                'SeasonSegment=&',
                'SeasonType=Regular+Season&',
                'StarterBench=&',
                'TeamID=0&',
                'VsConference=&',
                'VsDivision=&',
                'Weight=')
  
  json <- fromJSON(file = url)[[3]][[1]]
  
  stats <- json$rowSet
  
  # Create raw data frame
  stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats) <- json$headers
  
  # Clean data frame
  char.cols <- c('PLAYER_ID', 'PLAYER_NAME', 'TEAM_ID', 'TEAM_ABBREVIATION')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}