#' On-Off stats for each player on a team
#' 
#' @param id team's ID
#' @param year 2015 for 2014-15 season
#' @param season.type Either 'Regular Season' or 'Playoffs'
#' @param measure.type Either 'Base', 'Advanced', 'Misc', 'Four Factors', 'Scoring', 'Opponent', 'Usage', or 'Defense'
#' @param per.mode Either 'Totals', 'PerGame', 'MinutesPer', 'Per48', 'Per40', 'PerMinute', 'PerPossession', 'PerPlay', 'Per100Possessions', or 'Per100Plays'
#' @return data frame of stats
#' @keywords onoff team
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetOnOffStats(id = '1610612745', measure.type = 'Advanced')

GetOnOffStats <- function(id, year = .CurrentYear(), season.type = 'Regular Season', measure.type = 'Base', per.mode = 'Totals') {
  
  options(stringsAsFactors = FALSE)
  
  request = GET(
    "http://stats.nba.com/stats/teamplayeronoffdetails",
    query = list(
      DateFrom = "",
      DateTo = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      MeasureType = measure.type,
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PaceAdjust = "N",
      PerMode = "Totals",
      Period = 0,
      PlusMinus = "N",
      Rank = "N",
      Season = .YearToSeason(year),
      SeasonSegment = "",
      SeasonType = "Regular Season",
      TeamID = id,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers('Referer' = 'http://stats.nba.com/team/')
  )
  
  content <- content(request, 'parsed')[[3]]
  content.on <- content[[2]]
  content.off <- content[[3]]
    
  stats.on <- content.on$rowSet
  stats.off <- content.off$rowSet
  
  # Create raw data frame
  stats.on <- lapply(stats.on, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats.on <- data.frame(matrix(unlist(stats.on), nrow = length(stats.on), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats.on) <- content.on$headers
  
  # Create raw data frame
  stats.off <- lapply(stats.off, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats.off <- data.frame(matrix(unlist(stats.off), nrow = length(stats.off), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats.off) <- content.off$headers
  
  # Merge on and off court stats
  stats <- rbind(stats.on, stats.off)
  
  char.cols <- c('GROUP_SET', 'TEAM_ID', 'TEAM_ABBREVIATION', 'TEAM_NAME', 'VS_PLAYER_ID', 'VS_PLAYER_NAME', 'COURT_STATUS')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}