#' Player stats by team
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type Either 'Regular Season' or 'Playoffs'
#' @param measure.type Either 'Base', 'Advanced', 'Misc', 'Four Factors', 'Scoring', 'Opponent', 'Usage', or 'Defense'
#' @param per.mode Either 'Totals', 'PerGame', 'MinutesPer', 'Per48', 'Per40', 'PerMinute', 'PerPossession', 'PerPlay', 'Per100Possessions', or 'Per100Plays'
#' @param source Currently only 'NBA'
#' @return data frame with players stats
#' @keywords player
#' @importFrom XML readHTMLTable
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerStatsByTeam(year = 2016, team.id = '1610612745')

GetPlayerStatsByTeam <- function(year = .CurrentYear(), 
                                 team.id,
                                 season.type = 'Regular Season', 
                                 measure.type = 'Base', 
                                 per.mode = 'Per Game',
                                 source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'NBA') {
    return(.GetPlayerStatsByTeamNBA(year, season.type, measure.type, per.mode))
  }
}

.GetPlayerStatsByTeamNBA <- function(year, team.id, season.type, measure.type, per.mode) {
  
  if (measure.type == 'Basic') {
    measure.type <- 'Base'
  }
  
  if (per.mode == '100 Possessions') {
    per.mode <- 'Per100Possessions'
  }
  
  request <- GET(
    "http://stats.nba.com/stats/teamplayerdashboard",
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
      PerMode = per.mode,
      Period = 0,
      PlusMinus = "N",
      Rank = "N",
      Season = .YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      TeamID = team.id,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers('Referer' = 'http://stats.nba.com/team/')
  )
  
  content <- content(request, 'parsed')[[3]][[2]]
  stats <- content$rowSet
  
  # Create raw data frame
  stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats) <- content$headers
  
  char.cols <- c('GROUP_SET', 'PLAYER_ID', 'PLAYER_NAME')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}