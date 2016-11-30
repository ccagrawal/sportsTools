#' NBA Player Tracking
#' 
#' @param year 2015 for 2014-15 season
#' @param season.type Either 'Regular Season' or 'Playoffs'
#' @param per.mode Either 'Per Game' or 'Totals'
#' @param measure.type Either 'SpeedDistance', 'Rebounding', 'Possessions', 'CatchShoot', 'PullUpShot', 
#'        'Defense', 'Drives', 'Passing', 'ElbowTouch', 'PostTouch', 'PaintTouch', or 'Efficiency'
#' @param player.or.team Either 'Player' or 'Team'
#' @param position Either 'G', 'F', 'C', 'G-F', 'F-G', 'F-C', or 'C-F'
#' @return data frame of stats
#' @keywords synergy
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetTrackingStats(stat = 'Postup')

GetTrackingStats <- function(year = CurrentYear(), 
                             season.type = 'Regular Season', 
                             per.mode = 'Per Game', 
                             measure.type, 
                             player.or.team = 'Player', 
                             position = '') {
  
  options(stringsAsFactors = FALSE)
  
  if (per.mode == 'Per Game') {
    per.mode <- 'PerGame'
  }
  
  request <- GET(
    "http://stats.nba.com/stats/leaguedashptstats",
    query = list(
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      DraftPick = "",
      DraftYear = "",
      GameScope = "",
      Height = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PerMode = per.mode,
      PlayerExperience = "",
      PlayerOrTeam = player.or.team,
      PlayerPosition = position,
      PtMeasureType = measure.type,
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      StarterBench = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = "",
      Weight = ""
    ),
    add_headers('Referer' = 'http://stats.nba.com/tracking/')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  if (player.or.team == 'Player') {
    char.cols <- c('PLAYER_ID', 'PLAYER_NAME', 'TEAM_ID', 'TEAM_ABBREVIATION')
    char.cols <- which(colnames(stats) %in% char.cols)
    stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  } else {
    char.cols <- c('TEAM_ID', 'TEAM_ABBREVIATION', 'TEAM_NAME')
    char.cols <- which(colnames(stats) %in% char.cols)
    stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  }
  
  return(stats)
}