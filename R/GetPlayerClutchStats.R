#' Player clutch stats
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type Either 'Regular Season' or 'Playoffs'
#' @param measure.type Either 'Basic', 'Advanced', or 'RPM'
#' @param per.mode Either 'Per Game', 'Totals', or '100 Possessions'
#' @param position Either 'G', 'F', or 'C'
#' @return data frame with players stats
#' @keywords player
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerClutchStats(2014)

GetPlayerClutchStats <- function(year = CurrentYear(), 
                                 season.type = 'Regular Season',
                                 measure.type = 'Advanced',
                                 per.mode = 'Per Game',
                                 position = '') {
  
  options(stringsAsFactors = FALSE)
  
  if (measure.type == 'Basic') {
    measure.type <- 'Base'
  }
  
  if (per.mode == '100 Possessions') {
    per.mode <- 'Per100Possessions'
  } else if (per.mode == 'Per Game') {
    per.mode <- 'PerGame'
  }
  
  request <- GET(
    "http://stats.nba.com/stats/leaguedashplayerclutch",
    query = list(
      AheadBehind = 'Ahead or Behind',
      ClutchTime = 'Last 5 Minutes',
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      DraftPick = "",
      DraftYear = "",
      GameScope = "",
      GameSegment = "",
      Height = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      MeasureType = measure.type,
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = "N",
      PerMode = per.mode,
      Period = 0,
      PlayerExperience = "",
      PlayerPosition = position,
      PlusMinus = "N",
      PointDiff = 5,
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      ShotClockRange = "",
      StarterBench = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = "",
      Weight = ""
    ),
    add_headers('Referer' = 'http://stats.nba.com/player/')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  char.cols <- c('GROUP_SET', 'PLAYER_ID', 'PLAYER_NAME', 'TEAM_ID', 'TEAM_ABBREVIATION', 'CFPARAMS')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}