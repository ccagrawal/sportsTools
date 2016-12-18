#' Player Shooting Stats.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param close.def.dist Either '', '6+ Feet - Wide Open'
#' @param season.type 'Regular Season' or 'Playoffs'
#' @return data frame with shooting stats
#' @keywords player shooting
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerShootingStats(2014)

GetPlayerShootingStats <- function(year = CurrentYear(), 
                         close.def.dist = '',
                         season.type = 'Regular Season') {
  
  options(stringsAsFactors = FALSE)
  
  request <- GET(
    "http://stats.nba.com/stats/leaguedashplayerptshot",
    query = list(
      CloseDefDistRange = close.def.dist,
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      DraftPick = "",
      DraftYear = "",
      DribbleRange = "",
      GameSegment = "",
      GeneralRange = "",
      Height = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PerMode = 'Totals',
      Period = 0,
      PlayerExperience = "",
      PlayerPosition = "",
      PlusMinus = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      ShotClockRange = "",
      ShotDistRange = "",
      StarterBench = "",
      TeamID = 0,
      TouchTimeRange = "",
      VsConference = "",
      VsDivision = "",
      Weight = ""
    )
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- c('PLAYER_ID', 'PLAYER_NAME', 'PLAYER_LAST_TEAM_ID', 'PLAYER_LAST_TEAM_ABBREVIATION')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}