#' Team Shooting Stats.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param close.def.dist Either '', '6+ Feet - Wide Open'
#' @param season.type 'Regular Season' or 'Playoffs'
#' @return data frame with wins and losses for that season
#' @keywords team shooting
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetTeamShootingStats(2014)

GetTeamShootingStats <- function(year = CurrentYear(), 
                         close.def.dist = '',
                         season.type = 'Regular Season') {
  
  options(stringsAsFactors = FALSE)
  
  request <- GET(
    "http://stats.nba.com/stats/leaguedashteamptshot",
    query = list(
      CloseDefDistRange = close.def.dist,
      Conference = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      GameScope = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = 'N',
      PerMode = 'Totals',
      Period = 0,
      PlayerExperience = "",
      PlayerPosition = "",
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      ShotClockRange = "",
      StarterBench = "",
      TeamID = 0,
      TouchTimeRange = "",
      VsConference = "",
      VsDivision = "",
      closestDef10 = ""
    )
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- c('TEAM_ID', 'TEAM_NAME', 'TEAM_ABBREVIATION')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}