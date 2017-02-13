#' Team Shooting Stats.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param close.def.dist Either '', '6+ Feet - Wide Open'
#' @param distance.range If provided, either 'By Zone', '5ft Range', or '8ft Range'
#' @param season.type 'Regular Season' or 'Playoffs'
#' @param per.mode 'PerGame' or 'Totals'
#' @return data frame with wins and losses for that season
#' @keywords team shooting
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetTeamShootingStats(2014)

GetTeamShootingStats <- function(year = CurrentYear(), 
                         close.def.dist = '',
                         distance.range,
                         season.type = 'Regular Season',
                         per.mode = 'Totals') {
  
  options(stringsAsFactors = FALSE)
  
  per.mode <- CleanParam(per.mode)
  
  if (!missing(distance.range)) {
    request <- GET(
      "http://stats.nba.com/stats/leaguedashteamshotlocations",
      query = list(
        Conference = "",
        DateFrom = "",
        DateTo = "",
        DistanceRange = distance.range,
        Division = "",
        GameScope = "",
        GameSegment = "",
        LastNGames = 0,
        LeagueID = "00",
        Location = "",
        MeasureType = 'Base',
        Month = 0,
        OpponentTeamID = 0,
        Outcome = "",
        PORound = 0,
        PaceAdjust = 'N',
        PerMode = per.mode,
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
        VsConference = "",
        VsDivision = ""
      ),
      add_headers('User-Agent' = 'Mozilla/5.0')
    )
    content <- content(request, 'parsed')$resultSets
    
  } else {
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
      ),
      add_headers('User-Agent' = 'Mozilla/5.0')
    )
    content <- content(request, 'parsed')$resultSets[[1]]
  }
  
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}