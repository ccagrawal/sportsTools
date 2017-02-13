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
      MeasureType = CleanParam(measure.type),
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = "N",
      PerMode = CleanParam(per.mode),
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
    add_headers('Referer' = 'http://stats.nba.com/team/',
                'User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}