#' Team Hustle Stats.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type 'Regular Season' or 'Playoffs'
#' @param per.mode 'Per Game' or 'Totals'
#' @param quarter Quarter number (1, 2, 3, 4, 5 for OT1, 6 for OT2, etc.)
#' @return data frame with team stats
#' @keywords team hustle
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetTeamHustleStats(2017)

GetTeamHustleStats <- function(year = CurrentYear(), 
                               season.type = 'Regular Season', 
                               per.mode = 'Totals',
                               quarter = 0) {
  
  options(stringsAsFactors = FALSE)
  
  request <- GET(
    "http://stats.nba.com/stats/leaguehustlestatsteam",
    query = list(
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      DraftPick = "",
      GameScope = "",
      GameSegment = "",
      Height = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = 'N',
      PerMode = per.mode,
      Period = quarter,
      PlayerExperience = "",
      PlayerPosition = "",
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      ShotClockRange = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = "",
      Weight = ""
    ),
    add_headers('User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}