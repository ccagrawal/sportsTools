#' Team Defense Stats.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param defense.category 'Overall', '3 Pointers', '2 Pointers', 'Less Than 6Ft',
#' 'Less Than 10Ft', 'Greater Than 15Ft'
#' @param season.type 'Regular Season' or 'Playoffs'
#' @param per.mode 'Per Game' or 'Totals'
#' @return data frame with wins and losses for that season
#' @keywords team shooting
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetTeamDefenseStats(2017)

GetTeamDefenseStats <- function(year = CurrentYear(), 
                                defense.category = 'Less Than 6Ft',
                                season.type = 'Regular Season',
                                per.mode = 'Totals') {
  
  options(stringsAsFactors = FALSE)
  
  per.mode <- CleanParam(per.mode)
  
  request <- GET(
    "http://stats.nba.com/stats/leaguedashptteamdefend",
    query = list(
      Conference = "",
      DateFrom = "",
      DateTo = "",
      DefenseCategory = defense.category,
      Division = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PerMode = per.mode,
      Period = 0,
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers('User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- c('TEAM_ID', 'TEAM_NAME', 'TEAM_ABBREVIATION')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}