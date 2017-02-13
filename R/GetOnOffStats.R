#' On-Off stats for each player on a team
#' 
#' @param team Either team name ('Golden State') or team id
#' @param year 2015 for 2014-15 season
#' @param season.type Either 'Regular Season' or 'Playoffs'
#' @param measure.type Either 'Base', 'Advanced', 'Misc', 'Four Factors', 'Scoring', 'Opponent', 'Usage', or 'Defense'
#' @param per.mode Either 'Totals', 'PerGame', 'MinutesPer', 'Per48', 'Per40', 'PerMinute', 'PerPossession', 'PerPlay', 'Per100Possessions', or 'Per100Plays'
#' @param team.ids Teams and their IDs
#' @return data frame of stats
#' @keywords onoff team
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetOnOffStats(team = '1610612745', measure.type = 'Advanced')

GetOnOffStats <- function(team, 
                          year = CurrentYear(), 
                          season.type = 'Regular Season', 
                          measure.type = 'Base', 
                          per.mode = 'Totals',
                          team.ids = NA) {
  
  options(stringsAsFactors = FALSE)
  
  measure.type <- CleanParam(measure.type)
  per.mode <- CleanParam(per.mode)
  team <- TeamNameToID(team, year, team.ids = team.ids)
  
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
      PerMode = per.mode,
      Period = 0,
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = "Regular Season",
      TeamID = team,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers('Referer' = 'http://stats.nba.com/team/',
                'User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]]
  stats.on <- ContentToDF(content[[2]])
  stats.off <- ContentToDF(content[[3]])
  
  # Merge on and off court stats
  stats <- rbind(stats.on, stats.off)
  
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}