#' Shooting stats for players
#' 
#' @param id player's ID
#' @param stat which stat ('Shot Distance (5ft)', 'Shot Distance (8ft)', 'Shot Area', 
#' 'Assisted Shot', 'Shot Type Summary', 'Shot Type Detail', 'Assisted By')
#' @return data frame of stats
#' @keywords shooting player
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetShootingStats(id = '201147', stat = 'Shot Type Detail')

GetShootingStats <- function(id, year = CurrentYear(), stat = 'Shot Type Detail') {
  
  options(stringsAsFactors = FALSE)
  
  request = GET(
    "http://stats.nba.com/stats/playerdashboardbyshootingsplits",
    query = list(
      DateFrom = "",
      DateTo = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      MeasureType = "Base",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = "N",
      PerMode = "Totals",
      Period = 0,
      PlayerID = id,
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = "Regular Season",
      ShotClockRange = "",
      VsConference = "",
      VsDivision = ""
    ),
    add_headers('Referer' = 'http://stats.nba.com/player/')
  )
  
  content <- content(request, 'parsed')[[3]]
  
  if (stat == 'Shot Distance (5ft)') {
    content <- content[[2]]
  } else if (stat == 'Shot Distance (8ft)') {
    content <- content[[3]]
  } else if (stat == 'Shot Area') {
    content <- content[[4]]
  } else if (stat == 'Assisted Shot') {
    content <- content[[5]]
  } else if (stat == 'Shot Type Summary') {
    content <- content[[6]]
  } else if (stat == 'Shot Type Detail') {
    content <- content[[7]]
  } else if (stat == 'Assisted By') {
    content <- content[[8]]
  }
  
  stats <- ContentToDF(content)
  
  if (stat == 'Shot Type Detail') {
    char.cols <- c('GROUP_SET', 'GROUP_VALUE', 'CFPARAMS')
    char.cols <- which(colnames(stats) %in% char.cols)
    stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  }
  
  return(stats)
}