#' Shooting stats for players
#' 
#' @param player player's ID
#' @param stat which stat ('Shot Distance (5ft)', 'Shot Distance (8ft)', 'Shot Area', 
#' 'Assisted Shot', 'Shot Type Summary', 'Shot Type Detail', 'Assisted By')
#' @param player.ids Optional dataframe of player.ids
#' @return data frame of stats
#' @keywords shooting player
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetShootingStats(player = '201147', stat = 'Shot Type Detail')

GetShootingStats <- function(player, 
                             year = CurrentYear(), 
                             stat = 'Shot Type Detail',
                             per.mode = 'Totals',
                             player.ids = NA) {
  
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
      PerMode = CleanParam(per.mode),
      Period = 0,
      PlayerID = PlayerNameToID(player, year = year, player.ids = player.ids),
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = "Regular Season",
      ShotClockRange = "",
      VsConference = "",
      VsDivision = ""
    ),
    add_headers('Referer' = 'http://stats.nba.com/player/',
                'User-Agent' = 'Mozilla/5.0')
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
  
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}