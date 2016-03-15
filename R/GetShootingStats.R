#' Shooting stats for players
#' 
#' @param id player's ID
#' @param stat which stat ('Shot Type Detail')
#' @return data frame of stats
#' @keywords shooting player
#' @importFrom httr GET content
#' @export
#' @examples
#' GetShootingStats(id = '201147', stat = 'Shot Type Detail')

GetShootingStats <- function(id, stat = 'Shot Type Detail') {
  
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
      Season = "2015-16",
      SeasonSegment = "",
      SeasonType = "Regular Season",
      ShotClockRange = "",
      VsConference = "",
      VsDivision = ""
    ),
    add_headers('Referer' = 'http://stats.nba.com/player/')
  )
  
  content <- content(request, 'parsed')[[3]]
  
  if (stat == 'Shot Type Detail') {
    content <- content[[7]]
  }
  
  stats <- content$rowSet
  
  # Create raw data frame
  stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats) <- content$headers
  
  if (stat == 'Shot Type Detail') {
    char.cols <- c('GROUP_SET', 'GROUP_VALUE', 'CFPARAMS')
    char.cols <- which(colnames(stats) %in% char.cols)
    stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  }
  
  return(stats)
}