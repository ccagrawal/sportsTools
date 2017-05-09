#' Player specific stats in a season.
#'
#' @param player NBA player (e.g. "James Harden")
#' @param measure.type Either 'Base', 'Advanced', 'Misc', 'Scoring', or 'Usage'
#' @param per.mode Either 'Totals', 'PerGame'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type Either 'Regular Season' or 'Playoffs'
#' @param player.ids Players and their IDs
#' @return data frame with player's stats
#' @keywords player
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerSpecificStats('Anthony Davis', 'Scoring', 2015)

GetPlayerSpecificStats <- function(player, 
                                   measure.type,
                                   per.mode = 'Totals',
                                   year = CurrentYear(),
                                   season.type = 'Regular Season',
                                   player.ids = NA) {
  
  options(stringsAsFactors = FALSE)
  
  measure.type <- CleanParam(measure.type)
  per.mode <- CleanParam(per.mode)
  player <- PlayerNameToID(player, year, player.ids)
  
  request <- GET(
    "http://stats.nba.com/stats/playerdashboardbyyearoveryear",
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
      PORound = 0,
      PaceAdjust = 'N',
      PerMode = per.mode,
      Period = 0,
      PlayerID = player,
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      ShotClockRange = "",
      Split = "yoy",
      VsConference = "",
      VsDivision = ""
    ),
    add_headers(
      'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
      'Referer' = 'http://stats.nba.com/player/',
      'User-Agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36'
    )
  )
  
  content <- content(request, 'parsed')[[3]][[2]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  remove.cols <- c('GROUP_SET', 'CFPARAMS', 'CFID')
  remove.cols <- which(colnames(stats) %in% remove.cols)
  stats <- stats[, -remove.cols]
  
  stats$GROUP_VALUE <- SeasonToYear(stats$GROUP_VALUE)
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}