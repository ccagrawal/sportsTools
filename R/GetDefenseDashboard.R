#' Player's defensive dashboard
#'
#' @param player NBA player (e.g. "James Harden")
#' @param per.mode Either 'Totals', 'Per Game'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type Either 'Regular Season' or 'Playoffs'
#' @param player.ids Players and their IDs
#' @return data frame with player's defensive stats
#' @keywords player defense
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetDefenseDashboard('James Harden')

GetDefenseDashboard <- function(player,
                                year = CurrentYear(), 
                                season.type = 'Regular Season',
                                per.mode = 'Totals',
                                player.ids = NA) {
  
  options(stringsAsFactors = FALSE)
  
  per.mode <- CleanParam(per.mode)
  player <- PlayerNameToID(player, year, player.ids)
  
  request <- GET(
    "http://stats.nba.com/stats/playerdashptshotdefend",
    query = list(
      DateFrom = "",
      DateTo = "",
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
      PlayerID = player,
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
  
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}