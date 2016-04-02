#' Lineup stats
#' 
#' @param year e.g. 2015 for 2014-15 season
#' @param players Number of players to get lineup stats on (2-5)
#' @param type 'Advanced'
#' @return data frame of stats
#' @keywords lineup stats
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetLineupStats(year = 2016, players = 5, type = 'Advanced')

GetLineupStats <- function(year, players = 5, type = 'Advanced') {
  
  options(stringsAsFactors = FALSE)

  request = GET(
    "http://stats.nba.com/stats/leaguedashlineups",
    query = list(
      Conference = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      GameID = "",
      GameSegment = "",
      GroupQuantity = players,
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      MeasureType = type,
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = "N",
      PerMode = "Totals",
      Period = 0,
      PlusMinus = "N",
      Rank = "N",
      Season = .YearToSeason(year),
      SeasonSegment = "",
      SeasonType = "Regular Season",
      ShotClockRange = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers('Referer' = 'http://stats.nba.com/league/lineups/')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- content$rowSet
  
  # Create raw data frame
  stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats) <- content$headers
  
  char.cols <- c('GROUP_SET', 'GROUP_ID', 'GROUP_NAME', 'TEAM_ID', 'TEAM_ABBREVIATION')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)

  return(stats)
}