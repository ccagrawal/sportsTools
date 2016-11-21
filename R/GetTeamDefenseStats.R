#' Team Defense Stats.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param defense.category 'Overall', '3 Pointers', '2 Pointers', 'Less Than 6Ft',
#' 'Less Than 10Ft', 'Greater Than 15Ft'
#' @param season.type 'Regular Season' or 'Playoffs'
#' @return data frame with wins and losses for that season
#' @keywords team shooting
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetTeamDefenseStats(2017)

GetTeamDefenseStats <- function(year = .CurrentYear(), 
                                defense.category = 'Less Than 6Ft',
                                season.type = 'Regular Season') {
  
  options(stringsAsFactors = FALSE)
  
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
      PerMode = 'Totals',
      Period = 0,
      Season = '2016-17',
      SeasonSegment = "",
      SeasonType = season.type,
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    )
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  
  stats <- content$rowSet
  
  # Create raw data frame
  stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats) <- content$headers
  
  # Clean data frame
  char.cols <- c('TEAM_ID', 'TEAM_NAME', 'TEAM_ABBREVIATION')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}