#' Schedule and Results.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type 'Regular Season' or 'Playoffs'
#' @return data frame with game logs
#' @keywords game logs
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetSchedule(year = 2014, season.type = 'playoffs')

GetSchedule <- function(year = CurrentYear(), season.type = 'Regular Season') {
  
  options(stringsAsFactors = FALSE)
  
  request <- GET(
    "http://stats.nba.com/stats/leaguegamelog",
    query = list(
      Counter = 3000,
      DateFrom = "",
      DateTo = "",
      Direction = "DESC",
      LeagueID = "00",
      PlayerOrTeam = 'T',
      Season = YearToSeason(year),
      SeasonType = season.type,
      Sorter = "DATE"
    ),
    add_headers(
      'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
      'User-Agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36'
    )
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  logs <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(logs) %in% CHARACTER.COLUMNS)
  logs[, -char.cols] <- sapply(logs[, -char.cols], as.numeric)
  logs$GAME_DATE <- as.Date(logs$GAME_DATE)
  
  # Combine Home and Away
  home <- logs[grep('vs.', logs$MATCHUP), ]
  colnames(home) <- paste0('home.', colnames(home))
  
  extra.cols <- c('SEASON_ID', 'GAME_DATE', 'VIDEO_AVAILABLE', 'MATCHUP', 'WL', 'PLUS_MINUS')
  away <- logs[grep('@', logs$MATCHUP), -which(colnames(logs) %in% extra.cols)]
  colnames(away) <- paste0('away.', colnames(away))
  
  return(merge(home, away, by.x = 'home.GAME_ID', by.y = 'away.GAME_ID'))
}