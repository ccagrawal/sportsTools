#' Hustle Stats.
#'
#' @param player.or.team Either 'player' or 'team'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type 'Regular Season' or 'Playoffs'
#' @param per.mode 'Per Game' or 'Totals'
#' @param quarter Quarter number (1, 2, 3, 4, 5 for OT1, 6 for OT2, etc.)
#' @return data frame with hustle stats
#' @keywords player team hustle
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetHustleStats(2017)

GetHustleStats <- function(player.or.team = 'player', 
                           year = CurrentYear(), 
                           season.type = 'Regular Season', 
                           per.mode = 'Totals',
                           quarter = 0) {
  
  options(stringsAsFactors = FALSE)
  
  if (player.or.team == 'player') {
    return(.GetPlayerHustleStats(year, season.type, per.mode))
  } else {
    return(.GetTeamHustleStats(year, season.type, per.mode, quarter))
  }
}

.GetPlayerHustleStats <- function(year, season.type, per.mode) {

  request <- GET(
    "http://stats.nba.com/stats/leaguehustlestatsplayer",
    query = list(
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      DraftPick = "",
      DraftYear = '',
      GameScope = "",
      Height = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = 'N',
      PerMode = CleanParam(per.mode),
      PlayerExperience = "",
      PlayerPosition = "",
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      TeamID = 0,
      VsConference = "",
      VsDivision = "",
      Weight = ""
    ),
    add_headers('Referer' = 'http://stats.nba.com/players/hustle/',
                'User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}

.GetTeamHustleStats <- function(year, season.type, per.mode, quarter) {
  
  request <- GET(
    "http://stats.nba.com/stats/leaguehustlestatsteam",
    query = list(
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      DraftPick = "",
      GameScope = "",
      GameSegment = "",
      Height = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = 'N',
      PerMode = CleanParam(per.mode),
      Period = quarter,
      PlayerExperience = "",
      PlayerPosition = "",
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      ShotClockRange = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = "",
      Weight = ""
    ),
    add_headers('User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}