#' Shot Dashboard stats on players or teams
#' 
#' @param player Player name (if player's dashboard is desired)
#' @param team Team name (if team's dashboard is desired)
#' @param stat Statistic to pull (e.g. 'Defender Distance' or 'Shot Clock Range')
#' @param year e.g. 2017 for 2016-17 season
#' @param per.mode Either 'Totals' or 'Per Game'
#' @param season.type Either 'Regular Season' or 'Playoffs'
#' @param opponent.id Opponent team name
#' @return data frame of stats
#' @keywords shooting player team
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetShotDashboard('James Harden', stat = 'Defender Distance')

GetShotDashboard <- function(player, team, stat, year = CurrentYear(), per.mode = 'Totals', 
                             season.type = 'Regular Season', opponent.id = '0', 
                             date.from = '', date.to = '', ids) {
  
  options(stringsAsFactors = FALSE)
  
  per.mode <- CleanParam(per.mode)

  # Get team dashboard
  if (missing(player)) {
    return(.GetTeamShotDashboard(team, stat, year, per.mode, season.type, opponent.id, 
                                 date.from, date.to, ids))
  } else {
    return(.GetPlayerShotDashboard(player, stat, year, per.mode, season.type, opponent.id, 
                                   date.from, date.to, ids))
  }
  
  return(stats)
}

.GetTeamShotDashboard <- function(team, stat, year = CurrentYear(), per.mode = 'Totals', 
                                  season.type = 'Regular Season', opponent.id = '0', 
                                  date.from = '', date.to = '', team.ids = NA) {
  
  request <- GET(
    "http://stats.nba.com/stats/teamdashptshots",
    query = list(
      DateFrom = date.from,
      DateTo = date.to,
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      MeasureType = 'Base',
      Month = 0,
      OpponentTeamID = opponent.id,
      Outcome = "",
      PORound = 0,
      PaceAdjust = 'N',
      PerMode = per.mode,
      Period = 0,
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      TeamID = TeamNameToID(team, year, team.ids),
      VsConference = "",
      VsDivision = ""
    ),
    add_headers('User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]]
  
  if (stat == 'Shot Type') {
    content <- content[[1]]
  } else if (stat == 'Shot Clock Range') {
    content <- content[[2]]
  } else if (stat == 'Dribble Range') {
    content <- content[[3]]
  } else if (stat == 'Defender Distance') {
    content <- content[[4]]
  } else if (stat == 'Defender Distance >10ft') {
    content <- content[[5]]
  } else if (stat == 'Touch Time Range') {
    content <- content[[6]]
  }
  
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}


.GetPlayerShotDashboard <- function(player, stat, year = CurrentYear(), per.mode = 'Totals', 
                                    season.type = 'Regular Season', opponent.id = '0', 
                                    date.from = '', date.to = '', player.ids = NA) {
  
  request <- GET(
    "http://stats.nba.com/stats/playerdashptshots",
    query = list(
      DateFrom = date.from,
      DateTo = date.to,
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      MeasureType = 'Base',
      Month = 0,
      OpponentTeamID = opponent.id,
      Outcome = "",
      PerMode = CleanParam(per.mode),
      Period = 0,
      PlayerID = PlayerNameToID(player, year, player.ids),
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers('User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]]
  
  if (stat == 'Shot Type') {
    content <- content[[1]]
  } else if (stat == 'Shot Clock Range') {
    content <- content[[2]]
  } else if (stat == 'Dribble Range') {
    content <- content[[3]]
  } else if (stat == 'Defender Distance') {
    content <- content[[5]]
  } else if (stat == 'Defender Distance >10ft') {
    content <- content[[5]]
  } else if (stat == 'Touch Time Range') {
    content <- content[[6]]
  }
  
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}