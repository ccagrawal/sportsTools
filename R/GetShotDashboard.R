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
                             season.type = 'Regular Season', opponent.id = '0', player.ids, team.ids) {
  
  options(stringsAsFactors = FALSE)
  
  if (per.mode == 'Per Game') {
    per.mode <- 'PerGame'
  }

  # Get team dashboard
  if (missing(player)) {
    
    # If team name was provided, get team ID
    if (is.na(as.numeric(team))) {
      if (missing(team.ids)) {
        team.ids <- GetTeamIDs(year = year)
      }
      team <- team.ids[which(team.ids$name == team), 'id']
    }
    
    request <- GET(
      "http://stats.nba.com/stats/teamdashptshots",
      query = list(
        DateFrom = "",
        DateTo = "",
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
        TeamID = team,
        VsConference = "",
        VsDivision = ""
      )
    )
    
    content <- content(request, 'parsed')[[3]]
    char.cols <- c('TEAM_ID', 'TEAM_NAME')
    
    if (stat == 'Shot Type') {
      content <- content[[1]]
      char.cols <- c(char.cols, 'SHOT_TYPE')
    } else if (stat == 'Shot Clock Range') {
      content <- content[[2]]
      char.cols <- c(char.cols, 'SHOT_CLOCK_RANGE')
    } else if (stat == 'Dribble Range') {
      content <- content[[3]]
      char.cols <- c(char.cols, 'DRIBBLE_RANGE')
    } else if (stat == 'Defender Distance') {
      content <- content[[4]]
      char.cols <- c(char.cols, 'CLOSE_DEF_DIST_RANGE')
    } else if (stat == 'Defender Distance >10ft') {
      content <- content[[5]]
      char.cols <- c(char.cols, 'CLOSE_DEF_DIST_RANGE')
    } else if (stat == 'Touch Time Range') {
      content <- content[[6]]
      char.cols <- c(char.cols, 'TOUCH_TIME_RANGE')
    }
    
    stats <- ContentToDF(content)
    
    # Clean data frame
    char.cols <- which(colnames(stats) %in% char.cols)
    stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
    
  } else {
    url <- paste0('http://stats.nba.com/stats/playerdashptshots?',
                  'DateFrom=&',
                  'DateTo=&', 
                  'GameSegment=&', 
                  'LastNGames=0&', 
                  'LeagueID=00&', 
                  'Location=&', 
                  'Month=0&', 
                  'OpponentTeamID=', opponent.id, '&', 
                  'Outcome=&',
                  'PerMode=Totals&',
                  'Period=0&',
                  'PlayerID=', id, '&',
                  'Season=2015-16&',
                  'SeasonSegment=&',
                  'SeasonType=Regular+Season&',
                  'TeamID=0&',
                  'VsConference=&',
                  'VsDivision=')
    
    json <- fromJSON(file = url)[[3]]
    
    if (stat == 'Defender Distance') {
      json <- json[[5]]
    }
    
    stats <- json$rowSet
    
    # Create raw data frame
    stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
    stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
    
    # Get column headers
    colnames(stats) <- json$headers
    
    if (stat == 'Defender Distance') {
      char.cols <- c('PLAYER_ID', 'PLAYER_NAME_LAST_FIRST', 'CLOSE_DEF_DIST_RANGE')
      char.cols <- which(colnames(stats) %in% char.cols)
      stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
    }
  }
  
  return(stats)
}