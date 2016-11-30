#' Player specific stats in a season.
#'
#' @param player NBA player (e.g. "James Harden")
#' @param measure.type Either 'Base', 'Advanced', 'Misc', 'Scoring', or 'Usage'
#' @param per.mode Either 'Totals', 'PerGame'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param player.ids Players and their IDs
#' @param source Either 'Basketball-Reference' or 'NBA'
#' @return data frame with player's stats
#' @keywords player
#' @importFrom XML readHTMLTable
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerSpecificStats('Anthony Davis', 'on-off', 2015)

GetPlayerSpecificStats <- function(player, 
                                   measure.type,
                                   per.mode = 'Totals',
                                   year = CurrentYear(), 
                                   player.ids,
                                   source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  # If player name was provided, get player ID
  if (is.na(as.numeric(player))) {
    if (missing(player.ids)) {
      player.ids <- GetPlayerIDs(year = year)
    }
    player <- player.ids[which(player.ids$name == player), 'id']
  }
  
  if (source == 'Basketball-Reference') {
    return(.GetPlayerSpecificStatsBRef(player, measure.type, year))
  } else if (source == 'NBA') {
    return(.GetPlayerSpecificStatsNBA(player, measure.type, per.mode, year))
  }
}

.GetPlayerSpecificStatsBRef <- function(player, measure.type, year) {

  base.url <- paste0('http://www.basketball-reference.com/players', player)
  
  if (measure.type == 'on-off') {
    url <- paste0(base.url, '/on-off/', year, '/')
    
    table <- readHTMLTable(url)[[1]]
    table[, -c(1, 2)] <- sapply(table[, -c(1, 2)], as.numeric)
  } else if (measure.type == 'per game') {
    url <- paste0(base.url, '.html')
    
    table <- readHTMLTable(url)[['per_game']]
    table[, -c(1, 3, 4, 5)] <- sapply(table[, -c(1, 3, 4, 5)], as.numeric)
    table$Season <- as.numeric(substr(table$Season, 1, 4)) + 1
  } else if (measure.type == 'totals') {
    url <- paste0(base.url, '.html')
    
    table <- readHTMLTable(url)[['totals']]
    table[, -c(1, 3, 4, 5)] <- sapply(table[, -c(1, 3, 4, 5)], as.numeric)
    table$Season <- as.numeric(substr(table$Season, 1, 4)) + 1
  } else if (measure.type == 'shooting') {
    url <- paste0(base.url, '/shooting/', year, '/')
    
    table <- readHTMLTable(url)[[1]]
    table <- table[3:nrow(table), ]
    colnames(table) <- table[1, ]
    table <- table[-which(table$Split == 'Split'), ]
    table[, 3:11] <- sapply(table[, 3:11], as.numeric)
  } else if (measure.type == 'play by play') {
    url <- paste0(base.url, '.html')
    
    table <- readHTMLTable(url)[['advanced_pbp']]
    table[, c(8:12)] <- sapply(table[, c(8:12)], function(x) gsub('%', '', x))
    table[, -c(1, 3, 4, 5)] <- sapply(table[, -c(1, 3, 4, 5)], as.numeric)
    table[, c(8:12)] <- table[, c(8:12)] / 100
    table$Season <- as.numeric(substr(table$Season, 1, 4)) + 1
  } else if (measure.type == 'advanced') {
    url <- paste0(base.url, '.html')
    
    table <- readHTMLTable(url)[['advanced']]
    table <- table[, -c(20, 25)]
    table[, -c(1, 3, 4, 5)] <- sapply(table[, -c(1, 3, 4, 5)], as.numeric)
    table$Season <- as.numeric(substr(table$Season, 1, 4)) + 1
  } else if (measure.type == 'playoff totals') {
    url <- paste0(base.url, '.html')
    
    table <- readHTMLTable(url)[['playoffs_totals']]
    table[, -c(1, 3, 4, 5)] <- sapply(table[, -c(1, 3, 4, 5)], as.numeric)
    table$Season <- as.numeric(substr(table$Season, 1, 4)) + 1
  }
  
  return(table)
}

.GetPlayerSpecificStatsNBA <- function(player, measure.type, per.mode, year) {
  
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
    )
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- c('TEAM_ID', 'TEAM_NAME', 'CFID', 'CFPARAMS')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}