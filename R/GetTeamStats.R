#' Team Stats.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param stat.type 'regular' or 'advanced'
#' @param season.type 'regular' or 'playoffs'
#' @return data frame with wins and losses for that season
#' @keywords team
#' @importFrom XML readHTMLTable
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' GetTeamStats(2014)

GetTeamStats <- function(year, stat.type = 'regular', season.type = 'regular', keep.average = FALSE, source = 'Basketball-Reference') {
  
  if (source == 'Basketball-Reference') {
    return(.GetTeamStatsBRef(year, stat.type, season.type, keep.average))
  } else if (source == 'NBA') {
    return(.GetTeamStatsNBA(year, stat.type, season.type))
  }
}

.GetTeamStatsBRef <- function(year, stat.type = 'regular', season.type = 'regular', keep.average = FALSE) {
  
  options(stringsAsFactors = FALSE)
  
  if (season.type == 'playoffs') {
    url <- 'http://www.basketball-reference.com/playoffs/NBA_YYYY.html'
  } else {
    url <- 'http://www.basketball-reference.com/leagues/NBA_YYYY.html'
  }
  
  url <- gsub('YYYY', year, url)
  tables <- readHTMLTable(url)
  
  # Get table, convert appropriate columns to numeric, remove asterisks from team names
  if (stat.type == 'advanced') {
    
    stats <- tables[['misc']]
    stats[, -c(2, 23, 24)] <- lapply(stats[, -c(2, 23, 24)], as.numeric)
    stats$Attendance <- as.numeric(gsub(',', '', stats$Attendance))
    stats$Team <- gsub('\\*', '', stats$Team)
    
  } else if (stat.type == 'team') {
    
    stats <- tables[['team']]
    stats[, -2] <- lapply(stats[, -2], as.numeric)
    stats$Team <- gsub('\\*', '', stats$Team)
    
  } else if (stat.type == 'opponent') {
    
    stats <- tables[['opponent']]
    stats[, -2] <- lapply(stats[, -2], as.numeric)
    stats$Team <- gsub('\\*', '', stats$Team)
    
  } else if (stat.type == 'shooting') {
    
    stats <- tables[['shooting']]
    stats[, -2] <- lapply(stats[, -2], as.numeric)
    stats$Team <- gsub('\\*', '', stats$Team)
    
  } else if (stat.type == 'shooting opponent') {
    
    stats <- tables[['shooting_opp']]
    stats[, -2] <- lapply(stats[, -2], as.numeric)
    stats$Team <- gsub('\\*', '', stats$Team)
    
  } else if (stat.type == 'standings') {
    
    indices <- c(which(names(tables) == 'E_standings')[1], which(names(tables) == 'W_standings')[1])
    temp.east <- tables[[indices[1]]]
    temp.west <- tables[[indices[2]]]
    colnames(temp.east)[1] <- 'Team'
    colnames(temp.west)[1] <- 'Team'
    
    stats <- rbind(temp.east, temp.west)
    stats[, -1] <- lapply(stats[, -1], as.numeric)
    stats <- stats[-which(is.na(stats$W)), ]
    
    stats[which(is.na(stats$GB)), 'GB'] <- 0
    stats$Seed <- as.numeric(gsub('[^(]*\\(([^)]*)\\)', '\\1', stats$Team))
    
    stats$Team <- gsub('[\\*|\\(].*', '', stats$Team)
    stats$Team <- gsub('[^A-Za-z]$', '', stats$Team)
    
  }
  
  if (stat.type != 'standings') {
    if (!keep.average) {
      stats <- stats[-which(is.na(stats$Rk)), -1]   # Remove league average row and rank column
    } else {
      stats <- stats[, -1]
    }
  }
}

.GetTeamStatsNBA <- function(year, stat.type = 'regular', season.type = 'regular') {
  
  options(stringsAsFactors = FALSE)
  
  season <- .YearToSeason(year)
  
  url <- paste0('http://stats.nba.com/stats/leaguedashteamstats?Conference=&',
                'DateFrom=&',
                'DateTo=&',
                'Division=&',
                'GameScope=&',
                'GameSegment=&',
                'LastNGames=0&',
                'LeagueID=00&',
                'Location=&',
                'MeasureType=', stat.type, '&',
                'Month=0&',
                'OpponentTeamID=0&',
                'Outcome=&',
                'PORound=0&',
                'PaceAdjust=N&',
                'PerMode=Totals&',
                'Period=0&',
                'PlayerExperience=&',
                'PlayerPosition=&',
                'PlusMinus=N&',
                'Rank=N&',
                'Season=', season, '&',
                'SeasonSegment=&',
                'SeasonType=Regular+Season&',
                'ShotClockRange=&',
                'StarterBench=&',
                'TeamID=0&',
                'VsConference=&',
                'VsDivision=')
  
  json <- fromJSON(file = url)[[3]][[1]]
  
  stats <- json$rowSet
  
  # Create raw data frame
  stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats) <- json$headers
  
  # Clean data frame
  char.cols <- c('TEAM_ID', 'TEAM_NAME', 'CFID', 'CFPARAMS')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}