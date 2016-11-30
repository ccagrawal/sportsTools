#' Get Shots
#'
#' @param player.id Player ID
#' @param year e.g. 2015 for 2014-15 season
#' @param season.type either 'regular', 'playoffs', or 'both'
#' @return data frame of shots
#' @keywords shots
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' GetShots('201935', 2015, 'both')

GetShots <- function(player.id, year, season.type = 'regular') {
  
  options(stringsAsFactors = FALSE)
  
  season <- paste0(year - 1, '-', year %% 100)    # e.g. 2015 becomes 2014-15
  
  base.url <- paste0('http://stats.nba.com/stats/playerdashptshotlog?',
                     'DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&',
                     'OpponentTeamID=0&Outcome=&Period=0&PlayerID=PPPP&Season=SSSS&',
                     'SeasonSegment=&SeasonType=TTTT&TeamID=0&VsConference=&VsDivision=')
  
  base.url <- gsub('PPPP', player.id, base.url)
  base.url <- gsub('SSSS', season, base.url)
  
  # Create NBA URL based on season type
  if ((season.type == 'regular') | (season.type == 'both')) {
    url <- gsub('TTTT', 'Regular+Season', base.url)
  } else {
    url <- gsub('TTTT', 'Playoffs', base.url)
  }
  
  json <- fromJSON(file = url)[[3]]                  # (3) contains the actual info for the day
  temp <- json[[1]]                                  # (1) contains game list info
  shot.list <- temp[[3]]                             # (3) contains the actual rows
  
  # Also grab playoff games if both were requested
  if (season.type == 'both') {
    url <- gsub('TTTT', 'Playoffs', base.url)
    json <- fromJSON(file = url)[[3]]                  # (3) contains the actual info for the day
    temp <- json[[1]]                                  # (1) contains game list info
    shot.list <- c(shot.list, temp[[3]])               # (3) contains the actual rows
  }
  
  if (length(shot.list) > 0) {
    # Create raw data frame
    shot.list <- lapply(shot.list, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
    shot.list <- data.frame(matrix(unlist(shot.list), nrow = length(shot.list), byrow = TRUE)) # Turn list to data frame
    
    # Clean data frame
    shot.list <- shot.list[, c(1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17)]      # Drop useless columns
    colnames(shot.list) <- c('game.id', 'shot.num', 'period', 'game.clock', 'shot.clock', 'dribbles', 
                             'touch.time', 'shot.distance', 'fg.type', 'result', 'defender.id', 'defender.distance')
    
    # Fix column types
    shot.list[which(shot.list$result == 'missed'), 'result'] <- 0
    shot.list[which(shot.list$result == 'made'), 'result'] <- 1
    shot.list[, c(2, 3, 5, 6, 7, 8, 9, 10, 12)] <- sapply(shot.list[, c(2, 3, 5, 6, 7, 8, 9, 10, 12)], as.numeric)
    shot.list$game.clock <- ToSeconds(shot.list$game.clock)
    
    return(shot.list)
  }
}