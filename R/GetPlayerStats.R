#' Player stats per season.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param season.type Either 'Regular Season' or 'Playoffs'
#' @param measure.type Either 'Basic', 'Advanced', or 'RPM'
#' @param per.mode Either 'Per Game', 'Totals', or '100 Possessions'
#' @param position Either 'G', 'F', or 'C'
#' @param Location Either 'Home' or 'Away
#' @param source Either 'Basketball-Reference', 'NBA', or 'ESPN'
#' @return data frame with players stats
#' @keywords player
#' @importFrom XML readHTMLTable
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerStats(2014, measure.type = 'Basic')

GetPlayerStats <- function(year = CurrentYear(), 
                           season.type = 'Regular Season', 
                           measure.type = 'Basic', 
                           per.mode = 'Per Game',
                           position = '',
                           location = '',
                           source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'NBA') {
    return(.GetPlayerStatsNBA(year, season.type, measure.type, per.mode, position, location))
  } else if (source == 'ESPN') {
    return(.GetPlayerStatsESPN(year, measure.type, per.mode))
  }
}

.GetPlayerStatsNBA <- function(year, season.type, measure.type, per.mode, position, location) {
  
  measure.type <- CleanParam(measure.type)
  per.mode <- CleanParam(per.mode)
  
  request <- GET(
    "http://stats.nba.com/stats/leaguedashplayerstats",
    query = list(
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      DraftPick = "",
      DraftYear = "",
      GameScope = "",
      GameSegment = "",
      Height = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = location,
      MeasureType = measure.type,
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = "N",
      PerMode = per.mode,
      Period = 0,
      PlayerExperience = "",
      PlayerPosition = position,
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      ShotClockRange = "",
      StarterBench = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = "",
      Weight = ""
    ),
    add_headers(
      'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
      'Referer' = 'http://stats.nba.com/player/',
      'User-Agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36'
    )
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}

.GetPlayerStatsESPN <- function(year, measure.type = 'RPM', per.mode = 'Per Game') {
  
  if (CleanParam(measure.type) == 'RPM') {
    base.url <- paste0('http://espn.go.com/nba/statistics/rpm/_/year/', year, '/page/PPPP/sort/RPM')
    
    continue <- TRUE
    i <- 1
    df <- data.frame()
    
    # Loop through pages until we get to an empty page
    while(continue) {
      url <- gsub('PPPP', i, base.url)
      table <- readHTMLTable(url)[[1]]
      
      if (is.null(table)) {
        continue <- FALSE
      } else {
        df <- rbind(df, table)
        i <- i + 1
      }
    }
    
    # Split up Name into Name and Position
    df$POS <- gsub('.*, (.*)', '\\1', df$NAME)
    df$NAME <- gsub('(.*),.*', '\\1', df$NAME)
    df <- df[, c(2, 3, 10, 4:9)]
    
    # Fix the column types
    df[, -c(1:3)] <- lapply(df[, -c(1:3)], as.numeric)
  }
  
  return(df)
}