#' Betting lines.
#'
#' @param sport either "NBA" or "NFL"
#' @param year season (e.g. 2008 for 2007-08 season)
#' @param type either "regular season" or "playoffs" or "both"
#' @return data frame with schedule and line for each game in that season
#' @keywords schedule, odds, line, betting
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetLines("NBA", 2014, "playoffs")

GetLines <- function(sport = "NBA", year, type = "both") {
  
  options(stringsAsFactors = FALSE)
  sport <- tolower(sport)
  
  # Get team urls and names
  url <- gsub("SPORT", sport, "http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/SPORT/teams/teams.html")
  html <- readLines(url)
  links <- html[grep(gsub("SPORT", sport, "/data/SPORT/teams/team[0-9]"), html)]
  links <- unique(gsub(gsub("SPORT", sport, '.*/data/SPORT/teams/([^\"]*)\"[ ]*>([^<]*).*'), '\\1,\\2', links))
  links <- strsplit(links, ',')
  
  # Create URL stems for getting team records
  url.base <- paste(gsub("SPORT", sport, "http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/SPORT/teams/pastresults/"), year - 1, '-', year, '/', sep = '')
  full.lines <- data.frame()
  
  # Put all team records in one data frame
  for (i in 1:length(links)) {
    
    url <- paste(url.base, links[[i]][1], sep = "")
    tables <- readHTMLTable(url, header = TRUE)
    
    # Get appropriate data: regular season, playoffs, or both
    if (type == 'regular season') {
      
      if (length(tables) == 0) {  # If the team didn't exist at the time, make an empty df
        temp <- data.frame(matrix(nrow = 1, ncol = 6, data = 0))
        temp <- temp[-1, ]
      } else {
        if (sport == 'nfl') {
          temp <- tables[[length(tables) - 1]]  # Regular season is always the second to last table
        } else if (sport == 'nba') {
          temp <- tables[[length(tables)]]      # Regular season is always the last table
        }
      }
      
    } else if (type == 'playoffs') {
      
      if (length(tables) < 2) {  # If the team didn't make the playoffs, make an empty df
        temp <- data.frame(matrix(nrow = 1, ncol = 6, data = 0))
        temp <- temp[-1, ]
      } else {
        temp <- tables[[1]]
      }
      
    } else {
      
      if (length(tables) == 0) {  # If the team didn't exist at the time, make an empty df
        temp <- data.frame(matrix(nrow = 1, ncol = 6, data = 0))
        temp <- temp[-1, ]
      } else {
        if (sport == 'nfl') {
          if (length(tables) == 2) {
            temp <- tables[[1]]
          } else {
            temp <- rbind(tables[[1]], tables[[2]])
          }
        } else if (sport == 'nba') {
          if (length(tables) == 1) {
            temp <- tables[[1]]
          } else {
            temp <- rbind(tables[[1]], tables[[2]])
          }
        }
      }
      
    }
    
    colnames(temp) <- c("date", "away.team", "score", "type", "home.line", "over.under")
    
    if (nrow(temp) > 0) {   # Add home team if the df has any info (we didn't ask for playoffs from a team that didn't make it)
      temp$home.team <- links[[i]][2]
    }
    
    try(full.lines <- rbind(full.lines, temp), silent = FALSE)
  }
  
  # Extra formatting for NFL df
  if (sport == 'nfl') {
    full.lines <- full.lines[-which(is.na(full.lines$score)), ]
    full.lines$date <- gsub("[A-Za-z ]*", "", full.lines$date)
  }
  
  # Format date
  full.lines$date <- as.Date(full.lines$date, format = '%m/%d/%y')
  
  # Remove away games (the odds exist for home team already)
  full.lines <- full.lines[-grep('@', full.lines$away.team), ]
  
  # Create home score and away score columns
  full.lines$home.score <- as.numeric(gsub('[^0-9]*([0-9]+)-([0-9]+)[^0-9]*', '\\1', full.lines$score))
  full.lines$away.score <- as.numeric(gsub('[^0-9]*([0-9]+)-([0-9]+)[^0-9]*', '\\2', full.lines$score))
  
  # Fix line and over/under
  full.lines$home.line <- as.numeric(gsub('[^0-9\\.-]', '', full.lines$home.line))
  full.lines$over.under <- as.numeric(gsub('[^0-9\\.-]', '', full.lines$over.under))
  
  # Reorder columns, sort by date, and return
  full.lines <- full.lines[, c('date', 'type', 'home.team', 'away.team', 'home.score', 'away.score', 'home.line', 'over.under')]
  full.lines <- full.lines[order(full.lines$date), ]
  return(full.lines)
}

#' Betting lines (multi-year).
#'
#' @param sport either "NBA" or "NFL"
#' @param year.start season (e.g. 2008 for 2007-08 season)
#' @param year.end season (e.g. 2014 for 2013-14 season)
#' @param type either "regular season" or "playoffs" or "both"
#' @return data frame with schedule and line for each game in all the seasons
#' @keywords schedule, odds, line, betting
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetLinesRange("NBA", 2012, 2015, "playoffs")

GetLinesRange <- function(sport = "NBA", year.start, year.end, type = "both") {
  lines <- data.frame()
  
  for (year in year.start:year.end) {
    temp <- GetLines(sport, year, type)
    temp$season <- year
    lines <- rbind(lines, temp)
  }
  
  return(lines)
}