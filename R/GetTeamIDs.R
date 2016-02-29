#' Team IDs on websites
#' 
#' @param sport 'NBA' or 'NCAAB'
#' @param year NBA season for which you want team IDs (e.g. 2008 for the 2007-08 season)
#' @param source website that is being used ('NBA', 'ESPN', 'Basketball-Reference', 'Sports-Reference')
#' @return data frame of names and IDs
#' @keywords team IDs
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' GetTeamIDs(2015)

GetTeamIDs <- function(sport = 'NBA', year = 2016, source = 'NBA') {
  
  if (sport == 'NBA') {
    return(.GetTeamIDsNBA(year, source))
  } else if (sport == 'NCAAB') {
    return(.GetTeamIDsNCAAB(year, source))
  }
}

.GetTeamIDsNBA <- function(year = 2016, source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'NBA') {
    year <- year - 1
    url <- gsub('YEAR', year, 'http://stats.nba.com/stats/playoffpicture?LeagueID=00&SeasonID=2YEAR')
    json <- fromJSON(file = url)[[3]]                  # (3) contains the actual info for the day
    team.list <- c(json[[3]]$rowSet, json[[4]]$rowSet)      # Combine standings from East and West
    
    team.list <- lapply(team.list, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
    team.list <- data.frame(matrix(unlist(team.list), nrow = length(team.list), byrow = TRUE)) # Turn list to data frame
    
    team.list <- team.list[, c(4, 3)]      # Drop useless columns
    colnames(team.list) <- c('id', 'name')
  } else if (source == 'ESPN') {
    url <- 'http://espn.go.com/nba/teams'
    lines <- readLines(url)
    lines <- lines[grep('http://espn.go.com/nba/team/_/name', lines)]
    
    team.list <- gsub('.*team/_/name/([^/]*)/([^\"]*).*', '\\1~\\2', lines)
    team.list <- strsplit(team.list, '~')
    team.list <- data.frame(matrix(unlist(team.list), nrow = length(team.list), byrow = TRUE))
    colnames(team.list) <- c('id', 'name')
    
    team.list$name <- gsub('-', ' ', team.list$name)
    team.list$name <- sapply(team.list$name, .CapLetters)
  } else if (source == 'Basketball-Reference') {
    url <- gsub('YEAR', year, 'http://www.basketball-reference.com/leagues/NBA_YEAR.html')
    lines <- readLines(url)
    lines <- unique(lines[grep(' ><a href=\"/teams/', lines)])
    
    team.list <- gsub('.*teams/([^/]*)/[^>]*>([^<]*).*', '\\1~\\2', lines)
    team.list <- strsplit(team.list, '~')
    team.list <- data.frame(matrix(unlist(team.list), nrow = length(team.list), byrow = TRUE))
    colnames(team.list) <- c('id', 'name')
    
    team.list$name <- gsub('-', ' ', team.list$name)
    team.list$name <- sapply(team.list$name, .CapLetters)
  }
  
  return(team.list)
}

.GetTeamIDsNCAAB <- function(year = 2016, source = 'Sports-Reference') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'Sports-Reference') {
    url <- paste0('http://www.sports-reference.com/cbb/seasons/', year, '-school-stats.html')
    
    team.list <- readLines(url)
    team.list <- team.list[grepl('/cbb/schools/[a-z]', team.list)]
    team.list <- unique(gsub('.*/cbb/schools/([^/]*)[^>]*>([^<]*).*', '\\1~\\2', team.list))
    
    team.list <- strsplit(team.list, '~')
    team.list <- data.frame(matrix(unlist(team.list), nrow = length(team.list), byrow = TRUE))
    colnames(team.list) <- c('id', 'name')
    
    team.list$name <- gsub('&amp;', '&', team.list$name)
  }
  
  return(team.list)
}