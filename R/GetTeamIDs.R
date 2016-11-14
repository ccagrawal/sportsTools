#' Team IDs on websites
#' 
#' @param sport 'NBA' or 'NCAAB'
#' @param year Season for which you want team IDs (e.g. 2008 for the 2007-08 season)
#' @param source Either 'NBA', 'ESPN', 'Basketball-Reference', or 'Sports-Reference'
#' @return data frame of names and IDs
#' @keywords team IDs
#' @importFrom rjson fromJSON
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetTeamIDs(2015)

GetTeamIDs <- function(sport = 'NBA', year = .CurrentYear(), source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  if (sport == 'NBA') {
    return(.GetTeamIDsNBA(year, source))
  } else if (sport == 'NCAAB') {
    return(.GetTeamIDsNCAAB(year, source))
  }
}

.GetTeamIDsNBA <- function(year = .CurrentYear(), source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'NBA') {
    
    if (year == .CurrentYear()) {
      game.date <- min(Sys.Date(), as.Date(paste0(year, '-04-04')))
    } else {
      game.date <- as.Date(paste0(year, '-04-04'))
    }
    
    request <- GET(
      "http://stats.nba.com/stats/scoreboard",
      query = list(
        DayOffset = 0,
        LeagueId = '00',
        gameDate = game.date
      ),
      add_headers('Referer' = 'http://stats.nba.com/standings/')
    )
    
    content.east <- content(request, 'parsed')[[3]][[5]]
    content.west <- content(request, 'parsed')[[3]][[6]]
    team.list <- rbind(.ContentToDF(content.east), .ContentToDF(content.west))
    
    team.list <- team.list[, c(1, 6)]      # Drop useless columns
    colnames(team.list) <- c('id', 'name')
  } else if (source == 'ESPN') {
    url <- 'http://espn.go.com/nba/teams'
    lines <- readLines(url)
    lines <- unlist(strsplit(lines, '<span>'))
    lines <- lines[grep('http://www.espn.com/nba/team/_/name/', lines)]
    
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