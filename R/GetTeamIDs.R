#' Team IDs on websites
#' 
#' @param year Season for which you want team IDs (e.g. 2008 for the 2007-08 season)
#' @param source Either 'NBA' or 'ESPN'
#' @return data frame of names and IDs
#' @keywords team IDs
#' @importFrom rjson fromJSON
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetTeamIDs(2015)

GetTeamIDs <- function(year = CurrentYear(), source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  if (source == 'NBA') {
    
    team.list <- GetTeamStats(year = year)
    team.list <- team.list[, c(1, 2)]      # Drop useless columns
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
    team.list$name <- sapply(team.list$name, CapLetters)
  }
  
  return(team.list)
}