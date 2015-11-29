#' Team IDs on websites
#' 
#' @param year NBA season for which you want team IDs (e.g. 2008 for the 2007-08 season)
#' @param source website that is being used (e.g. 'Basketball-Reference')
#' @return data frame of names and IDs
#' @keywords team IDs
#' @importFrom rjson fromJSON
#' @export
#' @examples
#' GetTeamIDs(2015)

GetTeamIDs <- function(year = 2016, source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  # Create NBA URL and scrape the JSON for the input day
  url <- gsub('YEAR', year, 'http://stats.nba.com/stats/scoreboard?DayOffset=0&LeagueID=00&gameDate=03%2F20%2FYEAR')
  json <- fromJSON(file = url)[[3]]                  # (3) contains the actual info for the day
  
  # Check if games exist for the given day
  team.list <- c(json[[5]]$rowSet, json[[6]]$rowSet)      # Combine standings from East and West
    
  # Create raw data frame
  team.list <- lapply(team.list, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  team.list <- data.frame(matrix(unlist(team.list), nrow = length(team.list), byrow = TRUE)) # Turn list to data frame
  
  # Clean data frame
  team.list <- team.list[, c(1, 6)]      # Drop useless columns
  colnames(team.list) <- c('id', 'name')
  
  return(team.list)
}