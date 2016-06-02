#' NBA Player By Play
#' 
#' @param id Game ID
#' @param source Currently only 'NBA'
#' @return data frame of plays
#' @keywords play by play
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayByPlay(id = '0041500317')

GetPlayByPlay <- function(id, source = 'NBA') {
  
  options(stringsAsFactors = FALSE)
  
  request <- GET(
    "http://stats.nba.com/stats/playbyplayv2",
    query = list(
      EndPeriod = 14,
      EndRange = "",
      GameID = id,
      RangeType = 2,
      Season = "",
      SeasonType = "",
      StartPeriod = 0,
      StartRange = 0
    ),
    add_headers('Referer' = 'http://stats.nba.com/game/')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  actions <- content$rowSet
  
  # Create raw data frame
  actions <- lapply(actions, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  actions <- data.frame(matrix(unlist(actions), nrow = length(actions), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(actions) <- content$headers
  
  # Clean data frame
  actions[, c(2:5, 13, 20, 27)] <- sapply(actions[, c(2:5, 13, 20, 27)], as.numeric)
  
  return(actions)
}