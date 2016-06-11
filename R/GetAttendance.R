#' Attendance
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @return data frame with home, road, and overall attendance for each team
#' @keywords attendance
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' GetAttendance(2014)

GetAttendance <- function(year) {
  
  options(stringsAsFactors = FALSE)
  url <- 'http://espn.go.com/nba/attendance/_/year/YYYY'
  url <- gsub('YYYY', year, url)
  tables <- readHTMLTable(url)
  
  # Get table and remove extra columns
  attendance <- tables[[1]]
  attendance <- attendance[-1, ]
  colnames(attendance) <- c('Rank', 'Team', 'Home.Games', 'Home.Total', 'Home.Average', 'Home.Percent',
                            'Away.Games', 'Away.Average', 'Away.Percent', 'Games', 'Average', 'Percent')
  
  return(attendance)
}