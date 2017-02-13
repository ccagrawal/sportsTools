#' Get Team Picture
#'
#' @param team.id Team ID from ESPN (e.g. 'hou')
#' @return team's picture
#' @keywords picture team
#' @importFrom png readPNG
#' @export
#' @examples
#' GetTeamPicture('hou')

GetTeamPicture <- function(team.id) {
  
  url <- gsub('###', team.id, 'http://a.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/###.png')
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  pic <- readPNG(temp)
  file.remove(temp)
  
  return(pic)
}