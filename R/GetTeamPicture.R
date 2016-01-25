#' Get Team Picture
#'
#' @param team.id Team ID
#' @param source currently only 'ESPN'
#' @return team's picture
#' @keywords picture team
#' @importFrom png readPNG
#' @export
#' @examples
#' GetTeamPicture('hou', source = 'ESPN')

GetTeamPicture <- function(team.id, source = 'ESPN') {
  
  url <- gsub('###', team.id, 'http://a.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/###.png')
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  pic <- readPNG(temp)
  file.remove(temp)
  
  return(pic)
}