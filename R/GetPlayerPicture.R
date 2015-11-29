#' Get Player Picture
#'
#' @param player.id Player ID
#' @param source currently only 'NBA'
#' @return player's picture
#' @keywords picture player
#' @importFrom png readPNG
#' @export
#' @examples
#' GetPlayerPicture('201935', source = 'NBA')

GetPlayerPicture <- function(player.id, source = 'NBA') {
  
  url <- gsub('###', player.id, 'http://stats.nba.com/media/players/230x185/###.png')
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  pic <- readPNG(temp)
  file.remove(temp)
  
  return(pic)
}