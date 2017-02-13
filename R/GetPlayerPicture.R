#' Get Player Picture
#'
#' @param player Player's name or ID
#' @param player.ids Optional, data frame of player.ids
#' @return player's picture
#' @keywords picture player
#' @importFrom png readPNG
#' @export
#' @examples
#' GetPlayerPicture('201935', source = 'NBA')

GetPlayerPicture <- function(player, player.ids = NA) {
  
  player <- PlayerNameToID(player, player.ids = player.ids)
  
  url <- gsub('###', player, 'http://stats.nba.com/media/players/230x185/###.png')
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  pic <- readPNG(temp)
  file.remove(temp)
  
  return(pic)
}