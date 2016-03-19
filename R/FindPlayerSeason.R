#' Player Season Finder
#'
#' @param type either 'Advanced' or 'Per Game'
#' @param year.min Earliest year to search
#' @param season.start Earliest year of players' careers to search
#' @param season.end Latest year of players' careers to search
#' @param c1.stat Statistic for criteria 1
#' @param c1.val Value for criteria 1
#' @param c2.stat Statistic for criteria 2
#' @param c2.val Value for criteria 2
#' @param c3.stat Statistic for criteria 3
#' @param c3.val Value for criteria 3
#' @param order.by Statistic to order results by
#' @return data frame with player seasons that fit criteria
#' @keywords finder player season
#' @importFrom XML readHTMLTable
#' @export
#' @examples
#' FindPlayerSeason(type = 'Advanced', year.min = 1980, season.start = 1, season.end = 1, c1.stat = 'mp_per_g', c1.val = 24, order.by = 'ts_pct')

FindPlayerSeason <- function(type = 'Advanced', year.min = '', season.start = 1, season.end = -1, 
                             c1.stat = '', c1.val = '', c2.stat = '', c2.val = '', c3.stat = '', c3.val = '', order.by = '') {
  
  options(stringsAsFactors = FALSE)
  
  if (type == 'Advanced') {
    type <- 'advanced'
  } else if (type == 'Per Game') {
    type <- 'per_game'
  }
  
  search.url <- paste0('http://www.basketball-reference.com/play-index/psl_finder.cgi?',
                       'request=1&',
                       'match=single&',
                       'per_minute_base=36&',
                       'per_poss_base=100&',
                       'type=', type, '&',
                       'lg_id=NBA&',
                       'is_playoffs=N&',
                       'year_min=', year.min, '&',
                       'year_max=&',
                       'franch_id=&',
                       'season_start=', season.start, '&',
                       'season_end=', season.end, '&',
                       'age_min=0&',
                       'age_max=99&',
                       'height_min=0&',
                       'height_max=99&',
                       'shoot_hand=&',
                       'birth_country_is=Y&',
                       'birth_country=&',
                       'birth_state=&',
                       'college_id=&',
                       'draft_year=&',
                       'is_active=&',
                       'debut_yr_nba_start=&',
                       'debut_yr_nba_end=&',
                       'debut_yr_aba_start=&',
                       'debut_yr_aba_end=&',
                       'is_hof=&',
                       'is_as=&',
                       'as_comp=gt&',
                       'as_val=&',
                       'award=&',
                       'pos_is_g=Y&',
                       'pos_is_gf=Y&',
                       'pos_is_f=Y&',
                       'pos_is_fg=Y&',
                       'pos_is_fc=Y&',
                       'pos_is_c=Y&',
                       'pos_is_cf=Y&',
                       'qual=&',
                       'c1stat=', c1.stat, '&',
                       'c1comp=gt&',
                       'c1val=', c1.val, '&',
                       'c2stat=', c2.stat, '&',
                       'c2comp=gt&',
                       'c2val=', c2.val, '&',
                       'c3stat=', c3.stat, '&',
                       'c3comp=gt&',
                       'c3val=', c3.val, '&',
                       'c4stat=&',
                       'c4comp=gt&',
                       'c4val=&',
                       'c5stat=&',
                       'c5comp=gt&',
                       'c6mult=1.0&',
                       'c6stat=&',
                       'order_by=', order.by, '&',
                       'order_by_asc=&',
                       'offset=')
  
  df <- data.frame()
  continue <- TRUE    # Marker to track when we've finished going through the results
  offset <- 0         # BBall-Ref only lets you get 100 at a time
  
  # Keep going through each page and getting 100 results at a time
  while(continue) {
    url <- paste0(search.url, offset)
    tables <- readHTMLTable(url)
    
    if (length(tables) == 3) {
      df <- rbind(df, tables[[3]])
      offset <- offset + 100
    } else {
      continue <- FALSE
    }
  }
  
  df <- df[-which(df$Rk == 'Rk'), ]                       # Remove rows with just extra headers
  df$Player <- gsub('\\*', '', df$Player)                 # Remove asterisk from player names
  df$Season <- as.numeric(substr(df$Season, 1, 4)) + 1    # Convert season from "2007-08" to 2008
  
  # Convert non character columns into numeric columns
  char.cols <- c('Player', 'Tm', 'Lg')
  df[, -which(colnames(df) %in% char.cols)] <- lapply(df[, -which(colnames(df) %in% char.cols)], as.numeric)
  
  return(df)
}