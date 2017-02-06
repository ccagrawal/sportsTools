ToSeconds <- function(x) {
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x) <= 0) return(x)
  
  unlist(
    lapply(x,
      function(i) {
       i <- as.numeric(strsplit(i, ':', fixed = TRUE)[[1]])
       if (length(i) == 3) 
         i[1] * 3600 + i[2] * 60 + i[3]
       else if (length(i) == 2) 
         i[1] * 60 + i[2]
       else if (length(i) == 1) 
         i[1]
      }
    )
  )
}

CapLetters <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = '', collapse = ' ')
}

YearToSeason <- function(x) {
  paste0(x - 1, '-', substring(x, 3, 4))
}

SeasonToYear <- function(x) {
  as.numeric(substring(x, 1, 4)) + 1
}

CurrentYear <- function() {
  if (as.numeric(format(Sys.Date(), "%m")) >= 11) {
    return(as.numeric(format(Sys.Date(), "%Y")) + 1)
  } else {
    return(as.numeric(format(Sys.Date(), "%Y")))
  }
}

SecondsElapsed <- function(period, timestamp) {
  seconds <- rep(0, length(period))
  seconds[period <= 4] <- 12 * 60 * period[period <= 4]
  seconds[period > 4] <- 12 * 60 * 4 + 5 * 60 * (period[period > 4] - 4)
  seconds.remaining <- sapply(strsplit(timestamp, ":"), function(x) as.numeric(x[1]) * 60 + as.numeric(x[2]))
  return(seconds - seconds.remaining)
}

ContentToDF <- function(content) {
  data <- content$rowSet
  data <- lapply(data, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  data <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE)) # Turn list to data frame
  
  if (length(content$headers) == ncol(data)) {
    colnames(data) <- content$headers
    
  } else { # Multiple levels of headers
    headers <- lapply(colnames(data), function(x) c())
    
    for (col.level in 1:length(content$headers)) {
      col.names <- content$headers[[col.level]]
      
      if ('columnsToSkip' %in% names(col.names)) {
        start <- col.names$columnsToSkip + 1
      } else {
        start <- 1
      }
      span <- col.names$columnSpan
      
      col.ix <- 1
      for (i in seq(from = start, to = ncol(data), by = span)) {
        for (j in i:(i + span - 1)) {
          headers[[j]] <- c(headers[[j]], col.names$columnNames[[col.ix]][1])
        }
        col.ix <- col.ix + 1
      }
    }
    
    colnames(data) <- sapply(headers, function(x) paste(x, collapse = '.'))
  }
  
  return(data)
}

CleanParam <- function(param) {
  
  if (param == 'Basic') {
    return('Base')
  } else if (param == '100 Possessions') {
    return('Per100Possessions')
  } else if (param == 'Per Game') {
    return('PerGame')
  } else {
    return(param)
  }
}

CHARACTER.COLUMNS <- c('GROUP_SET', 'PLAYER_ID', 'PLAYER_NAME', 'TEAM_ID', 'TEAM_NAME', 'TEAM_ABBREVIATION', 'CFID', 'CFPARAMS',
                       'Team_ID', 'Game_ID', 'GAME_DATE', 'MATCHUP', 'WL',
                       'SHOT_TYPE', 'SHOT_CLOCK_RANGE', 'DRIBBLE_RANGE',
                       'CLOSE_DEF_DIST_RANGE', 'TOUCH_TIME_RANGE', 'PLAYER_NAME_LAST_FIRST')