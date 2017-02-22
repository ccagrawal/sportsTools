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
    
  } else if (param == 'Per 36 Minutes') {
    return('Per36')
    
  } else if (param == 'Offensive') {
    return('offensive')
    
  } else if (param == 'Defensive') {
    return('defensive')
    
  } else if (param == 'Player') {
    return('player')
    
  } else if (param == 'Team') {
    return('team')
    
  } else {
    return(param)
  }
}

CHARACTER.COLUMNS <- c('GROUP_SET', 'GROUP_ID', 'GROUP_NAME', 'PLAYER_ID', 'PLAYER_NAME', 'TEAM_ID', 'TEAM_NAME', 'TEAM_ABBREVIATION', 
                       'CFID', 'CFPARAMS', 'Team_ID', 'Game_ID', 'GAME_DATE', 'MATCHUP', 'WL',
                       'SHOT_TYPE', 'SHOT_CLOCK_RANGE', 'DRIBBLE_RANGE', 'VS_PLAYER_ID', 'VS_PLAYER_NAME', 'COURT_STATUS',
                       'CLOSE_DEF_DIST_RANGE', 'TOUCH_TIME_RANGE', 'PLAYER_NAME_LAST_FIRST', 'CLOSE_DEF_PERSON_ID', 
                       'PlayerIDSID', 'PlayerFirstName', 'PlayerLastName', 'P', 'TeamIDSID', 'TeamName', 
                       'TeamNameAbbreviation', 'TeamShortName', 'name', 'seasonType', 'GROUP_VALUE',
                       'PLAYER_LAST_TEAM_ID', 'PLAYER_LAST_TEAM_ABBREVIATION', 'PLAYER_POSITION', 'DEFENSE_CATEGORY',
                       'GAME_ID', 'TEAM_CITY', 'START_POSITION', 'COMMENT', 'SEASON_ID', 'GAME_ID')

PlayerNameToID <- function(player, year = CurrentYear(), player.ids = NA) {
  
  # If player name was provided, get player ID
  if (is.na(as.numeric(player))) {
    if (is.na(player.ids))  {
      player.ids <- GetPlayerIDs(year = year)
    }
    player <- player.ids[which(player.ids$DISPLAY_FIRST_LAST == player), 'PERSON_ID']
  }
  
  return(player)
}

TeamNameToID <- function(team, year = CurrentYear(), team.ids = NA) {
  
  # If team name was provided, get team ID
  if (is.na(as.numeric(team))) {
    if (is.na(team.ids)) {
      team.ids <- GetTeamIDs(year = year)
    }
    team <- team.ids[which(team.ids$name == team), 'id']
  }
  
  return(team)
}