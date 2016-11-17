#' Get Game Info.
#'
#' @param id Game ID that matches the source
#' @param source either 'Basketball-Reference' or 'NBA'
#' @param info desired information ('box scores', 'team scores', 'lineups', 'four factors', 'play by play', 'advanced')
#' @return list of information requested
#' @keywords boxscore
#' @importFrom XML readHTMLTable
#' @importFrom httr GET content add_headers
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' GetGameInfo('201004170ATL', 'Basketball-Reference', c('box scores'))

GetGameInfo <- function(id, source = 'Basketball-Reference', info = c('box scores')) {
  
  options(stringsAsFactors = FALSE)
  results <- list()
  
  if (source == 'Basketball-Reference') {
    url <- paste('http://www.basketball-reference.com/boxscores/', id, '.html', sep = '')
    tables <- readHTMLTable(url)
    table.num <- which(names(tables) == 'four_factors')
    
    if ('box scores' %in% info) {
      temp <- tables[[table.num + 3]]
      temp <- temp[grep(":", temp$MP),]   # Remove reserve line and DNPs
      results$home.box <- temp
      
      temp <- tables[[table.num + 1]]
      temp <- temp[grep(":", temp$MP),]   # Remove reserve line and DNPs
      results$away.box <- temp
    }
    
    if ('team scores' %in% info) {
      temp <- tables[[table.num - 1]]
      colnames(temp) <- temp[1, ]         # First row contains column names
      colnames(temp)[1] <- 'Team'         # Manually add team column name
      temp <- temp[-1,]                   # Remove row with column names
      results$team.scores <- temp
    }
    
    if ('lineups' %in% info) {
      results$lineups <- .GetBRefLineups(id)
    }
    
    if ('four factors' %in% info) {
      temp <- tables[[table.num]]
      temp[, -1] <- sapply(temp[, -1], as.numeric)
      results$four.factors <- temp
    }
  } else if (source == 'NBA') {
    
    if ('play by play' %in% info) {
      results$play.by.play <- .GetNBAPlayByPlay(id)
    }
    
    if ('lineups' %in% info) {
      results$lineups <- .GetNBALineups(id)
    }
    
    if ('box score' %in% info) {
      results$box.score <- .GetNBABoxScore(id)
    }
    
    if ('advanced' %in% info) {
      results$advanced <- .GetNBAAdvanced(id)
    }
  }
  
  return(results)
}

# Input:    Game ID (ex. '0021300359')
# Output:   Data frame with play-by-play breakdown from NBA.com
.GetNBAPlayByPlay <- function(id) {
  
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

# Input:    Game ID (ex. '0021300359')
#           start.period - starting quarter
#           end.period - ending quarter
# Output:   Data frame with box score from NBA.com
.GetNBABoxScore <- function(id, start.period = 1, end.period = 14) {
  
  request <- GET(
    "http://stats.nba.com/stats/boxscoretraditionalv2",
    query = list(
      EndPeriod = end.period,
      EndRange = 28800,
      GameID = id,
      RangeType = 1,
      Season = "",
      SeasonType = "",
      StartPeriod = start.period,
      StartRange = 0
    ),
    add_headers('Referer' = 'http://stats.nba.com/game/')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- content$rowSet
  
  # Create raw data frame
  stats <- lapply(stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  stats <- data.frame(matrix(unlist(stats), nrow = length(stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(stats) <- content$headers
  
  # Clean data frame
  stats[, 10:28] <- sapply(stats[, 10:28], as.numeric)
  
  return(stats)
}

# Input:    Game ID (ex. '0021300359')
#           start.period - starting quarter
#           end.period - ending quarter
# Output:   Data frame with advanced stats from NBA.com
.GetNBAAdvanced <- function(id, start.period = 1, end.period = 14) {
  
  request <- GET(
    "http://stats.nba.com/stats/boxscoreadvancedv2",
    query = list(
      EndPeriod = end.period,
      EndRange = 28800,
      GameID = id,
      RangeType = 1,
      Season = "",
      SeasonType = "",
      StartPeriod = start.period,
      StartRange = 0
    ),
    add_headers('Referer' = 'http://stats.nba.com/game/')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  player.stats <- content$rowSet
  
  # Create raw data frame
  player.stats <- lapply(player.stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  player.stats <- data.frame(matrix(unlist(player.stats), nrow = length(player.stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(player.stats) <- content$headers
  
  content <- content(request, 'parsed')[[3]][[2]]
  team.stats <- content$rowSet
  
  # Create raw data frame
  team.stats <- lapply(team.stats, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  team.stats <- data.frame(matrix(unlist(team.stats), nrow = length(team.stats), byrow = TRUE)) # Turn list to data frame
  
  # Get column headers
  colnames(team.stats) <- content$headers
  
  # Clean data frame
  stats <- bind_rows(player.stats, team.stats)
  stats <- stats[, -which(colnames(stats) == 'TEAM_NAME')]
  
  char.cols <- c('GAME_ID', 'PLAYER_ID', 'PLAYER_NAME', 'TEAM_ID', 'TEAM_ABBREVIATION', 'TEAM_CITY', 'START_POSITION', 'COMMENT', 'MIN')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}

# Input:    NBA Game ID (ex. '0021300359')
# Output:   Data frame with 5 man lineups with play-by-play from NBA.com
.GetNBALineups <- function(id) {
  
  # Get play by play first
  pbp <- .GetNBAPlayByPlay(id)
  
  # Add markers for when the 10 man lineups can change
  mark.periods <- c(1, which(pbp$EVENTMSGTYPE == 13 & pbp$EVENTMSGACTIONTYPE == 0))   # End of period
  markers <- which(pbp$EVENTMSGTYPE == 8 & pbp$EVENTMSGACTIONTYPE == 0)  # Subs
  markers <- markers[order(markers)]
  
  # Group subs based on consecutive actions
  ranges <- list(c(markers[1]))
  for (i in 2:length(markers)) {
    if (markers[i] == ranges[[length(ranges)]][length(ranges[[length(ranges)]])] + 1) {
      ranges[[length(ranges)]] <- c(ranges[[length(ranges)]], markers[i])
    } else {
      ranges[[length(ranges) + 1]] <- markers[i]
    }
  }
  
  # Remove consecutive markers at the same timestamp
  for (i in length(markers):2) {
    if (markers[i] == (markers[i - 1] + 1)) {
      markers <- markers[-i]
    }
  }
  
  # Add periods into markers
  markers <- c(markers, mark.periods)
  markers <- markers[order(markers)]
  
  # Get team IDs
  home.id <- pbp[pbp$EVENTMSGTYPE == 8 & pbp$EVENTMSGACTIONTYPE == 0 & !is.na(pbp$HOMEDESCRIPTION), 'PLAYER1_TEAM_ID'][1]
  away.id <- pbp[pbp$EVENTMSGTYPE == 8 & pbp$EVENTMSGACTIONTYPE == 0 & !is.na(pbp$VISITORDESCRIPTION), 'PLAYER1_TEAM_ID'][1]
  
  # Go through substitutions and get preliminary lineups
  home.players <- .ProcessNBASubs(pbp, markers, mark.periods, ranges, 'Home')
  away.players <- .ProcessNBASubs(pbp, markers, mark.periods, ranges, 'Away')
  
  # Fill in missing players using box score
  for (i in 1:(length(mark.periods) - 1)) {
    
    # Get box score for period and split into home and away
    box.score <- .GetNBABoxScore(id, start.period = i, end.period = i)
    
    if (i <= 4) {
      box.score <- box.score[box.score$MIN == '12:00', ]
    } else {
      box.score <- box.score[box.score$MIN == '5:00', ]
    }
    
    home.add <- box.score[box.score$TEAM_ID == home.id, 'PLAYER_ID']
    away.add <- box.score[box.score$TEAM_ID == away.id, 'PLAYER_ID']
    
    # Get range corresponding to period in players list
    group.first <- length(markers[markers <= mark.periods[i]])
    group.last <- length(markers[markers <= mark.periods[i + 1]]) - 1
    
    # If home players haven't played in the quarter, add them to the whole thing
    for (player in home.add) {
      if (!(player %in% .GetPlayers(home.players, group.first, group.last))) {
        home.players <- .AddPlayers(player, home.players, group.first, group.last)
      }
    }
    
    # If away players haven't played in the quarter, add them to the whole thing
    for (player in away.add) {
      if (!(player %in% .GetPlayers(away.players, group.first, group.last))) {
        away.players <- .AddPlayers(player, away.players, group.first, group.last)
      }
    }
  }
  
  pbp[, c('H1', 'H2', 'H3', 'H4', 'H5', 'A1', 'A2', 'A3', 'A4', 'A5')] <- NA
  
  # Add player ids into pbp
  for (i in 1:length(home.players)) {
    length <- markers[i + 1] - markers[i] + 1
    pbp[markers[i]:markers[i + 1], c('H1', 'H2', 'H3', 'H4', 'H5')] <- rep(home.players[[i]], each = length)
    pbp[markers[i]:markers[i + 1], c('A1', 'A2', 'A3', 'A4', 'A5')] <- rep(away.players[[i]], each = length)
  }
  
  return(pbp)
}

# Input:    pbp - NBA play by play data frame
#           markers - rows of pbp that signify changes
#           mark.periods - rows of pbp for start and end of quarters
#           ranges - list of consecutive subs
#           team - either 'Home' or 'Away'
# Output:   List of arrays of players at each time
.ProcessNBASubs <- function(pbp, markers, mark.periods, ranges, team) {

  # Keep track of players that are in the lineup between each marker
  players <- lapply(markers[-1], function(x) NULL)

  # Create act column corresponding to home or away team
  if (team == 'Home') {
    actions <- pbp$HOMEDESCRIPTION
  } else {
    actions <- pbp$VISITORDESCRIPTION
  }

  subs <- grep('SUB:', actions)                               # Find plays with subs
  for (j in 1:length(ranges)) {
    
    # Get subs in that range
    current.subs <- subs[subs %in% ranges[[j]]]
    
    # Only proceed if there was at least 1 sub
    if (length(current.subs) > 0) {
      
      # Get one sub
      i <- current.subs[1]
      
      # Compute the group this sub happened between, and the first and last group of the period
      group <- length(markers[markers <= i]) - 1
      i.last <- min(mark.periods[mark.periods > i])
      group.last <- length(markers[markers <= i.last]) - 1
      i.first <- max(mark.periods[mark.periods <= i])
      group.first <- length(markers[markers <= i.first])
      
      # Get the players subbed in and the players subbed out
      players.in <- pbp[current.subs, 'PLAYER2_ID']
      players.out <- pbp[current.subs, 'PLAYER1_ID']
      
      # Remove players subbed in and out
      overlap <- intersect(players.in, players.out)
      for (player in overlap) {
        players.in <- players.in[-match(player, players.in)]
        players.out <- players.out[-match(player, players.out)]
      }
      
      # Loop through subs
      if (length(players.in) > 0) {
        for (k in 1:length(players.in)) {
          
          player.in <- players.in[k]
          player.out <- players.out[k]
          
          # If we don't have the player that subbed out at all, add him in the whole quarter up to now
          if (!(player.out %in% .GetPlayers(players, group.first, group))) {
            players <- .AddPlayers(player.out, players, group.first, group)
          }
          
          # Remove the player subbed out for the remainder of the quarter, and add the guy subbed in
          players <- .RemovePlayer(player.out, players, group + 1, group.last)
          players <- .AddPlayers(player.in, players, group + 1, group.last)
        }
      }
    }
  }

  return(players)
}

# Input:    Basketball Reference Game ID
# Output:   Data frame with 5 man lineups with play-by-play from Basketball Reference
.GetBRefLineups <- function(id) {
  
  url.pbp <- paste('http://www.basketball-reference.com/boxscores/pbp/', id, '.html', sep = '')
  year <- as.numeric(substr(id, 1, 4))
  player.ids <- GetPlayerIDs(year, source = 'Basketball-Reference')
  
  # Replace names in play by play table with their IDs to solve M. Morris problem
  html <- readLines(url.pbp)
  html <- gsub('<a href=\"/players([^\"]*)\">([^<]*)</a>', '\\1', html)
  tables <- readHTMLTable(html)
  
  # Add column names and remove rows that don't have a timestamp
  pbp <- tables[[length(tables)]]
  colnames(pbp) <- c('time', 'away.action', 'away.change', 'score', 'home.change', 'home.action')
  pbp <- pbp[grep(':', pbp$time), ]
  
  # Make time column represent minutes elapsed in the whole game as a decimal
  pbp$period <- 1
  mark.periods <- grep('End', pbp$away.action)
  
  for (i in 1:(length(mark.periods) - 1)) {
    pbp[(mark.periods[i] + 1):mark.periods[i + 1], 'period'] <- i + 1
  }
  
  pbp$dec.min.rem <- sapply(strsplit(pbp$time, ':'), function(x) as.numeric(x)[1] + as.numeric(x)[2]/60)
  pbp[pbp$period <= 4, 'time'] <- pbp[pbp$period <= 4, 'period'] * 12 - pbp[pbp$period <= 4, 'dec.min.rem']
  pbp[pbp$period > 4, 'time'] <- 48 + (pbp[pbp$period > 4, 'period'] - 4) * 5 - pbp[pbp$period > 4, 'dec.min.rem']
  
  # Add markers for when the 10 man lineups can change
  markers <- c(mark.periods, grep('Start', pbp$away.action), grep('enters', pbp$away.action), grep('enters', pbp$home.action))
  markers <- markers[order(markers)]
  
  # Remove consecutive markers at the same timestamp
  for (i in length(markers):2) {
    if (pbp[markers[i], 'time'] == pbp[markers[i - 1], 'time']) {
      markers <- markers[-i]
    }
  }
  
  # Add an initial mark period signifying start of the first
  mark.periods <- c(1, mark.periods)
  
  # Go through substitutions and get preliminary lineups
  home.players <- .ProcessBRefSubs(pbp, markers, mark.periods, 'home')
  away.players <- .ProcessBRefSubs(pbp, markers, mark.periods, 'away')
  
  # Load up plus minus data for filling in missing players
  plus.minus <- .GetBRefPlusMinus(id)
  plus.minus <- plus.minus[plus.minus$in.game, ]
  plus.minus <- merge(plus.minus, player.ids, by.x = 'player', by.y = 'name')
  
  # Go back and add the players that played an entire period without being subbed
  home.players <- .FinishBRefLineups(plus.minus, home.players, mark.periods, markers, 'home')
  away.players <- .FinishBRefLineups(plus.minus, away.players, mark.periods, markers, 'away')
  
  
}

# Input:    Basketball Reference Game ID
# Output:   Data frame with plus minus information to help complete 5 man lineups
.GetBRefPlusMinus <- function(id) {
  
  url.pm <- paste('http://www.basketball-reference.com/boxscores/plus-minus/', id, '.html', sep = '')  
  html <- readLines(url.pm)
  
  # Only keep the useful stuff from the HTML
  html <- html[grep('large_text margin_top padding_half', html)[1]:grep('<!-- SR js files -->', html)[1]]
  
  # Create empty data frames to signify period changes and player changes
  period.times <- data.frame(matrix(nrow = 20, ncol = 3, data = NA))
  player.times <- data.frame(matrix(nrow = 1000, ncol = 5, data = NA))
  colnames(period.times) <- c('period', 'start', 'stop')
  colnames(player.times) <- c('team', 'player', 'start', 'stop', 'in.game')
  
  # Create "tracker" variables as we iterate through each row in the HTML
  team <- NULL
  player <- NULL
  period <- 1
  periods.done <- FALSE
  period.elapsed <- 0
  player.elapsed <- 0
  period.row <- 1
  player.row <- 1
  
  # Iterate through each line
  for (i in 1:length(html)) {
    
    line <- html[i]
    
    if (grepl('border large_text margin_top padding_half', line)) { # Team line
      if (is.null(team)) {
        team <- 'away'
      } else {
        team <- 'home'
      }
    } else if (grepl('float_left x_small_text', line) & !periods.done) {  # Period line
      width <- as.numeric(gsub('.*width:([0-9]+)p.*', '\\1', line))
      
      period.times[period.row, 'period'] <- period
      period.times[period.row, 'start'] <- period.elapsed
      period.times[period.row, 'stop'] <- period.elapsed + width
      
      period <- period + 1
      period.elapsed <- period.elapsed + width
      period.row <- period.row + 1
      
      if (!(grepl('border_right', line))) {
        periods.done <- TRUE
      }
      
    } else if (grepl('x_small_text"><span class="bold_text">', line)) {  # Player line
      player <- gsub('.*class=\"bold_text\">([^<]*)<.*', '\\1', line)
      player.elapsed <- 0
    } else if (grepl('overflow_hidden x_small_text', line)) {  # +/- line
      width <- as.numeric(gsub('.*width:([0-9]+)p.*', '\\1', line))
      
      player.times[player.row, 'team'] <- team
      player.times[player.row, 'player'] <- player
      player.times[player.row, 'start'] <- player.elapsed
      player.times[player.row, 'stop'] <- player.elapsed + width
      player.times[player.row, 'in.game'] <- grepl('background', line)
      
      player.elapsed <- player.elapsed + width
      player.row <- player.row + 1
    }
  }
  
  # Remove blank rows at the bottom
  period.times <- period.times[complete.cases(period.times), ]
  player.times <- player.times[complete.cases(player.times), ]
  
  # Create low and high "midpoints" in case the player is on the quarter's border
  player.times$midpoint.l <- (player.times$start + player.times$stop)/2 - 5
  player.times$midpoint.h <- (player.times$start + player.times$stop)/2 + 5
  
  # If low and high midpoints don't match, delete the row
  player.times$period <- sapply(player.times$midpoint.l, function(x) sum(period.times$start < x))
  player.times$period.h <- sapply(player.times$midpoint.h, function(x) sum(period.times$start < x))
  player.times <- player.times[player.times$period == player.times$period.h, ]
  
  # Clean up columns
  player.times <- player.times[, c('player', 'team', 'period', 'in.game')]
  colnames(player.times)
  
  return(player.times)
}

# Input:    pbp - Basketball Reference play by play data frame
#           markers - rows of pbp that signify changes
#           mark.periods - rows of pbp for start and end of quarters
#           team - either 'Home' or 'Away'
# Output:   List of arrays of players at each time
.ProcessBRefSubs <- function(pbp, markers, mark.periods, team) {
  
  # Keep track of players that are in the lineup between each marker
  players <- lapply(markers[-1], function(x) NULL)
  
  # Create act column corresponding to home or away team
  if (team == 'home') {
    pbp$act <- pbp$home.action
  } else {
    pbp$act <- pbp$away.action
  }
  
  subs <- grep('enters', pbp$act)
  for (i in subs) {
    
    # Computer the group this sub happened between, and the first and last group of the period
    group <- length(markers[markers <= i]) - 1
    i.last <- min(mark.periods[mark.periods > i])
    group.last <- length(markers[markers <= i.last]) - 1
    i.first <- max(mark.periods[mark.periods <= i])
    group.first <- length(markers[markers <= i.first])
    
    # Get the player subbed in and the player subbed out
    action <- pbp[i, 'act']
    player.in <- gsub('(/[^\\.]*\\.html) enters.*', '\\1', action)
    player.out <- gsub('.*for (/[^\\.]*\\.html)', '\\1', action)
    
    # If we don't have the player that subbed out at all, add him in the whole quarter up to now
    if (!(player.out %in% .GetPlayers(players, group.first, group))) {
      players <- .AddPlayers(player.out, players, group.first, group)
    }
    
    # Remove the player subbed out for the remainder of the quarter, and add the guy subbed in
    players <- .RemovePlayer(player.out, players, group + 1, group.last)
    players <- .AddPlayers(player.in, players, group + 1, group.last)
  }
  
  return(players)
}

# Combine preliminary lineups with the plus/minus lineups
.FinishBRefLineups <- function(plus.minus, lineups, mark.periods, markers, team) {
  
  for (i in 1:(length(mark.periods) - 1)) {
    
    # In each period, get the first and last markers
    i.first <- length(markers[markers <= mark.periods[i]])
    i.last <- length(markers[markers <= mark.periods[i+1]]) - 1
    
    # If we have 5 players, then we're good
    if (length(lineups[[i.first]]) < 5) {
      
      # Make vectors of players that were in during the period
      already.in <- .GetPlayers(lineups, i.first, i.last)
      
      # Get the players in the plus minus data but not in our groups
      pm.in <- plus.minus[plus.minus$team == team & plus.minus$period == i, 'id']
      if (length(already.in) > 0) {
        missing <- pm.in[-which(pm.in %in% already.in)]
      } else {
        missing <- pm.in
      }
      
      # Add missing players
      lineups <- .AddPlayers(missing, lineups, i.first, i.last)
    }
  }
  
  return(lineups)
}

# Get all the players in the lineups between the first and last indices
.GetPlayers <- function(lineups, first, last) {
  players <- c()
  for (i in first:last) {
    players <- union(players, lineups[[i]])
  }
  return(players)
}

# Add all the players to the lineups between the first and last indices
.AddPlayers <- function(players, lineups, first, last) {
  for (i in first:last) {
    lineups[[i]] <- union(lineups[[i]], players)
  }
  return(lineups)
}

# Remove player from the lineups between the first and last indices
.RemovePlayer <- function(player, lineups, first, last) {
  for (i in first:last) {
    if (player %in% lineups[[i]]) {
      lineups[[i]] <- lineups[[i]][lineups[[i]] != player]
    }
  }
  return(lineups)
}