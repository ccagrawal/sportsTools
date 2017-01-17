#' Get Game Info.
#'
#' @param id Game ID that matches the source
#' @param info desired information ('box scores', 'summary, 'lineups', 'play by play', 'advanced')
#' @return list of information requested
#' @keywords boxscore
#' @importFrom httr GET content add_headers
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' GetGameInfo('0021600425', c('box score'))

GetGameInfo <- function(id, info = c('box score')) {
  
  options(stringsAsFactors = FALSE)
  results <- list()
    
  if ('play by play' %in% info) {
    results$play.by.play <- .GetPlayByPlay(id)
  }
  
  if ('summary' %in% info) {
    results$summary <- .GetSummary(id)
  }
  
  if ('lineups' %in% info) {
    results$lineups <- .GetLineups(id)
  }
  
  if ('box score' %in% info) {
    results$box.score <- .GetBoxScore(id)
  }
  
  if ('advanced' %in% info) {
    results$advanced <- .GetAdvanced(id)
  }
  
  return(results)
}

# Input:    Game ID (ex. '0021300359')
# Output:   Data frame with box score from NBA.com
.GetSummary <- function(id) {
  
  request <- GET(
    "http://stats.nba.com/stats/boxscoresummaryv2",
    query = list(
      GameID = id
    ),
    add_headers('Referer' = 'http://stats.nba.com/game/')
  )
  
  content <- content(request, 'parsed')[[3]][[6]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  pts.cols <- colnames(stats)[grepl('PTS', colnames(stats))]
  stats[, pts.cols] <- sapply(stats[, pts.cols], as.numeric)
  num.periods <- sum(colSums(stats[, pts.cols] == 0) != 2) - 1
  stats <- stats[, c('GAME_ID', 'TEAM_ID', 'TEAM_ABBREVIATION', 'TEAM_CITY_NAME', 'TEAM_NICKNAME', 'PTS')]
  stats$PERIODS <- num.periods
  
  # Get home and away teams
  extra.info <- ContentToDF(content(request, 'parsed')[[3]][[1]])
  home.team <- extra.info[1, 'HOME_TEAM_ID']
  stats$LOCATION <- 'Away'
  stats[stats$TEAM_ID == home.team, 'LOCATION'] <- 'Home'
  
  return(stats)
}

# Input:    Game ID (ex. '0021300359')
#           start.period - starting quarter
#           end.period - ending quarter
# Output:   Data frame with box score from NBA.com
.GetBoxScore <- function(id, start.period = 1, end.period = 14) {
  
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
  stats <- ContentToDF(content)
  
  # Clean data frame
  stats[, 10:28] <- sapply(stats[, 10:28], as.numeric)
  
  return(stats)
}

# Input:    Game ID (ex. '0021300359')
# Output:   Data frame with play-by-play breakdown from NBA.com
.GetPlayByPlay <- function(id) {
  
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
  actions <- ContentToDF(content)
  
  # Clean data frame
  actions[, c(2:5, 13, 20, 27)] <- sapply(actions[, c(2:5, 13, 20, 27)], as.numeric)
  
  return(actions)
}

# Input:    Game ID (ex. '0021300359')
#           start.period - starting quarter
#           end.period - ending quarter
# Output:   Data frame with advanced stats from NBA.com
.GetAdvanced <- function(id, start.period = 1, end.period = 14) {
  
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
  
  # Return NA if empty (game isn't done yet)
  if (length(content$rowSet) == 0) {
    return(NA)
  }
  
  player.stats <- ContentToDF(content)
  
  content <- content(request, 'parsed')[[3]][[2]]
  team.stats <- ContentToDF(content)
  
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
.GetLineups <- function(id) {
  
  # Get play by play first
  pbp <- .GetPlayByPlay(id)
  
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
  home.players <- .ProcessSubs(pbp, markers, mark.periods, ranges, 'Home')
  away.players <- .ProcessSubs(pbp, markers, mark.periods, ranges, 'Away')
  
  # Fill in missing players using box score
  for (i in 1:(length(mark.periods) - 1)) {
    
    # Get box score for period and split into home and away
    box.score <- .GetBoxScore(id, start.period = i, end.period = i)
    
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
.ProcessSubs <- function(pbp, markers, mark.periods, ranges, team) {

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