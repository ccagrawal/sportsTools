#' Get Game Info.
#'
#' @param id Game ID that matches the source
#' @param source either 'Basketball-Reference' or 'NBA'
#' @param info desired information ('box scores', 'team scores', 'lineups', 'four factors', 'play by play')
#' @return list of information requested
#' @keywords boxscore
#' @importFrom XML readHTMLTable
#' @importFrom rjson fromJSON
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
  }
  
  return(results)
}

# Input:    Game ID (ex. '0021300359')
# Output:   Data frame with play-by-play breakdown
#           game.id, action, detail, quarter, time,
#           action.home, action.neutral, action.away, score, margin
.GetNBAPlayByPlay <- function(game.id) {
  
  # Create URL and scrape the JSON for the input game
  url <- gsub('###', game.id, 'http://stats.nba.com/stats/playbyplay?GameID=###&StartPeriod=0&EndPeriod=0')
  json <- fromJSON(file = url)[[3]]                       # (3) contains the actual info for the game
  
  # Check if plays exist for the given game
  temp <- json[[1]]                                       # (1) contains play by play data
  plays <- temp[[3]]                                      # (3) contains the actual rows
  
  if (length(plays) > 0) {
    
    # Create raw data frame
    plays <- lapply(plays, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
    plays <- data.frame(matrix(unlist(plays), nrow = length(plays), byrow = TRUE)) # Turn list to data frame
    
    # Clean data frame
    colnames(plays) <- c('game.id', 'event.id', 'action', 'detail', 'quarter', 'real.time', 
                         'time', 'action.home', 'action.neutral', 'action.away', 'score', 'margin')
    
    # Convert columns to proper types
    plays[, c('action', 'detail', 'quarter')] <- sapply(plays[, c('action', 'detail', 'quarter')], as.numeric)
    
    return(plays)
  }
}

# Get 5 man unit
.GetBRefLineups <- function(id) {
  
  url.pbp <- paste('http://www.basketball-reference.com/boxscores/pbp/', id, '.html', sep = '')
  player.ids <- .GetBRefPlayerIDs(id)
  
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

# Get player IDs to help with players with same name (e.g. M. Morris and M. Morris)
.GetBRefPlayerIDs <- function(id) {
  
  url.box <- paste('http://www.basketball-reference.com/boxscores/', id, '.html', sep = '')
  
  html <- readLines(url.box)
  player.urls <- unique(html[grep('href=\"/players/[a-z]', html)])
  player.urls <- player.urls[grep('csk', player.urls)]
  player.urls <- gsub('.*href=\"/players([^\"]*)\">([^<]*).*', '\\1,\\2', player.urls)
  player.ids <- data.frame(matrix(unlist(strsplit(player.urls, ',')), ncol = 2, byrow=T))
  colnames(player.ids) <- c('id', 'name')
  
  return(player.ids)
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

# Process player substitutions and create preliminary lineups
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