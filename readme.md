## sportsTools

This is an R package for scraping sports related data. Currently, most of the implemented functionality is for scraping NBA data.

# Installation and Usage

There is no version of sportsTools on CRAN. To get the current development version from Github:

```R
install.packages("devtools")    # If you don't have devtools already installed
devtools::install_github("ccagrawal/sportsTools")
library(sportsTools)
```

# Functionality

New functionality is constantly implemented in no particular order. Currently, I implement something whenever I need it for analysis. Below is a list of current functionality along with links to examples of data that is scraped:

- Find NBA player seasons fitting certain criteria [(Source: Basketball-Reference)](http://www.basketball-reference.com/play-index/psl_finder.cgi?lid=header_pi)
- Get information on NBA Games
    - Box Scores [(Source: Basketball-Reference)](http://www.basketball-reference.com/boxscores/201510270ATL.html)
    - Play by Play [(Source: NBA.com)](http://stats.nba.com/game/#!/0021500884/playbyplay/)
- Get historical NBA and NFL betting lines [(Source: Covers)](http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/nba/teams/pastresults/2015-2016/team404029.html)
- Get pictures of NBA players [(Source: NBA.com)](http://stats.nba.com/media/players/230x185/201588.png)
- Get stats specific to a particular NBA player
    - On-off differential [(Source: Basketball-Reference)](http://www.basketball-reference.com/players/h/hardeja01/on-off/2016/)
    - Per game [(Source: Basketball-Reference)](http://www.basketball-reference.com/players/h/hardeja01.html)
    - Totals [(Source: Basketball-Reference)](http://www.basketball-reference.com/players/h/hardeja01.html)
    - Advanced [(Source: Basketball-Reference)](http://www.basketball-reference.com/players/h/hardeja01.html)
    - Play by play [(Source: Basketball-Reference)](http://www.basketball-reference.com/players/h/hardeja01.html)
    - Shooting  [(Source: Basketball-Reference)](http://www.basketball-reference.com/players/h/hardeja01/shooting/2016/)
- Get all NBA player stats
    - Totals [(Source: Basketball-Reference)](http://www.basketball-reference.com/leagues/NBA_2016_totals.html) [(Source: NBA.com)](http://stats.nba.com/leaders/#!/?Season=2015-16&SeasonType=Regular%20Season&PerMode=Totals)
    - Per game [(Source: Basketball-Reference)](http://www.basketball-reference.com/leagues/NBA_2016_per_game.html)
    - Advanced [(Source: Basketball-Reference)](http://www.basketball-reference.com/leagues/NBA_2016_advanced.html)
- Get schedule and results
    - NBA [(Source: Basketball-Reference)](http://www.basketball-reference.com/leagues/NBA_2016_games.html)
    - NCAA Basketball [(Source: Sports-Reference)](http://www.sports-reference.com/cbb/schools/texas/2016-schedule.html)
- Get NBA player's shot dashboard [(Source: NBA.com)](http://stats.nba.com/player/#!/203081/tracking/shots/)
- Get NBA Synergy stats [(Source: NBA.com)](http://stats.nba.com/playtype/#!/isolation/?dir=1&PT=player&OD=offensive)
- Get NBA Tracking stats [(Source: NBA.com)](http://stats.nba.com/tracking/#!/player/catchshoot/)
- Get pictures of NBA teams [(Source: ESPN)](http://a.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/HOU.png)
- Get stats specific to a particular NBA team
    - Basic gamelog [(Source: Basketball-Reference)](http://www.basketball-reference.com/teams/POR/2015/gamelog/)
    - Advanced gamelog [(Source: Basketball-Reference)](http://www.basketball-reference.com/teams/POR/2015/gamelog/)
    - Roster [(Source: Basketball-Reference)](http://www.basketball-reference.com/teams/POR/2015.html#all_roster)
- Get NBA team stats
    - Basic [(Source: Basketball-Reference)](http://www.basketball-reference.com/leagues/NBA_2016.html) [(Source: NBA.com)](http://stats.nba.com/league/team/#!/)
    - Opponent [(Source: Basketball-Reference)](http://www.basketball-reference.com/leagues/NBA_2016.html)
    - Advanced [(Source: Basketball-Reference)](http://www.basketball-reference.com/leagues/NBA_2016.html)
    - Shooting [(Source: Basketball-Reference)](http://www.basketball-reference.com/leagues/NBA_2016.html)
    - Shooting opponent [(Source: Basketball-Reference)](http://www.basketball-reference.com/leagues/NBA_2016.html)
    - Standings [(Source: Basketball-Reference)](http://www.basketball-reference.com/leagues/NBA_2016.html)

# Sources

Data is scraped from the following great sources:

- [Basketball-Reference](http://www.basketball-reference.com/)
- [NBA.com](http://stats.nba.com/)
- [Sports-Reference](http://www.sports-reference.com/cbb/)
- [Covers](http://www.covers.com/index.aspx)
- [ESPN](http://espn.go.com/)
