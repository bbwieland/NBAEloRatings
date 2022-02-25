## Importing packages, setting working directory ----
## the working directory is customizable depending on personal preferences

## elo.model.builder function: ----
## this function takes a year argument
## it returns an Elo model for that NBA season

#' @export
elo.model.builder <- function(year,throughDate = Sys.Date()) {
  Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
  season22 <- nbastatR::game_logs(seasons = year, season_types = "Regular Season")

  test <- season22 %>% group_by(idGame) %>% summarise(winner = slugTeamWinner,
                                                      loser = slugTeamLoser,
                                                      date = dateGame)

  test <- test %>% filter(date <= throughDate)



  test2 <- test[order(test$idGame,decreasing=TRUE),]
  test2 <- test[!duplicated(test$idGame),]
  elo_results <- elo.seq(winner = test2$winner,
                         loser = test2$loser,
                         Date = test2$date,
                         runcheck = F, k = 25)
  elo_results
}

## elo.by.team function: ----
## takes an Elo model argument
## returns a dataframe of each team with its Elo

#' @export
elo.by.team <- function(elo.model) {
  elo_by_team <- data.frame(elo = extract_elo(elo.model))
  elo_by_team <- elo_by_team %>% mutate(Abbreviation = rownames(elo_by_team))
  rownames(elo_by_team) = NULL
  elo_by_team
}

## team.info.table and overall.table function: ----
## This function takes a year as the argument, like elo.table.builder
## It returns a dataframe

#' @export
team.info.table <- function(year){

  site <- read_html("https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations")
  Abbreviation <- site %>% html_nodes("tr+ tr td:nth-child(1)") %>% html_text()
  Team <- site %>% html_nodes("tr+ tr td+ td") %>% html_text()
  team_names <- data.frame(Abbreviation,Team) %>% mutate(Abbreviation = gsub("\n","",Abbreviation),
                                                         Team = gsub("\n","",Team))

  standings <- nbastatR::standings(seasons = year) %>%
    select(slugTeam,nameTeam,nameConference,nameDivison,pctWinTeam,recordOverall) %>%
    rename(Abbreviation = slugTeam, Team = nameTeam, Record = recordOverall,
           Conference = nameConference, Division = nameDivison, WinPct = pctWinTeam)
  standings[13,1] = "LAC"
  standings[8,1] = "DEN"

  team_info <- inner_join(team_names,standings,by = "Abbreviation") %>%
    select(-Team.x) %>%
    rename(Team = Team.y)

  team_info
}

## This function takes an elo table and a team info table as an argument
## It combines them into one overall table which can be used for plotting/tables

#' @export
overall.table <- function(elo.table,info.table) {
  newtable <- inner_join(elo.table,info.table)
  newtable
}

## The elo.x.generator family of functions ----
## elo.table.generator = returns a table of all 30 NBA teams and their Elo
## elo.standings.generator = returns a standings-like table subsetted by division and conference
## elo.conference.generator = returns a table subsetted by just conference

#' @export
elo.table.generator <- function(overall.table){
  overall.table <- overall.table %>%
    select(Abbreviation,Team,Conference,Division,WinPct,Record,elo)

  gt(overall.table) %>%
    tab_header(title = md("**NBA Elo Rating Model**"),
               subtitle = md("*Data scraped using nbastatR*")) %>%
    data_color(columns = c(WinPct,elo),colors = brewer.pal(5,"RdYlGn")) %>%
    tab_source_note(source_note = paste0("Date generated: ",Sys.Date())) %>%
    espnscrapeR::gt_theme_538()
}

#' @export
elo.standings.generator <- function(overall.table){
  overall.table <- overall.table %>%
    select(Abbreviation,Team,Conference,Division,WinPct,Record,elo) %>%
    group_by(Conference,Division)

  gt(overall.table) %>%
    tab_header(title = md("**NBA Elo Rating Model**"),
               subtitle = md("*Data scraped using nbastatR*")) %>%
    data_color(columns = c(WinPct,elo),colors = brewer.pal(5,"RdYlGn")) %>%
    tab_source_note(source_note = paste0("Date generated: ",Sys.Date())) %>%
    espnscrapeR::gt_theme_538()
}

#' @export
elo.conference.generator <- function(overall.table){
  overall.table <- overall.table %>%
    select(Abbreviation,Team,Conference,Division,WinPct,Record,elo) %>%
    group_by(Conference)

  gt(overall.table) %>%
    tab_header(title = md("**NBA Elo Rating Model**"),
               subtitle = md("*Data scraped using nbastatR*")) %>%
    data_color(columns = c(WinPct,elo),colors = brewer.pal(5,"RdYlGn")) %>%
    tab_source_note(source_note = paste0("Date generated: ",Sys.Date())) %>%
    espnscrapeR::gt_theme_538()

}

## The elo.prediction function ----
## this takes two team Elo ratings as the inputs and returns the probability of team 1 winning

#' @export
elo.prediction <- function(elorating1,elorating2) {
  team1.odds <- 1/(10 ** (-(elorating1 - elorating2) / 400) + 1)
  team1.odds
}

#' @export
current.day.prediction <- function(elo.table,date = Sys.Date()){

  Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
  home.teams <- nbastatR::current_schedule() %>%
    select(dateGame,idGame,slugTeamHome) %>%
    filter(dateGame == date) %>%
    inner_join(elo.table,by = c("slugTeamHome" = "Abbreviation")) %>%
    rename(HomeTeam = slugTeamHome, HomeElo = elo)

  away.teams <- nbastatR::current_schedule() %>%
    select(dateGame,idGame,slugTeamAway) %>%
    filter(dateGame == date) %>%
    inner_join(elo.table,by = c("slugTeamAway" = "Abbreviation")) %>%
    rename(AwayTeam = slugTeamAway, AwayElo = elo)

  day.games <- inner_join(home.teams,away.teams,by = "idGame") %>%
    mutate(HomeWP = round(elo.prediction(HomeElo,AwayElo),3),
           AwayWP = round(elo.prediction(AwayElo,HomeElo),3)) %>%
    rename(dateGame = dateGame.x) %>% select(-dateGame.y) %>%
    mutate(WinnerPick = ifelse(HomeWP >= .5,HomeTeam,AwayTeam)) %>%
    mutate(HCAHomeElo = HomeElo + 50,
           HCAHomeWP = round(elo.prediction(HCAHomeElo,AwayElo),3),
           HCAAwayWP = round(elo.prediction(AwayElo,HCAHomeElo),3),
           HCAWinnerPick = ifelse(HCAHomeWP >= .5,HomeTeam,AwayTeam))

  winner.picks <- day.games %>%
    mutate(Matchup = paste0(AwayTeam," @ ",HomeTeam)) %>%
    select(idGame,Matchup,HCAWinnerPick)

  picks.gt.data <- day.games %>%
    mutate(Matchup = paste0(HomeTeam," vs. ",AwayTeam)) %>%
    select(Matchup,HomeElo,AwayElo,HCAHomeElo,HCAHomeWP,HCAAwayWP,HCAWinnerPick)

  picks.gt <- picks.gt.data %>% gt() %>%
    tab_header(title = md("**NBA Daily Picks**"),
               subtitle = md("*Win probabilities based on Elo rating model inputs*")) %>%
    data_color(columns = c(HCAHomeWP,HCAAwayWP),colors = brewer.pal(5,"RdYlGn")) %>%
    data_color(columns = c(HCAWinnerPick),colors = "lightgrey") %>%
    tab_source_note(source_note = paste0("Date generated: ",Sys.Date())) %>%
    espnscrapeR::gt_theme_538() %>%
    fmt_percent(columns = c(HCAHomeWP,HCAAwayWP),
                decimals = 1) %>%
    cols_label(HomeElo = "Home Team Elo",
               AwayElo = "Away Team Elo",
               HCAHomeElo = "Home Team Elo Home-Court Adjusted",
               HCAHomeWP = "Home Win Probability",
               HCAAwayWP = "Away Win Probability",
               HCAWinnerPick = "Predicted Winner") %>%
    cols_align(
      align = "center",
      columns = everything()
    )

  set <- list(fullpredictions = day.games,picks = winner.picks,picktable = picks.gt)
  set
}


## Malleability ----

#' @export
overallFunctionWrapper = function(year,date = Sys.Date()){

  eloModel = elo.model.builder(year,throughDate = date)

  teamElos = elo.by.team(eloModel)

  teamInfo = team.info.table(year)

  teamElosAndInfo = overall.table(teamElos,teamInfo)

  divisionTable = elo.standings.generator(teamElosAndInfo)

  conferenceTable = elo.conference.generator(teamElosAndInfo)

  standingsTable = elo.table.generator(teamElosAndInfo)

  dailyPredictions = current.day.prediction(teamElos,date = date)

  return(list(TeamElo = teamElos,TeamInfo = teamInfo,TeamTable = teamElosAndInfo,
              OverallStandings = standingsTable,ConferenceStandings = conferenceTable,
              DivisionStandings = divisionTable,DailyPicks = dailyPredictions))
}
