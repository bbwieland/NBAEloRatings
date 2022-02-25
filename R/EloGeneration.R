
#' Build an Elo model
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
elo.model.builder <- function(year,throughDate = Sys.Date()) {
  Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
  season22 <- nbastatR::game_logs(seasons = year, season_types = "Regular Season")

  test <- season22 %>% dplyr::group_by(idGame) %>% dplyr::summarise(winner = slugTeamWinner,
                                                                    loser = slugTeamLoser,
                                                                    date = dateGame)

  test <- test %>% dplyr::filter(date <= throughDate)



  test2 <- test[order(test$idGame,decreasing=TRUE),]
  test2 <- test[!duplicated(test$idGame),]
  elo_results <- EloRating::elo.seq(winner = test2$winner,
                                    loser = test2$loser,
                                    Date = test2$date,
                                    runcheck = F, k = 25)
  elo_results
}

#' Get Elo ratings by team
#'
#' Gives Elo ratings by team.
#'
#' @param elo.model A valid Elo model generated using elo.model.builder
#' @return A list of Elo ratings for each NBA team
#' @export
elo.by.team <- function(elo.model) {
  elo_by_team <- data.frame(elo = EloRating::extract_elo(elo.model))
  elo_by_team <- elo_by_team %>% dplyr::mutate(Abbreviation = rownames(elo_by_team))
  rownames(elo_by_team) = NULL
  elo_by_team
}

