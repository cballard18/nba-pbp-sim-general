#' nbasim: League-average NBA possession simulator
#'
#' Monte Carlo NBA game simulator built from a chain of small models (timeout, outcome,
#' duration) fit to 2016-17 through 2025-26 play-by-play. Start with [sim_pbp()],
#' [sim_games()], [sim_from_state()] and [win_prob()].
#'
#' @keywords internal
#' @import data.table
#' @importFrom xgboost xgb.load xgb.model.parameters<-
#' @importFrom stats predict runif rbinom
#' @importFrom utils globalVariables
"_PACKAGE"

.datatable.aware <- TRUE

utils::globalVariables(c("game", "clock", "label", "offense", "event", "fta", "ftm", "rebound",
                         "path", "pts1", "pts2", "win", "share"))
