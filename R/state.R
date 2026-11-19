STAT_NAMES <- c("poss", "segments", "fg2a", "fg3a", "fga_made", "fta", "tov", "oreb", "timeouts", "fouls")

#' Describe a game situation
#'
#' Builds a game state from the point of view of the team with the ball, which is team 1 in
#' all simulation output.
#'
#' @param period Period (1-4, 5+ for overtime).
#' @param secs_left Seconds left in the period.
#' @param off_score,def_score Scores of the team with the ball and its opponent.
#' @param start_type How the possession began: `"period_start"`, `"after_make"`,
#'   `"after_dreb"`, `"after_oreb"`, `"after_turnover"` or `"sideout"`.
#' @param off_fouls,def_fouls Team fouls this period.
#' @param off_fouls_l2,def_fouls_l2 Team fouls in the last 2:00 of the period.
#' @param off_to_left,def_to_left Timeouts left. Default: the most the rules allow.
#' @param tip_off Did the team with the ball win the opening tip? (Sets who starts Q2-Q4.)
#' @return A one-row data.table.
#' @examples
#' game_state(period = 4, secs_left = 40, off_score = 101, def_score = 103)
#' @export
game_state <- function(period, secs_left, off_score = 0, def_score = 0, start_type = "after_dreb",
                       off_fouls = 0, def_fouls = 0, off_fouls_l2 = 0, def_fouls_l2 = 0,
                       off_to_left = NULL, def_to_left = NULL, tip_off = TRUE) {
  stopifnot(start_type %in% START_TYPES)
  max_to <- fcase(period <= 3, 7, period == 4 & secs_left <= 180, 2, period == 4, 4, default = 2)
  data.table(period = period, secs_left = secs_left, off_score = off_score, def_score = def_score,
             score_diff = off_score - def_score, start_type = start_type,
             off_fouls = off_fouls, def_fouls = def_fouls, off_fouls_l2 = off_fouls_l2,
             def_fouls_l2 = def_fouls_l2, off_to_left = if (is.null(off_to_left)) max_to else off_to_left,
             def_to_left = if (is.null(def_to_left)) max_to else def_to_left, tip_off = tip_off)
}

#' Set up games for [simulate_games()]
#'
#' `new_games()` creates `n` games at tip-off. `games_from_states()` creates games from game
#' states (rows from [game_state()]), each repeated `reps` times; team 1 has the ball.
#'
#' @param n Number of games.
#' @param s A data.table of game states.
#' @param reps Copies of each state.
#' @return A list holding the state of every game.
#' @export
new_games <- function(n) {
  tip <- sample(1:2, n, replace = TRUE)
  list(period = rep(1L, n), secs_left = rep(720, n), off = tip, tip = tip,
       start_type = rep("period_start", n), done = rep(FALSE, n),
       score = matrix(0, n, 2), fouls = matrix(0, n, 2), fouls_l2 = matrix(0, n, 2),
       to_left = matrix(7, n, 2),
       stats = matrix(0, n, length(STAT_NAMES), dimnames = list(NULL, STAT_NAMES)))
}

#' @rdname new_games
#' @export
games_from_states <- function(s, reps = 1) {
  s <- s[rep(seq_len(nrow(s)), each = reps)]
  n <- nrow(s)
  score <- if ("off_score" %in% names(s)) cbind(s$off_score, s$def_score) else cbind(s$score_diff, 0)
  list(period = s$period, secs_left = s$secs_left, off = rep(1L, n),
       tip = fifelse(s$tip_off, 1L, 2L), start_type = s$start_type, done = rep(FALSE, n),
       score = score, fouls = cbind(s$off_fouls, s$def_fouls),
       fouls_l2 = cbind(s$off_fouls_l2, s$def_fouls_l2), to_left = cbind(s$off_to_left, s$def_to_left),
       stats = matrix(0, n, length(STAT_NAMES), dimnames = list(NULL, STAT_NAMES)))
}
