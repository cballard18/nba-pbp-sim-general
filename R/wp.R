# Win probability from the neural net ensemble of "NBA-Shot-Optimization" (data-raw/import_wp.R).
# 20 small nets (4 inputs -> 16 relu -> 8 relu -> 1 sigmoid), averaged. Inputs: home margin,
# pregame home spread, seconds left in the game (in OT: in the period), home team has the ball.

# Ensemble mean for a matrix of raw inputs (columns as in nn$inputs)
nn_predict <- function(nn, X) {
  X <- sweep(sweep(X, 2, nn$min), 2, nn$max - nn$min, "/")
  X[] <- pmin(pmax(X, 0), 1)                        # the recipe clips to the training range
  p <- vapply(nn$nets, function(layers) {
    h <- X
    for (k in seq_along(layers)) {
      h <- sweep(h %*% layers[[k]]$W, 2, layers[[k]]$b, "+")
      h <- if (k < length(layers)) pmax(h, 0) else 1 / (1 + exp(-h))
    }
    as.numeric(h)
  }, numeric(nrow(X)))
  rowMeans(matrix(p, nrow(X)))
}

# Team 1's win probability on a neutral court: the average of team 1 as the home team and team 2
# as the home team, with a pregame spread of 0 (the simulator has no home court or team strength)
nn_wp <- function(margin, game_secs, team1_ball, nn) {
  n <- length(margin)
  X <- rbind(cbind(margin, 0, game_secs, as.numeric(team1_ball)),
             cbind(-margin, 0, game_secs, 1 - as.numeric(team1_ball)))
  p <- nn_predict(nn, X)
  (p[seq_len(n)] + 1 - p[n + seq_len(n)]) / 2
}

# Team 1's win probability for games `i` of a simulation: 1/0 once a game is over
state_wp <- function(g, i, game_secs, m) {
  over <- g$over[i] %in% TRUE
  fifelse(over, as.numeric(g$score[i, 1] > g$score[i, 2]),
          nn_wp(g$score[i, 1] - g$score[i, 2], game_secs, g$off[i] == 1, m$wp))
}

#' Neural net win probability for a game state
#'
#' Win probability for the team with the ball from the neural net ensemble of the
#' NBA-Shot-Optimization project, on a neutral court with a pregame spread of 0. It is
#' instant, unlike [win_prob()], which simulates the rest of the game.
#'
#' @param state A [game_state()].
#' @return Win probability for the team with the ball, one per row of `state`.
#' @examples
#' nn_win_prob(game_state(period = 4, secs_left = 40, off_score = 101, def_score = 103))
#' @export
nn_win_prob <- function(state) {
  nn_wp(state$off_score - state$def_score, game_secs_left(state$period, state$secs_left), TRUE,
        nbasim_models()$wp)
}
