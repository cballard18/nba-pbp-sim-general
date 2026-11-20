# Definitions shared by the data pipeline (data-raw/), model fitting and the simulator.

# What can end a segment (a team's turn with the ball, until a shot / turnover / foul / period end)
OUTCOMES <- c("fg2_make", "fg2_miss", "fg3_make", "fg3_miss", "fg2_and1", "fg3_and1",
              "foul_shoot2", "foul_shoot3", "foul_nonshoot", "turnover", "end_period")

# Actions a user can force on a segment, and the outcomes the model may pick for each.
# "foul" is an intentional foul by the defense: a quick non-shooting foul (2 FTs in the bonus).
ACTIONS <- list(fg2a = c("fg2_make", "fg2_miss", "fg2_and1", "foul_shoot2"),
                fg3a = c("fg3_make", "fg3_miss", "fg3_and1", "foul_shoot3"),
                turnover = "turnover",
                foul = "foul_nonshoot")

check_action <- function(action) {
  bad <- setdiff(action[!is.na(action)], c(names(ACTIONS), setdiff(OUTCOMES, "end_period")))
  if (length(bad)) stop("Unknown action: ", paste(bad, collapse = ", "), ". Use NA (model decides), ",
                        paste(names(ACTIONS), collapse = ", "), " or an exact outcome.", call. = FALSE)
}

# How a segment began
START_TYPES <- c("period_start", "after_make", "after_dreb", "after_oreb", "after_turnover", "sideout")

MAX_DUR <- 30                        # duration classes are whole seconds 0..30 (30 = 30+)
TEAM_FOUL_ACTS <- c(1, 2, 3, 5, 6, 9, 14, 15, 27, 28, 29, 31)  # defensive fouls that count toward the bonus
NONSHOOT_ACTS  <- c(1, 3, 5, 6, 9, 14, 15, 27, 28, 31)
SHOOT_ACTS     <- c(2, 29)

period_length <- function(period) ifelse(period <= 4, 720, 300)

# Time until the game could end (end of regulation, or end of the current overtime)
game_secs_left <- function(period, secs_left) secs_left + pmax(4 - period, 0) * 720

# Close late-game situations are rare but decide games; the models get extra weight there
is_clutch <- function(period, secs_left, score_diff) period >= 4 & secs_left <= 180 & abs(score_diff) <= 10

# NBA penalty: 5th team foul of a quarter (4th in OT), or 2nd foul in the last 2:00 of a period
in_bonus <- function(period, secs_left, fouls, fouls_l2) {
  fouls >= ifelse(period <= 4, 4, 3) | (secs_left <= 120 & fouls_l2 >= 1)
}

one_hot <- function(x, levels, prefix) {
  m <- outer(x, levels, "==") * 1
  colnames(m) <- paste0(prefix, levels)
  m
}

# ---- model inputs (identical for training rows and simulated game states) ----
x_state <- function(d) {
  cbind(period     = pmin(d$period, 5),
        secs_left  = d$secs_left,
        game_secs_left = game_secs_left(d$period, d$secs_left),
        score_diff = d$score_diff,
        def_bonus  = as.numeric(d$def_bonus),
        one_hot(d$start_type, START_TYPES, "st_"))
}
x_timeout  <- function(d) cbind(x_state(d), off_to_left = d$off_to_left)
x_outcome  <- function(d) cbind(x_state(d), after_timeout = as.numeric(d$after_timeout))
x_duration <- function(d) cbind(x_outcome(d), one_hot(d$outcome, OUTCOMES, "o_"))

# Draw one category per row from a matrix of (unnormalised) probabilities
sample_rows <- function(prob) {
  cum <- prob %*% upper.tri(diag(ncol(prob)), diag = TRUE)
  u <- runif(nrow(prob)) * cum[, ncol(prob)]
  pmin(rowSums(u > cum) + 1L, ncol(prob))
}

# Class probabilities as an n x K matrix (works across xgboost versions)
predict_probs <- function(model, X) {
  p <- predict(model, X)
  if (is.null(dim(p))) p <- matrix(p, nrow = nrow(X), byrow = TRUE)
  p
}
