#' Simulate games segment by segment
#'
#' The engine behind every other function. Many games are simulated side by side: each loop
#' step advances every unfinished game by one segment (a team's turn with the ball), so each
#' model is called once per step on all games.
#'
#' @param g Games from [new_games()] or [games_from_states()].
#' @param until `"game"` plays every game to the final buzzer; `"possession"` stops each game
#'   after `possessions` changes of possession, or when the period ends.
#' @param pbp If `TRUE`, also return `g$pbp`, one row per simulated segment.
#' @param action Force what happens, one entry per segment in order (`NA` = the model decides).
#'   `"fg2a"` / `"fg3a"`: the offense shoots, and the model picks make, miss, and-1 or shooting
#'   foul. `"turnover"`. `"foul"`: the defense fouls on purpose within 1-3 seconds (2 FTs in
#'   the bonus, otherwise a side-out). Or an exact outcome such as `"fg3_miss"`. A forced
#'   segment never starts with a timeout. An offensive rebound or a side-out starts a new
#'   segment, so e.g. `c("fg3a", NA, "foul")` means: shoot a 3, let the model play the next
#'   segment, then foul on the one after.
#' @param possessions With `until = "possession"`, how many changes of possession to play
#'   before stopping (the end of a period always stops).
#' @param odds Odds of a forced `"fg2a"` / `"fg3a"` (make, miss, and-1, shooting foul).
#'   `"league"` (default): league averages, the same at any clock and score. `"model"`: the
#'   outcome model's odds for that clock and score, which late in the clock include heaves and
#'   end-of-clock shots. Shots the model decides on its own always use the model's odds.
#'   Either way a forced shot (`"fg2a"`, `"fg3a"` or one of their exact outcomes) takes the same
#'   time whatever kind it is and however it ends: the model's duration averaged over all shot
#'   outcomes.
#' @param m Models, by default those shipped with the package.
#' @param max_steps Safety cap on loop steps.
#' @param fix Internal (used for eWP): pins the first segment's free throws and rebound, as a
#'   list of `ftm_lead` (FTs made before the last one), `last_made` and `oreb`; `NA` = random.
#' @return `g`, updated: final `score` matrix, `period`, per-game `stats`, `over` (game
#'   finished), and `pbp` if asked. `pbp$wp` is team 1's win probability after each event from
#'   the neural net (see [win_prob()]), and `wp_delta` its change from the row before.
#' @export
simulate_games <- function(g, until = c("game", "possession"), pbp = FALSE, action = NULL,
                           possessions = 1, odds = c("league", "model"), m = nbasim_models(),
                           max_steps = 3000, fix = NULL) {
  until <- match.arg(until)
  odds <- match.arg(odds)
  check_action(action)
  if (is.null(g$over)) g$over <- rep(FALSE, length(g$done))
  changes <- integer(length(g$done))
  if (pbp) wp0 <- state_wp(g, seq_along(g$done), game_secs_left(g$period, g$secs_left), m)
  log <- list()
  for (step in seq_len(max_steps)) {
    a <- which(!g$done)
    if (!length(a)) break
    n <- length(a)
    act <- if (step <= length(action)) action[step] else NA
    fx  <- if (step == 1) fix else NULL
    off <- g$off[a]; def <- 3L - off
    oi <- cbind(a, off); di <- cbind(a, def)

    # Q4 rule: at most 2 timeouts after the 3:00 mark
    late4 <- a[g$period[a] == 4 & g$secs_left[a] <= 180]
    g$to_left[late4, ] <- pmin(g$to_left[late4, , drop = FALSE], 2)

    d <- segment_inputs(g, a)

    # 1. timeout
    p_to <- predict(m$timeout, x_timeout(d)) * (d$off_to_left > 0) * is.na(act)
    d$after_timeout <- runif(n) < p_to
    g$to_left[oi] <- g$to_left[oi] - d$after_timeout

    # 2. outcome
    o <- OUTCOMES[sample_rows(outcome_probs(d, act, m, odds))]
    d$outcome <- o

    # 3. duration, never longer than the clock (an intentional foul comes within 1-3 s)
    pd <- if (forced_shot(act)) shot_clock_probs(d, m) else predict_probs(m$duration, x_duration(d))
    pd[outer(d$secs_left + 0.5, 0:MAX_DUR, "<")] <- 0
    dur <- (0:MAX_DUR)[sample_rows(pd)] + runif(n, -0.5, 0.5)
    if (identical(act, "foul")) dur <- runif(n, 1, 3)
    dur <- pmin(pmax(dur, 0.1), d$secs_left)
    dur[o == "end_period"] <- d$secs_left[o == "end_period"]
    clock <- d$secs_left - dur

    # 4. resolve: points, fouls, free throws
    pts  <- c(fg2_make = 2, fg3_make = 3, fg2_and1 = 2, fg3_and1 = 3)[o]
    n_ft <- c(fg2_and1 = 1, fg3_and1 = 1, foul_shoot2 = 2, foul_shoot3 = 3)[o]
    pts[is.na(pts)] <- 0; n_ft[is.na(n_ft)] <- 0
    fouled <- o %in% c("fg2_and1", "fg3_and1", "foul_shoot2", "foul_shoot3", "foul_nonshoot")
    n_ft[o == "foul_nonshoot" & in_bonus(d$period, clock, g$fouls[di], g$fouls_l2[di])] <- 2
    g$fouls[di]    <- g$fouls[di] + fouled
    g$fouls_l2[di] <- g$fouls_l2[di] + (fouled & clock <= 120)

    has_ft    <- n_ft > 0
    last_made <- pinned(fx$last_made, runif(n) < m$ft_pct)
    ft_lead   <- pinned(fx$ftm_lead, rbinom(n, pmax(n_ft - 1, 0), m$ft_pct))
    ft_made   <- has_ft * (ft_lead + last_made)
    pts <- pts + ft_made
    g$score[oi] <- g$score[oi] + pts

    # 5. who has the ball next, and how the next segment starts
    kind <- fcase(o %in% c("fg2_make", "fg3_make"), "make",
                  has_ft, fifelse(last_made, "make", "miss"),
                  o %in% c("fg2_miss", "fg3_miss"), "miss",
                  o == "turnover", "turnover",
                  o == "foul_nonshoot", "sideout",
                  default = "period_end")
    shot_kind <- fcase(has_ft, "ft", o == "fg3_miss", "fg3", default = "fg2")
    oreb <- kind == "miss" & pinned(fx$oreb, runif(n) < m$oreb[shot_kind])
    g$off[a] <- fifelse(kind == "sideout" | oreb, off, def)
    g$start_type[a] <- fcase(kind == "make", "after_make", oreb, "after_oreb",
                             kind == "miss", "after_dreb", kind == "turnover", "after_turnover",
                             default = "sideout")
    g$secs_left[a] <- clock
    s <- cbind(d$start_type %in% c("period_start", "after_make", "after_dreb", "after_turnover"),
               1, o %in% c("fg2_make", "fg2_miss", "fg2_and1"), o %in% c("fg3_make", "fg3_miss", "fg3_and1"),
               o %in% c("fg2_make", "fg3_make", "fg2_and1", "fg3_and1"), n_ft, o == "turnover", oreb,
               d$after_timeout, fouled)
    g$stats[a, ] <- g$stats[a, ] + s

    # 6. end of period / game
    e <- a[clock <= 0 | o == "end_period"]
    if (length(e)) {
      over <- g$period[e] >= 4 & g$score[e, 1] != g$score[e, 2]
      g$done[e[over]] <- TRUE
      g$over[e[over]] <- TRUE
      nx <- e[!over]
      if (length(nx)) {
        g$period[nx] <- g$period[nx] + 1L
        p <- g$period[nx]
        g$secs_left[nx] <- period_length(p)
        g$fouls[nx, ] <- 0; g$fouls_l2[nx, ] <- 0
        g$start_type[nx] <- "period_start"
        # Q2 & Q3 go to the team that lost the opening tip, Q4 to the winner, OT is a jump ball
        g$off[nx] <- fcase(p %in% 2:3, 3L - g$tip[nx], p == 4, g$tip[nx],
                           default = sample(1:2, length(nx), replace = TRUE))
        # timeouts: at most 4 in Q4, 2 per overtime
        q4 <- nx[p == 4]; ot <- nx[p >= 5]
        g$to_left[q4, ] <- pmin(g$to_left[q4, , drop = FALSE], 4)
        g$to_left[ot, ] <- 2
      }
    }
    if (pbp) log[[step]] <- data.table(
      game = a, period = d$period, clock = clock, offense = off, start_type = d$start_type,
      timeout = d$after_timeout, duration = dur,
      event = if (identical(act, "foul")) "foul_intentional" else o, fta = n_ft, ftm = ft_made,
      rebound = fifelse(kind == "miss" & clock > 0, fifelse(oreb, "offensive", "defensive"), NA_character_),
      points = pts, score1 = g$score[a, 1], score2 = g$score[a, 2],
      wp = state_wp(g, a, game_secs_left(g$period[a], g$secs_left[a]), m))  # state after any period change

    if (until == "possession") {
      changes[a] <- changes[a] + (g$off[a] != off)
      g$done[a[changes[a] >= possessions | a %in% e]] <- TRUE
    }
  }
  if (pbp) {
    g$pbp <- rbindlist(log)
    setorder(g$pbp, game)
    g$pbp[, clock := sprintf("%d:%04.1f", clock %/% 60, clock %% 60)]
    g$pbp[, wp_delta := wp - shift(wp, fill = wp0[.BY$game]), by = game]
  }
  g
}

# A pinned value for every game, or the random draws when it isn't pinned
pinned <- function(value, draws) if (length(value) && !is.na(value)) rep(value, length(draws)) else draws

# Model inputs for the segment that games `a` are about to play (before any timeout)
segment_inputs <- function(g, a) {
  off <- g$off[a]
  oi <- cbind(a, off); di <- cbind(a, 3L - off)
  list(period = g$period[a], secs_left = g$secs_left[a], score_diff = g$score[oi] - g$score[di],
       def_bonus = in_bonus(g$period[a], g$secs_left[a], g$fouls[di], g$fouls_l2[di]),
       start_type = g$start_type[a], off_to_left = g$to_left[oi])
}

SHOTS <- c("fg2a", "fg3a")

# A forced shot: a 2 or a 3, or one of their exact outcomes (e.g. "fg3_miss")
forced_shot <- function(act) !is.na(act) && act %in% c(SHOTS, unlist(ACTIONS[SHOTS]))

# League odds of each shot's outcomes (from rates.rds)
league_shot_odds <- function(m) {
  if (is.null(m$shot_odds)) stop("These models have no league shot odds (rates.rds); ",
                                 "refit with data-raw/fit_models.R or use odds = \"model\".", call. = FALSE)
  m$shot_odds
}

# Duration probabilities of a forced shot: the same for a 2 or a 3, made or missed. The model's
# duration for each shot outcome, averaged with the league odds (half 2s, half 3s). Conditioning
# on the outcome would bring back the selection that league odds remove: late in the clock,
# real 3s (heaves) beat the buzzer far more often than 2s, and those cells are thin and noisy.
shot_clock_probs <- function(d, m) {
  w <- unlist(unname(league_shot_odds(m))) / 2
  # games in a batch often share a state (e.g. eWP copies of one state): predict each distinct
  # state once, for all shot outcomes in a single call
  X <- x_outcome(d)
  key <- do.call(paste, as.data.frame(X))
  first <- !duplicated(key)
  Xu <- X[first, , drop = FALSE]
  nu <- nrow(Xu)
  p <- predict_probs(m$duration, do.call(rbind, lapply(names(w), function(o) {
    cbind(Xu, one_hot(rep(o, nu), OUTCOMES, "o_"))
  })))
  pu <- Reduce(`+`, lapply(seq_along(w), function(k) w[[k]] * p[(k - 1) * nu + seq_len(nu), , drop = FALSE]))
  pu[match(key, key[first]), , drop = FALSE]
}

# Outcome probabilities (unnormalised, one row per game). Shot clock: a team can't run out a
# period with more than 24 s left. A forced action keeps only its outcomes, in the model's
# proportions, or for a forced shot with odds = "league", the league's.
outcome_probs <- function(d, act, m, odds = "league") {
  n <- length(d$secs_left)
  if (!is.na(act) && odds == "league" && act %in% SHOTS) {
    lg <- league_shot_odds(m)[[act]]
    po <- matrix(0, n, length(OUTCOMES))
    po[, match(names(lg), OUTCOMES)] <- rep(lg, each = n)
    return(po)
  }
  po <- predict_probs(m$outcome, x_outcome(d))
  po[d$secs_left > 24, OUTCOMES == "end_period"] <- 0
  if (!is.na(act)) po[, !OUTCOMES %in% c(ACTIONS[[act]], act)] <- 0
  po
}

#' One row per simulated game
#' @param g Output of [simulate_games()].
#' @return A data.table: final scores, number of periods, and box-score counts.
#' @export
game_summary <- function(g) {
  data.table(score1 = g$score[, 1], score2 = g$score[, 2], periods = g$period, g$stats)
}

#' Play-by-play for one simulated game
#'
#' @param state `NULL` for a full game from tip-off, or a [game_state()].
#' @param until `"game"` to finish the game, `"possession"` for just the next possession(s).
#' @inheritParams simulate_games
#' @return A data.table with one row per segment: period, clock (after the event), offense
#'   (1/2), start_type, timeout, duration, event, fta, ftm, rebound, points, score1, score2.
#' @examples
#' sim_pbp()
#' state <- game_state(period = 4, secs_left = 40, off_score = 101, def_score = 103)
#' sim_pbp(state)
#' sim_pbp(state, until = "possession")
#' sim_pbp(state, until = "possession", action = "fg3a", possessions = 2)
#' @export
sim_pbp <- function(state = NULL, until = c("game", "possession"), action = NULL, possessions = 1,
                    odds = c("league", "model")) {
  g <- if (is.null(state)) new_games(1) else games_from_states(state)
  simulate_games(g, until = until, pbp = TRUE, action = action,
                 possessions = possessions, odds = odds)$pbp[, game := NULL][]
}

#' Simulate many games from tip-off
#' @param n Number of games.
#' @return [game_summary()] of the simulated games.
#' @examples
#' games <- sim_games(200)
#' mean(games$score1 + games$score2)
#' @export
sim_games <- function(n = 1000) {
  game_summary(simulate_games(new_games(n)))
}

#' Simulate many games from a game state
#' @param state A [game_state()]. Team 1 is the team with the ball.
#' @param n Number of simulations.
#' @inheritParams simulate_games
#' @return [game_summary()] of the simulations.
#' @export
sim_from_state <- function(state, n = 1000, until = c("game", "possession"), action = NULL,
                           possessions = 1, odds = c("league", "model")) {
  game_summary(simulate_games(games_from_states(state, n), until = until, action = action,
                              possessions = possessions, odds = odds))
}

# Simulate n copies of the next `possessions` possessions from a state. One row per simulation:
# the path of events, points scored, NN win prob after the path, and its weight `w` (the
# weights sum to 1).
#
# When the first action is a shot ("fg2a" / "fg3a"), what happens in that segment is not drawn
# at random. The odds of each branch are known exactly: the shot's outcome (make, miss, and-1 or
# shooting foul, at `odds`), then its free throws (league FT%) and the rebound after a miss
# (league OREB rate). Each branch gets its own batch of simulations (about n x its probability)
# and the batch is weighted by that probability. Only the clock and the later possessions are
# still random, which removes most of the run-to-run noise in eWP.
run_paths <- function(state, action, possessions, n, odds = "league", m = nbasim_models()) {
  first <- action[1]
  if (is.na(first) || !first %in% SHOTS)
    return(sim_path_batch(state, action, possessions, n, odds, m)[, w := 1 / n][])
  b <- shot_branches(state, first, m, odds)
  rbindlist(lapply(seq_len(nrow(b)), function(i) {
    n_b <- max(1L, round(n * b$p[i]))
    fix <- list(ftm_lead = b$ftm_lead[i], last_made = b$last_made[i], oreb = b$oreb[i])
    sim_path_batch(state, c(b$outcome[i], action[-1]), possessions, n_b, odds, m, fix)[
      , w := b$p[i] / n_b]
  }))
}

# Every way a forced shot's segment can end, with its probability: outcome x free throws x
# rebound. `NA` columns are left random (they don't apply to that branch).
shot_branches <- function(state, act, m, odds = "league") {
  d <- segment_inputs(games_from_states(state), 1L)
  d$after_timeout <- FALSE                                 # a forced segment never starts with one
  outs <- ACTIONS[[act]]
  p_out <- outcome_probs(d, act, m, odds)[1, match(outs, OUTCOMES)]
  p_out <- p_out / sum(p_out)
  ft <- m$ft_pct
  b <- rbindlist(Map(function(o, p_o) {
    n_ft <- c(fg2_and1 = 1, fg3_and1 = 1, foul_shoot2 = 2, foul_shoot3 = 3)[o]
    if (is.na(n_ft)) {                                     # no FTs: a make, or a miss and a rebound
      if (!o %in% c("fg2_miss", "fg3_miss")) return(data.table(outcome = o, p = p_o))
      r <- m$oreb[sub("_miss", "", o)]
      return(data.table(outcome = o, oreb = c(TRUE, FALSE), p = p_o * c(r, 1 - r)))
    }
    r <- m$oreb["ft"]
    lead <- 0:(n_ft - 1)                                   # FTs made before the last one
    b <- CJ(ftm_lead = lead, last = c("make", "oreb", "dreb"))
    b[, `:=`(outcome = o, last_made = last == "make",
             oreb = fifelse(last == "make", NA, last == "oreb"),
             p = p_o * dbinom(ftm_lead, n_ft - 1, ft) *
               fcase(last == "make", ft, last == "oreb", (1 - ft) * r, default = (1 - ft) * (1 - r)))]
    b[, last := NULL][]
  }, outs, p_out), fill = TRUE)
  for (col in c("ftm_lead", "last_made", "oreb")) if (!col %in% names(b)) b[, (col) := NA]
  b[p > 0]
}

sim_path_batch <- function(state, action, possessions, n, odds, m, fix = NULL) {
  g <- simulate_games(games_from_states(state, n), until = "possession", pbp = TRUE,
                      action = action, possessions = possessions, odds = odds, m = m, fix = fix)
  p <- g$pbp
  p[, label := paste0("T", offense, " ", event,
                      fifelse(fta > 0, paste0(" ", ftm, "/", fta, " FT"), ""),
                      fifelse(is.na(rebound), "", fifelse(rebound == "offensive", " OREB", " DREB")))]
  res <- p[, .(path = paste(label, collapse = " > "), ewp = wp[.N]), by = game]
  res[, `:=`(pts1 = g$score[game, 1] - state$off_score, pts2 = g$score[game, 2] - state$def_score)][]
}

#' How often each action path happens, and what it does to the game
#'
#' Simulates `n` copies of the next `possessions` possessions from a state (with any forced
#' `action`) and labels each one's path of events.
#'
#' @inheritParams sim_from_state
#' @return A data.table, one row per path, most common first: `path` (e.g.
#'   `"T1 fg3_miss DREB > T2 turnover"`; team 1 has the ball in `state`), `share` of
#'   simulations, `pts1` / `pts2` scored during the path, and `ewp1`, team 1's neural net win
#'   probability right after the path.
#' @examples
#' state <- game_state(period = 4, secs_left = 40, off_score = 101, def_score = 103)
#' sim_paths(state, action = "fg3a", n = 2000)
#' sim_paths(state, action = c("fg2a", "foul"), possessions = 2, n = 2000)
#' @export
sim_paths <- function(state, action = NULL, possessions = 1, n = 10000,
                      odds = c("league", "model")) {
  run_paths(state, action, possessions, n, match.arg(odds))[
    , .(share = sum(w), pts1 = weighted.mean(pts1, w), pts2 = weighted.mean(pts2, w),
        ewp1 = weighted.mean(ewp, w)),
    by = path][order(-share)]
}

#' Compare actions by expected win probability
#'
#' For each action, simulates the next `possessions` possessions `n` times and averages team 1's
#' neural net win probability after them (eWP).
#'
#' @inheritParams sim_from_state
#' @param actions A named list of actions, each one as in [simulate_games()]'s `action`.
#' @return A data.table, one row per action, best first: `ewp1`, and `ewp_vs_best`, its change
#'   against the best action (0 for the best, negative for the others).
#' @examples
#' state <- game_state(period = 4, secs_left = 40, off_score = 101, def_score = 103)
#' compare_actions(state, n = 2000)
#' @export
compare_actions <- function(state, actions = list(fg2a = "fg2a", fg3a = "fg3a"),
                            possessions = 1, n = 10000, odds = c("league", "model")) {
  odds <- match.arg(odds)
  res <- rbindlist(lapply(actions, function(act) {
    data.table(ewp1 = win_prob(state, act, possessions, n, odds))
  }), idcol = "action")
  res[order(-ewp1)][, ewp_vs_best := ewp1 - ewp1[1]][]
}
