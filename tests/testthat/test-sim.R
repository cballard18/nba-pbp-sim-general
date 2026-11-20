test_that("full games look like NBA games", {
  set.seed(1)
  games <- sim_games(300)
  expect_equal(nrow(games), 300)
  expect_true(all(games$score1 != games$score2))          # no ties
  expect_true(all(games$periods >= 4))
  expect_gt(mean(games$score1 + games$score2), 205)
  expect_lt(mean(games$score1 + games$score2), 240)
})

test_that("possession mode stops when the ball changes teams", {
  set.seed(2)
  state <- game_state(period = 2, secs_left = 400, off_score = 50, def_score = 48)
  for (i in 1:20) {
    p <- sim_pbp(state, until = "possession")
    expect_true(all(p$offense == 1))                       # team 1 keeps it until the last row
    expect_true(all(p$period == 2))
  }
})

test_that("finishing a game starts from the given score", {
  set.seed(3)
  state <- game_state(period = 4, secs_left = 30, off_score = 101, def_score = 103)
  p <- sim_pbp(state)
  expect_gte(p$score1[1], 101)
  expect_gte(p$score2[1], 103)
  expect_true(tail(p$score1, 1) != tail(p$score2, 1))
})

test_that("win probability is sensible", {
  set.seed(4)
  big_lead <- game_state(period = 4, secs_left = 30, off_score = 110, def_score = 95)
  expect_gt(win_prob(big_lead, n = 500), 0.99)
})

test_that("a forced shot only produces outcomes of that shot", {
  set.seed(5)
  state <- game_state(period = 3, secs_left = 300, off_score = 70, def_score = 72)
  g <- simulate_games(games_from_states(state, 500), until = "possession", pbp = TRUE, action = "fg3a")
  first <- g$pbp[, .SD[1], by = game]
  expect_true(all(first$event %in% ACTIONS$fg3a))
  expect_false(any(first$timeout))
  expect_gt(mean(first$event == "fg3_make"), 0.25)
  expect_lt(mean(first$event == "fg3_make"), 0.45)
  expect_error(sim_pbp(state, action = "dunk"), "Unknown action")
})

test_that("an intentional foul is quick and gives FTs in the bonus", {
  set.seed(6)
  state <- game_state(period = 4, secs_left = 20, off_score = 103, def_score = 100, def_fouls = 5)
  g <- simulate_games(games_from_states(state, 200), until = "possession", pbp = TRUE, action = "foul")
  first <- g$pbp[, .SD[1], by = game]
  expect_true(all(first$event == "foul_intentional"))
  expect_true(all(first$duration >= 1 & first$duration <= 3))
  expect_true(all(first$fta == 2))
})

test_that("possessions = 2 plays both teams' possessions", {
  set.seed(7)
  state <- game_state(period = 2, secs_left = 400, off_score = 50, def_score = 48)
  for (i in 1:20) {
    p <- sim_pbp(state, until = "possession", action = "fg2a", possessions = 2)
    expect_true(p$event[1] %in% ACTIONS$fg2a)
    expect_equal(sum(diff(p$offense) != 0), 1)            # exactly one hand-off inside the window
  }
})

test_that("path breakdown covers every simulation", {
  set.seed(8)
  state <- game_state(period = 4, secs_left = 40, off_score = 101, def_score = 103)
  paths <- sim_paths(state, action = "fg3a", n = 1000)
  expect_equal(sum(paths$share), 1)
  expect_true(all(paths$win1 >= 0 & paths$win1 <= 1))
  expect_equal(paths[path == "T1 fg3_make", pts1], 3)
})

test_that("neural net win probability is sensible and in the pbp", {
  ot_start <- nn_win_prob(game_state(period = 5, secs_left = 300, off_score = 100, def_score = 100))
  expect_gt(ot_start, 0.5)                                       # tied at the start of OT: the ball
  expect_lt(ot_start, 0.6)                                       # is worth a little
  expect_gt(nn_win_prob(game_state(period = 4, secs_left = 30, off_score = 110, def_score = 95)), 0.99)
  set.seed(9)
  p <- sim_pbp()
  expect_true(all(p$wp >= 0 & p$wp <= 1))
  expect_equal(tail(p$wp, 1), as.numeric(tail(p$score1, 1) > tail(p$score2, 1)))
  expect_equal(sum(p$wp_delta), tail(p$wp, 1) - 0.5, tolerance = 0.05)   # tip-off starts near 0.5
})

test_that("compare_actions gives one row per action", {
  set.seed(10)
  state <- game_state(period = 4, secs_left = 40, off_score = 101, def_score = 103)
  res <- compare_actions(state, n = 500)
  expect_equal(res$action, c("model", "fg2a", "fg3a"))
  expect_equal(res$ewp_vs_model[1], 0)
  expect_true(all(res$ewp1 > 0 & res$ewp1 < 1))
})
