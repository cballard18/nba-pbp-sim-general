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
