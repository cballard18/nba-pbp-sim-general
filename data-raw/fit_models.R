# Fit the sub-models that drive the simulator:
#   1. timeout   - does the team with the ball call a timeout this segment?   (xgboost, binary)
#   2. outcome   - how does the segment end?                                    (xgboost, 11 classes)
#   3. duration  - how many seconds does it take, given the outcome?           (xgboost, 0..30 s)
#   4. rebounds  - offensive rebound rate after a miss, by shot type           (league rate)
#   5. free throws - league FT%                                                 (league rate)
#   6. shot odds  - make / miss / and-1 / shooting foul of a forced 2 or 3     (league rate)
# Games are split 85/15; held-out log loss is reported against a no-feature baseline.
# Run from the package root. Loads the package code (rules, feature builders, simulator).
devtools::load_all(quiet = TRUE)
suppressMessages({ library(data.table); library(arrow); library(xgboost) })
set.seed(1)

seg <- as.data.table(read_parquet("data-raw/data/segments.parquet"))
seg[, test := game_id %in% sample(unique(game_id), round(0.15 * uniqueN(game_id)))]
seg[, dur_class := pmin(round(duration), MAX_DUR)]
seg[, clutch := is_clutch(period, secs_left, score_diff)]

logloss <- function(prob, y) -mean(log(pmax(prob[cbind(seq_along(y), y + 1)], 1e-15)))

# clutch: rows that are kept in full and up-weighted (rare late-game states decide close games)
fit_xgb <- function(X, y, test, clutch, num_class, name, max_train = 1e6, clutch_weight = 10) {
  binary <- num_class == 2
  train <- which(!test & !clutch)
  if (length(train) > max_train) train <- sample(train, max_train)  # more rows add little here
  train <- c(train, which(!test & clutch))
  w <- ifelse(clutch, clutch_weight, 1)
  params <- list(objective = if (binary) "binary:logistic" else "multi:softprob",
                 eval_metric = if (binary) "logloss" else "mlogloss",
                 eta = 0.2, max_depth = 6, min_child_weight = 50, subsample = 0.8,
                 tree_method = "hist", nthread = parallel::detectCores())
  if (!binary) params$num_class <- num_class
  dtrain <- xgb.DMatrix(X[train, , drop = FALSE], label = y[train], weight = w[train])
  dtest  <- xgb.DMatrix(X[test, , drop = FALSE],  label = y[test],  weight = w[test])
  model <- xgb.train(params, dtrain, nrounds = 500, evals = list(test = dtest),
                     early_stopping_rounds = 20, verbose = 0)

  p <- predict_probs(model, X[test, , drop = FALSE])
  if (binary) p <- cbind(1 - p, p)
  base <- tabulate(y[!test] + 1, num_class) / sum(!test)
  cat(sprintf("%-9s rounds %3d | test logloss %.4f vs baseline %.4f (%.1f%% better)\n", name,
              xgb.attr(model, "best_iteration"), logloss(p, y[test]),
              logloss(matrix(base, sum(test), num_class, byrow = TRUE), y[test]),
              100 * (1 - logloss(p, y[test]) / logloss(matrix(base, sum(test), num_class, byrow = TRUE), y[test]))))
  model
}

dir.create("inst/models", showWarnings = FALSE)

# 1. timeouts: only seasons under the current 7-timeout rules (2017-18 on), and only when one is left
to <- seg[season >= 2018 & off_to_left > 0]
m_timeout <- fit_xgb(x_timeout(to), as.integer(to$after_timeout), to$test, to$clutch, 2, "timeout")

# 2. outcome
m_outcome <- fit_xgb(x_outcome(seg), match(seg$outcome, OUTCOMES) - 1L, seg$test, seg$clutch,
                     length(OUTCOMES), "outcome")

# 3. duration (end-of-period segments simply run out the clock, so they are excluded)
du <- seg[outcome != "end_period"]
m_duration <- fit_xgb(x_duration(du), du$dur_class, du$test, du$clutch, MAX_DUR + 1, "duration")

# 4-6. league rates
oreb <- seg[!is.na(oreb), .(rate = mean(oreb)), by = shot_kind]
shot_odds <- lapply(ACTIONS[c("fg2a", "fg3a")], function(outs) {
  prop.table(table(factor(seg[outcome %in% outs]$outcome, outs)))[outs] |> unclass() |> c()
})
rates <- list(oreb = setNames(oreb$rate, oreb$shot_kind), ft_pct = readRDS("data-raw/data/constants.rds")$ft_pct,
              shot_odds = shot_odds)
print(rates)

invisible(xgb.save(m_timeout,  "inst/models/timeout.ubj"))
invisible(xgb.save(m_outcome,  "inst/models/outcome.ubj"))
invisible(xgb.save(m_duration, "inst/models/duration.ubj"))
saveRDS(rates, "inst/models/rates.rds")
saveRDS(unique(seg[test == TRUE, game_id]), "data-raw/data/test_games.rds")

# ---- calibration check: predicted vs actual outcome mix in key game situations ----
tst <- seg[test == TRUE]
tst[, situation := fcase(period >= 4 & secs_left <= 60 & score_diff < 0 & score_diff >= -6, "Q4/OT last 1:00, offense down 1-6",
                         period >= 4 & secs_left <= 60 & score_diff > 0 & score_diff <= 6,  "Q4/OT last 1:00, offense up 1-6",
                         secs_left <= 24 & period < 4, "end of Q1-Q3 (<24s)",
                         default = "rest of game")]
p <- predict_probs(m_outcome, x_outcome(tst)); colnames(p) <- OUTCOMES
cal <- cbind(tst[, .(situation, outcome)], p)
show <- c("fg2_make", "fg3_make", "fg3_miss", "foul_nonshoot", "turnover", "end_period")
for (s in unique(cal$situation)) {
  x <- cal[situation == s]
  cat("\n", s, " (n =", nrow(x), ")\n")
  print(round(rbind(predicted = colMeans(x[, ..show]),
                    actual    = sapply(show, function(o) mean(x$outcome == o))), 3))
}
