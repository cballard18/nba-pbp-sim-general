# Does the simulator reproduce real NBA games?
#   1. per-game box-score totals: simulated games from tip-off vs. actual games
#   2. final-margin distribution (tests late-game fouling / clock management)
#   3. win-probability calibration: simulate held-out games forward from real mid-game states
#   4. close late-game states: does the sim send the right share of games to overtime?
# Run from the package root. Loads the package code (rules, feature builders, simulator).
devtools::load_all(quiet = TRUE)
suppressMessages({ library(data.table); library(arrow); library(xgboost) })
set.seed(7)
seg <- as.data.table(read_parquet("data-raw/data/segments.parquet"))
dir.create("data-raw/output", showWarnings = FALSE)

# ---- 1. per-game totals ----
actual <- seg[, .(total = final_total[1], margin = abs(final_margin[1]), ot = max(period) > 4,
                  poss = sum(start_type %in% c("period_start", "after_make", "after_dreb", "after_turnover")),
                  segments = .N,
                  fg2a = sum(outcome %like% "^fg2"), fg3a = sum(outcome %like% "^fg3"),
                  fta = sum(n_ft), tov = sum(outcome == "turnover"), oreb = sum(oreb, na.rm = TRUE),
                  timeouts = sum(after_timeout)), by = game_id]

t0 <- Sys.time()
sim <- game_summary(simulate_games(new_games(5000)))
cat("simulated 5000 games in", round(as.numeric(Sys.time() - t0, units = "secs")), "s\n")
sim[, `:=`(total = score1 + score2, margin = abs(score1 - score2), ot = periods > 4)]

cols <- c("total", "margin", "ot", "poss", "segments", "fg2a", "fg3a", "fta", "tov", "oreb", "timeouts")
tab <- data.table(stat = cols,
                  actual_mean = sapply(cols, function(x) mean(actual[[x]])),
                  sim_mean    = sapply(cols, function(x) mean(sim[[x]])),
                  actual_sd   = sapply(cols, function(x) sd(actual[[x]])),
                  sim_sd      = sapply(cols, function(x) sd(sim[[x]])))
cat("\nPer-game totals (both teams combined)\n"); print(tab, digits = 3)

# ---- 2. final margin distribution ----
k <- 0:30
marg <- data.table(margin = k,
                   actual = sapply(k, function(x) mean(actual$margin == x)),
                   sim    = sapply(k, function(x) mean(sim$margin == x)))
cat("\nFinal margin distribution (first 12)\n"); print(marg[1:12], digits = 3)

# ---- 3. win-probability calibration from held-out mid-game states ----
test_games <- readRDS("data-raw/data/test_games.rds")
states <- seg[game_id %in% test_games & period <= 4 & final_margin != 0][sample(.N, 2000)]
reps <- 100
g <- simulate_games(games_from_states(states, reps))
states[, `:=`(
  wp_sim     = colMeans(matrix(g$score[, 1] > g$score[, 2], nrow = reps)),
  margin_sim = colMeans(matrix(g$score[, 1] - g$score[, 2], nrow = reps)),
  win        = final_margin > 0)]

brier <- function(p, y) mean((p - y)^2)
cat(sprintf("\nWin probability, %d held-out states x %d sims\n", nrow(states), reps))
cat(sprintf("  Brier: sim %.4f | coin flip 0.2500 | 'leader wins 75%%' baseline %.4f\n",
            brier(states$wp_sim, states$win), brier(0.5 + 0.25 * sign(states$score_diff), states$win)))
cat(sprintf("  Final margin RMSE: sim %.2f | current margin carried forward %.2f\n",
            sqrt(mean((states$margin_sim - states$final_margin)^2)),
            sqrt(mean((states$score_diff - states$final_margin)^2))))
states[, bin := cut(wp_sim, seq(0, 1, 0.1), include.lowest = TRUE)]
cal <- states[, .(n = .N, predicted = mean(wp_sim), actual = mean(win)), keyby = bin]
print(cal, digits = 3)
late <- states[period == 4 & secs_left <= 300]
cat(sprintf("  Q4 last 5:00 only (n = %d): Brier %.4f\n", nrow(late), brier(late$wp_sim, late$win)))

# ---- 4. overtime rate from close late-game states (held-out games) ----
seg[, went_ot := max(period) > 4, by = game_id]
close <- seg[game_id %in% test_games & period == 4 & secs_left <= 120 & abs(score_diff) <= 3]
reps <- 50
g <- simulate_games(games_from_states(close, reps))
close[, sim_ot := colMeans(matrix(g$period > 4, nrow = reps))]
cat("\nOvertime rate from Q4 states with <= 2:00 left, by offense's score margin\n")
print(close[, .(n = .N, actual = mean(went_ot), sim = mean(sim_ot)), keyby = score_diff], digits = 3)

# ---- plots ----
png("data-raw/output/validation.png", width = 1400, height = 500, res = 110)
par(mfrow = c(1, 3))
plot(marg$margin, marg$actual, type = "h", lwd = 6, col = "grey70", xlab = "final margin",
     ylab = "share of games", main = "Final margin: actual (bars) vs sim (line)")
lines(marg$margin, marg$sim, type = "b", pch = 19, col = "steelblue")
plot(cal$predicted, cal$actual, pch = 19, col = "steelblue", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "simulated win probability", ylab = "actual win rate", main = "Win-prob calibration")
abline(0, 1, lty = 2)
r <- range(c(actual$total, sim$total))
hist(actual$total, breaks = seq(r[1] - 5, r[2] + 5, 5), col = rgb(.5, .5, .5, .5), freq = FALSE,
     main = "Game total: actual (grey) vs sim (blue)", xlab = "total points")
hist(sim$total, breaks = seq(r[1] - 5, r[2] + 5, 5), col = rgb(.27, .51, .71, .4), freq = FALSE, add = TRUE)
dev.off()
cat("\nplots saved to data-raw/output/validation.png\n")
