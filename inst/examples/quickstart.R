# Quick tour of the simulator. Run after fit_models.R:  Rscript example.R
source("sim.R")
set.seed(1)
m <- load_models()

# 1. Play-by-play of one full game
pbp <- sim_pbp(m)
print(head(pbp, 10))
cat("Final:", tail(pbp$score1, 1), "-", tail(pbp$score2, 1), "\n\n")

# 2. A late-game spot: down 2 with the ball, 0:40 left in Q4, opponent in the bonus
state <- game_state(period = 4, secs_left = 40, off_score = 101, def_score = 103,
                    def_fouls = 5, def_fouls_l2 = 1)

print(sim_pbp(m, state, until = "possession"))  # just the next possession
print(sim_pbp(m, state))                        # play out the rest of the game

# 3. Win probability from that spot (10,000 simulations)
g <- simulate_games(games_from_states(state, reps = 10000), m)
cat("\nWin probability:", mean(g$score[, 1] > g$score[, 2]),
    "| OT:", mean(g$period > 4), "\n")

# 4. League-average game distribution (1,000 full games)
games <- game_summary(simulate_games(new_games(1000), m))
cat("Avg total:", mean(games$score1 + games$score2),
    "| Avg margin:", mean(abs(games$score1 - games$score2)), "\n")
