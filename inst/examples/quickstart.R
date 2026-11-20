# Quick tour of nbasim. Run with:  source(system.file("examples/quickstart.R", package = "nbasim"))
library(nbasim)
set.seed(1)

# 1. Play-by-play of one full game
pbp <- sim_pbp()
print(head(pbp, 10))
cat("Final:", tail(pbp$score1, 1), "-", tail(pbp$score2, 1), "\n\n")

# 2. A late-game spot: down 2 with the ball, 0:40 left in Q4, opponent in the bonus
state <- game_state(period = 4, secs_left = 40, off_score = 101, def_score = 103,
                    def_fouls = 5, def_fouls_l2 = 1)

print(sim_pbp(state, until = "possession"))  # just the next possession
print(sim_pbp(state))                        # play out the rest of the game

# 3. Force an action: team 1 shoots a 3, then the model plays team 2's next possession
print(sim_pbp(state, until = "possession", action = "fg3a", possessions = 2))
print(head(sim_paths(state, action = "fg3a"), 8))   # how often each path happens + win prob

# 4. eWP of each choice: let the model decide, take a 2, take a 3
print(compare_actions(state))

# 5. Win probability from that spot (10,000 simulations)
cat("\nWin probability: sim", win_prob(state, n = 10000), "| neural net", nn_win_prob(state), "\n")

# 6. League-average game distribution (1,000 full games)
games <- sim_games(1000)
cat("Avg total:", mean(games$score1 + games$score2),
    "| Avg margin:", mean(abs(games$score1 - games$score2)), "\n")
