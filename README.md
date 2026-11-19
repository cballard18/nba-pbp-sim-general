# NBA possession simulator (league average)

A Monte Carlo game simulator built from a chain of small ML models: the same "ML sub-models + Monte Carlo loop" design as DraftKings' football model, applied to NBA possessions. Teams and players are not modeled: every team plays like the 2016-17 to 2025-26 league average.

## Run order

```
Rscript get_data.R        # nbadatar play-by-play -> data/pbp.parquet           (~1 min)
Rscript build_segments.R  # pbp -> data/segments.parquet (one row per segment)  (~20 s)
Rscript fit_models.R      # fit sub-models -> models/                            (~4 min)
Rscript example.R         # quick tour: pbp, next possession, finish a game, win prob (~10 s)
Rscript validate.R        # compare simulated vs real games -> output/           (~10 min)
```

## The unit: a segment

A **segment** is a team's turn with the ball until one of these ends it: a shot, a turnover, a defensive foul, or the end of the period. An offensive rebound or a non-bonus (side-out) foul starts a new segment for the same team. Each row stores the game state at the start of the segment:

| feature | meaning |
|---|---|
| `period`, `secs_left`, `game_secs_left` | clock |
| `score_diff` | offense minus defense |
| `start_type` | period_start / after_make / after_dreb / after_oreb / after_turnover / sideout |
| `def_bonus` | defense is in the penalty (NBA rules, verified against the data at 99.6%) |
| `after_timeout` | offense called a timeout during the segment |
| `off_to_left` | offense's timeouts left (7 per game, max 4 in Q4 and 2 after 3:00, 2 per OT) |

## The chain (one loop step = one segment)

1. **Timeout?** xgboost, binary. Only allowed if the team has a timeout left.
2. **Outcome**, xgboost with 11 classes: 2PT/3PT make/miss, 2PT/3PT and-1, shooting foul (2 or 3 FTs), non-shooting foul, turnover, end of period.
3. **Duration** given the outcome: xgboost over 0–30 seconds, truncated so it can't exceed the clock.
4. **Free throws:** league FT% (77.5%). A non-shooting foul gives 2 FTs when the defense is in the bonus, otherwise a side-out.
5. **Rebound** after a missed FG or a missed final FT: league offensive rebound rate by shot type (2PT 32%, 3PT 25%, FT 12%).
6. Update score, fouls, timeouts and possession. Handle period ends: Q2/Q3 ball to the team that lost the opening tip, Q4 to the winner, OT starts with a jump ball.

Close late-game rows (Q4/OT, ≤ 3:00 left, margin ≤ 10) get 10× training weight, because they are rare but decide close games. Without the weighting, the sim sent too few close games to overtime.

## Using the simulator

```r
source("sim.R")
m <- load_models()

# a game state, from the point of view of the team with the ball (team 1 in all output)
state <- game_state(period = 4, secs_left = 40, off_score = 101, def_score = 103,
                    def_fouls = 5, def_fouls_l2 = 1)

# play-by-play for a single game
sim_pbp(m)                                 # full game from tip-off
sim_pbp(m, state)                          # finish the game from the state
sim_pbp(m, state, until = "possession")    # just the next possession (ends when the ball changes teams)

# many games at once (fast: all games are advanced together)
res <- game_summary(simulate_games(new_games(10000), m))       # 10k full games
g   <- simulate_games(games_from_states(state, reps = 10000), m)
mean(g$score[, 1] > g$score[, 2])          # win probability for the team with the ball
g   <- simulate_games(games_from_states(state, 10000), m, until = "possession", pbp = TRUE)
g$pbp                                      # pbp for 10k simulated next possessions (column `game`)
```

The `pbp` columns are: period, clock (after the event), offense (1/2), start_type, timeout, duration, event, fta/ftm, rebound, points, score1, score2.

## Known simplifications

- **No home-court advantage.** No team or player strength, and no era adjustment (all seasons pooled). Game-to-game spread in totals and pace is therefore smaller than in real games.
- **2015-16 is excluded:** its play-by-play has no offense column.
- **FT% and offensive rebound rates are constants.** Late-game intentional misses aren't modeled.
- **Ignored entirely:** technical fouls (about 0.5 points per game), jump-ball possession changes, and defensive timeouts.
- **Rule changes are pooled.** Timeout rules before 2017-18 differ, so the timeout model trains on 2017-18 onward only. The 14-second offensive-rebound shot-clock reset (2018-19) is not separated out.
