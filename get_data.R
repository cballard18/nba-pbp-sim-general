# Download regular-season play-by-play from nbadatar and cache it locally.
library(nbadatar)
library(data.table)
library(arrow)

seasons <- 2017:2026  # 2016 lacks off_team_abb (offense) so it is not usable

pbp <- as.data.table(nbadatar::load_pbp(seasons = seasons, season_type = "regular"))

keep <- c("season", "game_id", "number_event", "msg_type", "act_type", "period", "clock",
          "team_abb", "off_team_abb", "description", "hs", "vs", "total_fta", "and1",
          "team_home", "team_away")
pbp <- pbp[, ..keep]

dir.create("data", showWarnings = FALSE)
write_parquet(pbp, "data/pbp.parquet")
cat("saved", nrow(pbp), "rows,", uniqueN(pbp$game_id), "games\n")
