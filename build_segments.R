# Turn raw play-by-play into one row per segment: a team's turn with the ball that ends in a
# shot, turnover, defensive foul, or the end of the period. An offensive rebound or a side-out
# foul starts a new segment for the same team.
source("common.R")

p <- as.data.table(read_parquet("data/pbp.parquet"))
p <- p[season >= 2017]  # 2016 has no off_team_abb
setorder(p, game_id, number_event)

p[, period := as.integer(period)]
p[, secs_left := as.numeric(sub(":.*", "", clock)) * 60 + as.numeric(sub(".*:", "", clock))]
p[, c("hs", "vs") := lapply(.SD, function(x) nafill(nafill(x, "locf"), fill = 0)),
  by = game_id, .SDcols = c("hs", "vs")]
p[, `:=`(hs_before = shift(hs, fill = 0), vs_before = shift(vs, fill = 0)), by = game_id]
p[, is3 := grepl("3pt", description, ignore.case = TRUE)]
p[, def_foul := msg_type == 6 & !is.na(team_abb) & team_abb != off_team_abb]

# ---- running team-foul and timeout counts (per side), used for bonus + timeouts-left ----
p[, team_foul := msg_type == 6 & act_type %in% TEAM_FOUL_ACTS & !is.na(team_abb)]
p[, timeout := msg_type == 9 & act_type %in% c(1, 2) & !is.na(team_abb)]
for (side in c("home", "away")) {
  mine <- p$team_abb == p[[paste0("team_", side)]] & !is.na(p$team_abb)
  p[, f   := team_foul & mine]
  p[, t   := timeout & mine]
  p[, paste0(side, c("_fouls", "_fouls_l2", "_to_per", "_to_l3")) :=
        list(cumsum(f), cumsum(f & secs_left <= 120), cumsum(t), cumsum(t & secs_left <= 180)),
    by = .(game_id, period)]
  p[, paste0(side, c("_to_reg", "_to_all")) := list(cumsum(t & period <= 4), cumsum(t)), by = game_id]
}
p[, c("f", "t") := NULL]

# ---- terminal events ----
p[, outcome := fcase(
  msg_type == 1 & and1 %in% 1, fifelse(is3, "fg3_and1", "fg2_and1"),
  msg_type == 1,               fifelse(is3, "fg3_make", "fg2_make"),
  msg_type == 2,               fifelse(is3, "fg3_miss", "fg2_miss"),
  msg_type == 5 & act_type != 0, "turnover",
  def_foul & act_type %in% SHOOT_ACTS & !(and1 %in% 1),
                               fifelse(total_fta %in% 3, "foul_shoot3", "foul_shoot2"),
  def_foul & act_type %in% NONSHOOT_ACTS, "foul_nonshoot",
  msg_type == 13,              "end_period",
  default = NA_character_)]

# Free throws (not technicals) belong to the most recent terminal event
p[, boundary := msg_type == 12 | !is.na(outcome)]
p[, seg := cumsum(boundary), by = game_id]
ft <- p[msg_type == 3 & !act_type %in% c(16, 21, 22),
        .(n_ft = .N, last_ft_made = !grepl("miss", description[.N], ignore.case = TRUE)),
        by = .(game_id, seg)]

# ---- one row per boundary; state at segment start = values at the previous boundary ----
b <- p[boundary == TRUE]
b <- ft[b, on = .(game_id, seg)]
b[is.na(n_ft), `:=`(n_ft = 0L, last_ft_made = NA)]
b[, offense := off_team_abb]
b[, off_home := offense == team_home]

start_cols <- c("secs_left", grep("^(home|away)_(fouls|to)", names(b), value = TRUE))
b[, paste0("s_", start_cols) := shift(.SD), by = .(game_id, period), .SDcols = start_cols]
b[, prev_offense := shift(offense), by = .(game_id, period)]
b[, prev_outcome := shift(outcome), by = .(game_id, period)]
b[, prev_n_ft := shift(n_ft), by = .(game_id, period)]
b[, prev_last_ft := shift(last_ft_made), by = .(game_id, period)]
b[, first_in_period := is.na(prev_outcome)]

seg <- b[!is.na(outcome) & !is.na(offense)]
seg[is.na(s_secs_left), s_secs_left := period_length(period)]
for (col in grep("^s_(home|away)", names(seg), value = TRUE)) seg[is.na(get(col)), (col) := 0]

pick <- function(home_val, away_val, home) fifelse(home, home_val, away_val)
to_left <- function(period, secs, reg, per, l3) {
  left <- fcase(period <= 3, 7 - reg,
                period == 4, pmin(7 - reg, 4 - per, fifelse(secs <= 180, 2 - l3, 99)),
                default = 2 - per)
  pmax(left, 0)
}

seg[, `:=`(
  secs_left     = s_secs_left,
  end_secs      = secs_left,
  duration      = s_secs_left - secs_left,
  score_diff    = pick(hs_before - vs_before, vs_before - hs_before, off_home),
  off_fouls     = pick(s_home_fouls, s_away_fouls, off_home),
  off_fouls_l2  = pick(s_home_fouls_l2, s_away_fouls_l2, off_home),
  def_fouls     = pick(s_away_fouls, s_home_fouls, off_home),
  def_fouls_l2  = pick(s_away_fouls_l2, s_home_fouls_l2, off_home),
  off_to_left   = to_left(period, s_secs_left, pick(s_home_to_reg, s_away_to_reg, off_home),
                          pick(s_home_to_per, s_away_to_per, off_home), pick(s_home_to_l3, s_away_to_l3, off_home)),
  def_to_left   = to_left(period, s_secs_left, pick(s_away_to_reg, s_home_to_reg, off_home),
                          pick(s_away_to_per, s_home_to_per, off_home), pick(s_away_to_l3, s_home_to_l3, off_home)),
  after_timeout = pick(home_to_all - s_home_to_all, away_to_all - s_away_to_all, off_home) > 0
)]
# timeouts called at this terminal row's index are after it; the _to_all at the terminal row
# only counts rows up to and including it, so after_timeout covers the segment exactly.
seg[, def_bonus := in_bonus(period, secs_left, def_fouls, def_fouls_l2)]

# How the previous segment ended -> how this one started
seg[, prev_kind := fcase(
  prev_outcome %in% c("fg2_make", "fg3_make"), "make",
  prev_outcome %in% c("fg2_miss", "fg3_miss"), "miss",
  prev_outcome == "turnover", "turnover",
  prev_n_ft > 0, fifelse(prev_last_ft, "make", "miss"),
  prev_outcome == "foul_nonshoot", "sideout",
  default = "make")]
seg[, same := !is.na(prev_offense) & offense == prev_offense]
seg[, start_type := fcase(
  first_in_period, "period_start",
  prev_kind == "miss", fifelse(same, "after_oreb", "after_dreb"),
  prev_kind == "make" & !same, "after_make",
  prev_kind == "turnover" & !same, "after_turnover",
  same, "sideout",
  default = "after_turnover")]

# Offensive rebound label for misses (FG or last FT): does the same team have the next segment?
seg[, next_offense := shift(offense, -1), by = .(game_id, period)]
seg[, shot_kind := fcase(outcome %in% c("fg2_miss"), "fg2",
                         outcome %in% c("fg3_miss"), "fg3",
                         n_ft > 0 & last_ft_made == FALSE, "ft",
                         default = NA_character_)]
seg[, oreb := fifelse(is.na(shot_kind) | is.na(next_offense), NA, next_offense == offense)]

# Game result from the offense's point of view (for win-probability validation)
final <- p[, .(final_hs = hs[.N], final_vs = vs[.N]), by = game_id]
seg <- final[seg, on = "game_id"]
seg[, final_margin := pick(final_hs - final_vs, final_vs - final_hs, off_home)]
seg[, final_total := final_hs + final_vs]
seg[, tip_team := offense[period == 1][1], by = game_id]
seg[, tip_off := offense == tip_team]

# Drop period-end rows that are just the buzzer after a final event, and bad clock rows
seg <- seg[!(outcome == "end_period" & duration <= 0)]
seg <- seg[duration >= 0 & duration <= 60 & secs_left <= period_length(period)]

keep <- c("season", "game_id", "period", "secs_left", "end_secs", "score_diff", "start_type",
          "def_bonus", "after_timeout", "off_to_left", "def_to_left", "off_fouls", "off_fouls_l2",
          "def_fouls", "def_fouls_l2", "outcome", "duration", "n_ft", "last_ft_made", "shot_kind",
          "oreb", "final_margin", "final_total", "tip_off")
seg <- seg[, ..keep]
write_parquet(seg, "data/segments.parquet")

# FT make rate (all non-technical free throws)
fts <- p[msg_type == 3 & !act_type %in% c(16, 21, 22)]
saveRDS(list(ft_pct = fts[, mean(!grepl("miss", description, ignore.case = TRUE))]), "data/constants.rds")

cat("segments:", nrow(seg), " games:", uniqueN(seg$game_id), "\n")
print(round(prop.table(table(seg$outcome)), 4))
print(round(prop.table(table(seg$start_type)), 4))
