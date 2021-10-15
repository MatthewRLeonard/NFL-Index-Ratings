# 1. Load Packages ----

library(nflfastR)
library(nflseedR)
library(future)
library(tidyverse)
#library(gt)

# 2. Load Data ----

future::plan("multisession")
games <- nflfastR::load_pbp(2014:2021)

divs <- nflseedR::divisions |>
  dplyr::select(-sdiv) |>
  dplyr::mutate(div = stringr::str_sub(division, start = 5))

teams <- nflfastR::teams_colors_logos |>
  dplyr::select(team_abbr, team_name, team_logo_espn) |>
  dplyr::rename(team = "team_abbr") |>
  dplyr::full_join(divs)

# 3. Pre-Processing ----

# create variable to change drives kickoff
games <- games |>
  dplyr::filter(!is.na(play_type)) |>
  dplyr::mutate(drive_off_play_id_started = 
           if_else(play_type == "kickoff", dplyr::lead(play_id, n = 1), 
                   if_else(lag(play_type == "kickoff" & touchdown == 1, n = 1), dplyr::lag(play_id, n = 1),
                           if_else(lag(play_type == "kickoff", n = 1), play_id, drive_play_id_started, drive_play_id_started)))
  ) |>
  dplyr::relocate("drive_off_play_id_started", .after = "drive_play_id_started")

# 4. Game PBP Example ----
# games |>
#   filter(pass == 1, complete_pass == 1, posteam %in% c("BUF")) |>
#   select(week, play_id, posteam, drive, score_differential_post, yardline_100, down, ydstogo, play_type, yards_gained, air_yards, yards_after_catch, touchdown, ydsnet, drive_start_yard_line, drive_end_yard_line, drive_play_id_started, drive_off_play_id_started, drive_play_id_ended, drive_end_transition, ep, epa, wpa, away_wp_post, drive_play_count) |>
#   #filter(play_id == drive_off_play_id_started) |>
#   gt() |>
#     cols_label(
#       play_id = "id",
#       posteam = "ball",
#       score_differential_post = "score",
#       yardline_100 = "endzone",
#       ydstogo = "to go",
#       play_type = "play",
#       yards_gained = "yards",
#       air_yards = "air yards",
#       yards_after_catch = "YAC",
#       drive_start_yard_line = "start",
#       drive_end_yard_line = "end",
#       drive_play_id_started = "id_1",
#       drive_off_play_id_started = "id_2",
#       drive_play_id_ended = "id_3",
#       drive_end_transition = "how",
#       away_wp_post = "wp"
#     ) |>
#     fmt_number(
#       columns = 21:22,
#       decimals = 2
#     ) |>
#     fmt_percent(
#       columns = 23:24,
#       decimals = 1
#     )

# 5. Analysis ----

## Available Off Yards % ----
off_game_yards <- games |>
  dplyr::group_by(season, posteam, week) |>
  dplyr::filter(!is.na(posteam)) |>
  dplyr::filter(!drive_end_transition %in% c("END_HALF", "END_GAME")) |>
  dplyr::mutate(game_plays = sum(pass) + sum(rush)) |>
  dplyr::filter(play_id == drive_off_play_id_started) |>
  dplyr::summarise(game_drives = n(),
            game_plays = game_plays,
            game_available_yards = sum(yardline_100),
            game_yards_gained = sum(ydsnet),
            game_plays_per_drive = game_plays / game_drives,
            game_yards_per_play = game_yards_gained / game_plays,
            game_available_yards_perc = game_yards_gained / game_available_yards
   ) |> dplyr::distinct()

## Available Def Yards % ----
def_game_yards <- games |>
  group_by(season, defteam, week) |>
  filter(!is.na(defteam)) |>
  filter(!drive_end_transition %in% c("END_HALF", "END_GAME")) |>
  mutate(game_plays = sum(pass) + sum(rush)) |>
  filter(play_id == drive_off_play_id_started) |>
  summarise(game_drives = n(),
            game_plays = game_plays,
            game_available_yards = sum(yardline_100),
            game_yards_gained = sum(ydsnet),
            game_plays_per_drive = game_plays / game_drives,
            game_yards_per_play = game_yards_gained / game_plays,
            game_available_yards_perc = game_yards_gained / game_available_yards
  ) |> distinct()

## Season Off Yards ----
off_season_yards <- off_game_yards |>
  group_by(season, posteam) |>
  summarise(season_drives = sum(game_drives),
            season_plays = sum(game_plays),
            season_available_yards = sum(game_available_yards),
            season_yards_gained = sum(game_yards_gained),
            season_plays_per_drive = season_plays / season_drives,
            season_yards_per_play = season_yards_gained / season_plays,
            season_available_yards_perc = season_yards_gained / season_available_yards
  ) |>
  rename_with(function(x) paste0("off_", x)) |>
  rename(team = "off_posteam", season = "off_season")

## Season Def Yards ----
def_season_yards <- def_game_yards |>
  group_by(season, defteam) |>
  summarise(season_drives = sum(game_drives),
            season_plays = sum(game_plays),
            season_available_yards = sum(game_available_yards),
            season_yards_gained = sum(game_yards_gained),
            season_plays_per_drive = season_plays / season_drives,
            season_yards_per_play = season_yards_gained / season_plays,
            season_available_yards_perc = season_yards_gained / season_available_yards
  ) |>
  rename_with(function(x) paste0("def_", x)) |>
  rename(team = "def_defteam", season = "def_season")

## Off Passing ----   
off_passing <- games |>
  group_by(season, posteam) |>
  filter(!drive_end_transition %in% c("END_HALF", "END_GAME")) |>
  filter(play_type == "pass") |>
  summarise(num_plays = sum(play_type == "pass", na.rm = TRUE),
            epa_pp = sum(epa, na.rm = TRUE) / num_plays,
            success_rate = sum(success, na.rm = TRUE) / num_plays,
            explosiveness = sum(yards_gained[yards_gained >= 20], na.rm = TRUE) / sum(yards_gained, na.rm = TRUE),
            negatives = sum(yards_gained < 0) / num_plays,
            big_plays = sum(yards_gained >= 20) / num_plays,
            air_yards_pp = sum(air_yards, na.rm = TRUE) / num_plays,
            completions = sum(complete_pass == 1, na.rm = TRUE),
            air_yards_pc = sum(air_yards[complete_pass == 1], na.rm = TRUE) / completions,
            yac_pc = sum(yards_after_catch[complete_pass == 1], na.rm = TRUE) / completions,
            ypc = sum(yards_gained[complete_pass == 1], na.rm = TRUE) / completions,
            comp_perc = completions / num_plays,
            ypa = sum(yards_gained, na.rm = TRUE) / num_plays
  ) |>
  rename_with(function(x) paste0("off_pass_", x)) |>
  rename(team = "off_pass_posteam", season = "off_pass_season")

## Def Passing ----     
def_passing <- games |>
  group_by(season, defteam) |>
  filter(!drive_end_transition %in% c("END_HALF", "END_GAME")) |>
  filter(play_type == "pass") |>
  summarise(num_plays = sum(play_type == "pass", na.rm = TRUE),
            epa_pp = sum(epa, na.rm = TRUE) / num_plays,
            success_rate = sum(success, na.rm = TRUE) / num_plays,
            explosiveness = sum(yards_gained[yards_gained >= 20], na.rm = TRUE) / sum(yards_gained, na.rm = TRUE),
            negatives = sum(yards_gained < 0) / num_plays,
            big_plays = sum(yards_gained >= 20) / num_plays,
            air_yards_pp = sum(air_yards, na.rm = TRUE) / num_plays,
            completions = sum(complete_pass == 1, na.rm = TRUE),
            air_yards_pc = sum(air_yards[complete_pass == 1], na.rm = TRUE) / completions,
            yac_pc = sum(yards_after_catch[complete_pass == 1], na.rm = TRUE) / completions,
            ypc = sum(yards_gained[complete_pass == 1], na.rm = TRUE) / completions,
            comp_perc = completions / num_plays,
            ypa = sum(yards_gained, na.rm = TRUE) / num_plays
  ) |>
  rename_with(function(x) paste0("def_pass_", x)) |>
  rename(team = "def_pass_defteam", season = "def_pass_season")

## Off Rushing ----  
off_rushing <- games |>
  group_by(season, posteam) |>
  filter(!drive_end_transition %in% c("END_HALF", "END_GAME")) |>
  filter(play_type == "run") |>
  summarise(num_plays = sum(play_type == "run", na.rm = TRUE),
            epa_pp = sum(epa, na.rm = TRUE) / num_plays,
            success_rate = sum(success, na.rm = TRUE) / num_plays,
            explosiveness = sum(yards_gained[yards_gained >= 10], na.rm = TRUE) / sum(yards_gained, na.rm = TRUE),
            negatives = sum(yards_gained < 0) / num_plays,
            big_plays = sum(yards_gained >= 10) / num_plays,
            ypc = sum(yards_gained, na.rm = TRUE) / num_plays
  ) |>
  rename_with(function(x) paste0("off_rush_", x)) |>
  rename(team = "off_rush_posteam", season = "off_rush_season")

## Def Passing ----  
def_rushing <- games |>
  group_by(season, defteam) |>
  filter(!drive_end_transition %in% c("END_HALF", "END_GAME")) |>
  filter(play_type == "run") |>
  summarise(num_plays = sum(play_type == "run", na.rm = TRUE),
            epa_pp = sum(epa, na.rm = TRUE) / num_plays,
            success_rate = sum(success, na.rm = TRUE) / num_plays,
            explosiveness = sum(yards_gained[yards_gained >= 10], na.rm = TRUE) / sum(yards_gained, na.rm = TRUE),
            negatives = sum(yards_gained < 0) / num_plays,
            big_plays = sum(yards_gained >= 10) / num_plays,
            ypc = sum(yards_gained, na.rm = TRUE) / num_plays
  ) |>
  rename_with(function(x) paste0("def_rush_", x)) |>
  rename(team = "def_rush_defteam", season = "def_rush_season")

## Team Offense ----
off_team_stats <- off_passing |>
  full_join(off_rushing) |>
  mutate(off_total_plays = off_pass_num_plays + off_rush_num_plays,
         off_epa_pp = ((off_pass_num_plays * off_pass_epa_pp) + (off_rush_num_plays * off_rush_epa_pp)) / off_total_plays,
         off_success_rate = ((off_pass_num_plays * off_pass_success_rate) + (off_rush_num_plays * off_rush_success_rate)) / off_total_plays,
         off_explosiveness = ((off_pass_num_plays * off_pass_explosiveness) + (off_rush_num_plays * off_rush_explosiveness)) / off_total_plays,
         off_negatives = ((off_pass_num_plays * off_pass_negatives) + (off_rush_num_plays * off_rush_negatives)) / off_total_plays
         ) |>
  relocate(23:27, .after = "team")

## Team Defense ----
def_team_stats <- def_passing |>
  full_join(def_rushing) |>
  mutate(def_total_plays = def_pass_num_plays + def_rush_num_plays,
         def_epa_pp = ((def_pass_num_plays * def_pass_epa_pp) + (def_rush_num_plays * def_rush_epa_pp)) / def_total_plays,
         def_success_rate = ((def_pass_num_plays * def_pass_success_rate) + (def_rush_num_plays * def_rush_success_rate)) / def_total_plays,
         def_explosiveness = ((def_pass_num_plays * def_pass_explosiveness) + (def_rush_num_plays * def_rush_explosiveness)) / def_total_plays,
         def_negatives = ((def_pass_num_plays * def_pass_negatives) + (def_rush_num_plays * def_rush_negatives)) / def_total_plays
  ) |>
  relocate(23:27, .after = "team")
     
## Team Stats ----
team_stats <- off_season_yards |>
  full_join(def_season_yards) |>
  full_join(off_team_stats) |>
  full_join(def_team_stats) |>
  left_join(select(teams, team, team_name, conf, division, div, team_logo_espn)) |>
  relocate(c("off_total_plays", "off_epa_pp", "off_success_rate", "off_explosiveness", "off_negatives", "def_total_plays", "def_epa_pp", "def_success_rate", "def_explosiveness", "def_negatives"), .after = "team") |>
  relocate(c("team_name", "conf", "division", "div", "team_logo_espn"), .after = "team")

## Net Stats ----
team_stats <- team_stats |>
  mutate(net_epa_pp = off_epa_pp - def_epa_pp,
         net_success_rate = off_success_rate - def_success_rate,
         net_yards_per_play = off_season_yards_per_play - def_season_yards_per_play,
         net_available_yards_perc = off_season_available_yards_perc - def_season_available_yards_perc,
         net_explosiveness = off_explosiveness - def_explosiveness,
         net_negatives = def_negatives - off_negatives
  ) |>
  relocate(c("net_epa_pp", "net_success_rate", "net_yards_per_play", "net_available_yards_perc", "net_explosiveness", "net_negatives"), .after = "team_logo_espn")

## Team Ratings ----
team_stats <- team_stats |>
  dplyr::mutate(scaled_epa_pp = scales::rescale(net_epa_pp, to = c(0, 0.2), from = range(net_epa_pp)),
         scaled_success_rate = scales::rescale(net_success_rate, to = c(0, 0.2), from = range(net_success_rate)),
         scaled_yards_per_play = scales::rescale(net_yards_per_play, to = c(0, 0.2), from = range(net_yards_per_play)),
         scaled_available_yards_perc = scales::rescale(net_available_yards_perc, to = c(0, 0.2), from = range(net_available_yards_perc)),
         scaled_explosiveness = scales::rescale(net_explosiveness, to = c(0, 0.1), from = range(net_explosiveness)),
         scaled_negatives = scales::rescale(net_negatives, to = c(0, 0.1), from = range(net_negatives)),
         net_rating = scaled_epa_pp + scaled_success_rate + scaled_explosiveness + scaled_negatives + scaled_yards_per_play + scaled_available_yards_perc,
         scaled_off_epa_pp = scales::rescale(off_epa_pp, to = c(0, 0.2), from = range(off_epa_pp)),
         scaled_off_success_rate = scales::rescale(off_success_rate, to = c(0, 0.2), from = range(off_success_rate)),
         scaled_off_yards_per_play = scales::rescale(off_season_yards_per_play, to = c(0, 0.2), from = range(off_season_yards_per_play)),
         scaled_off_available_yards_perc = scales::rescale(off_season_available_yards_perc, to = c(0, 0.2), from = range(off_season_available_yards_perc)),
         scaled_off_explosiveness = scales::rescale(off_explosiveness, to = c(0, 0.1), from = range(off_explosiveness)),
         scaled_off_negatives = scales::rescale(off_negatives, to = c(0.1, 0), from = range(off_negatives)),
         off_rating = scaled_off_epa_pp + scaled_off_success_rate + scaled_off_explosiveness + scaled_off_negatives + scaled_off_yards_per_play + scaled_off_available_yards_perc,
         scaled_def_epa_pp = scales::rescale(def_epa_pp, to = c(0.2, 0), from = range(def_epa_pp)),
         scaled_def_success_rate = scales::rescale(def_success_rate, to = c(0.2, 0), from = range(def_success_rate)),
         scaled_def_yards_per_play = scales::rescale(def_season_yards_per_play, to = c(0.2, 0), from = range(def_season_yards_per_play)),
         scaled_def_available_yards_perc = scales::rescale(def_season_available_yards_perc, to = c(0.2, 0), from = range(def_season_available_yards_perc)),
         scaled_def_explosiveness = scales::rescale(def_explosiveness, to = c(0.1, 0), from = range(def_explosiveness)),
         scaled_def_negatives = scales::rescale(def_negatives, to = c(0, 0.1), from = range(def_negatives)),
         def_rating = scaled_def_epa_pp + scaled_def_success_rate + scaled_def_explosiveness + scaled_def_negatives + scaled_def_yards_per_play + scaled_def_available_yards_perc
  ) |>
  dplyr::select(-dplyr::starts_with("scaled_")) |>
  dplyr::relocate(c("net_rating", "off_rating", "def_rating"), .after = "team_logo_espn") |>
  dplyr::arrange(dplyr::desc(net_rating))

# 6. Export team_stats to CSV ----

localfile <- "C:/Users/mrl394/OneDrive - The University of Newcastle/Data/Projects/NFL-Index-Ratings/team_stats.csv"

write.csv(team_stats, localfile, row.names = FALSE)

# 7. Upload CSV to Google Drive ----

googledrive::drive_put(localfile, "Data/Projects/NFL-Index-Ratings/") |>
  googledrive::drive_share_anyone()
