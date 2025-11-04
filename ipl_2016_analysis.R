# ─────────────────────────────────────────────────────────────────────────────
# Project: IPL 2016 Analysis
# Author:  Ankasandra Naveen Kumar Karthik
# Class:   ALY6000 Introduction To Analytics
# Date:    2023-10-09
#
# Folder layout expected:
#   ./data/matches.csv
#   ./data/deliveries.csv
# Run this file from the project root: source("ipl_2016_analysis.R")
# ─────────────────────────────────────────────────────────────────────────────

# 0) Housekeeping & packages ---------------------------------------------------
cat("\014")
try(grDevices::dev.off(), silent = TRUE)
options(scipen = 100)

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(ggeasy)
  library(ggthemes)
})

# 1) Load raw CSVs from ./data -------------------------------------------------
matches_path    <- file.path("data", "matches.csv")
deliveries_path <- file.path("data", "deliveries.csv")

if (!file.exists(matches_path))    stop("Missing file: data/matches.csv")
if (!file.exists(deliveries_path)) stop("Missing file: data/deliveries.csv")

deliveries <- read.csv(deliveries_path, stringsAsFactors = FALSE)
matches    <- read.csv(matches_path,    stringsAsFactors = FALSE)

# 2) Clean names/types ---------------------------------------------------------
deliveries <- clean_names(deliveries)
matches    <- clean_names(matches)
matches$date <- as.Date(matches$date)

# 3) Filter to IPL 2016 (via match IDs) ---------------------------------------
match_ids_2016 <- matches %>% filter(season == 2016) %>% pull(id)
if (length(match_ids_2016) == 0) stop("No matches found for season 2016.")

deliv_2016   <- deliveries %>% filter(match_id %in% match_ids_2016)
matches_2016 <- matches    %>% filter(id %in% match_ids_2016)

# 4) Batting metrics -----------------------------------------------------------
per_innings_score <- deliv_2016 %>%
  group_by(match_id, batsman) %>%
  summarise(score = sum(batsman_runs, na.rm = TRUE), .groups = "drop")

dismissals_by_batsman <- deliv_2016 %>%
  filter(!is.na(player_dismissed), player_dismissed != "") %>%
  count(player_dismissed, name = "dismissals") %>%
  rename(batsman = player_dismissed)

batting_stats <- deliv_2016 %>%
  group_by(batsman) %>%
  summarise(
    matches_played = n_distinct(match_id),
    total_runs     = sum(batsman_runs, na.rm = TRUE),
    balls_faced    = n(),
    fours          = sum(batsman_runs == 4, na.rm = TRUE),
    sixes          = sum(batsman_runs == 6, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(dismissals_by_batsman, by = "batsman") %>%
  mutate(
    dismissals  = replace_na(dismissals, 0L),
    batting_avg = round(ifelse(dismissals == 0, NA_real_, total_runs / dismissals), 2),
    strike_rate = round(ifelse(balls_faced == 0, NA_real_, total_runs / balls_faced * 100), 2)
  ) %>%
  arrange(desc(total_runs)) %>%
  rename(player = batsman)

# 5) Bowling metrics -----------------------------------------------------------
credited_kinds <- c("stumped","bowled","caught","lbw","hit wicket","caught and bowled")

per_innings_wickets <- deliv_2016 %>%
  filter(dismissal_kind %in% credited_kinds) %>%
  group_by(match_id, bowler) %>%
  summarise(wickets = n(), .groups = "drop")

wickets_by_bowler <- deliv_2016 %>%
  filter(dismissal_kind %in% credited_kinds) %>%
  count(bowler, name = "wickets")

bowling_stats <- deliv_2016 %>%
  group_by(bowler) %>%
  summarise(
    matches_played = n_distinct(match_id),
    total_runs     = sum(batsman_runs, na.rm = TRUE),  # proxy for conceded runs
    balls_bowled   = n(),
    .groups = "drop"
  ) %>%
  left_join(wickets_by_bowler, by = "bowler") %>%
  mutate(
    wickets = replace_na(wickets, 0L),
    overs   = round(balls_bowled / 6, 1),
    economy = round(ifelse(balls_bowled == 0, NA_real_, total_runs / (balls_bowled / 6)), 2)
  ) %>%
  arrange(desc(wickets)) %>%
  rename(player = bowler)

# 6) Team table (wins / losses / points) --------------------------------------
wins_tbl <- matches_2016 %>%
  filter(!is.na(winner), winner != "") %>%
  count(winner, name = "wins") %>%
  rename(team = winner)

played_tbl <- matches_2016 %>%
  select(team1, team2) %>%
  pivot_longer(everything(), names_to = "slot", values_to = "team") %>%
  count(team, name = "played")

points_table <- played_tbl %>%
  left_join(wins_tbl, by = "team") %>%
  mutate(
    wins   = replace_na(wins, 0L),
    loss   = pmax(played - wins, 0L),
    points = wins * 2L
  ) %>%
  arrange(desc(points))

# 7) MVP scoring (custom rules) -----------------------------------------------
batsman_points <- batting_stats %>%
  transmute(
    player,
    runs_points = total_runs * 0.5,
    sr_points   = case_when(
      strike_rate >= 150 ~ 15,
      strike_rate >= 120 ~ 10,
      TRUE               ~ 1
    ),
    total_points = runs_points + sr_points
  )

bowler_points <- bowling_stats %>%
  transmute(
    player,
    wickets_points = wickets * 10,
    eco_points     = case_when(
      economy <= 6.50 ~ 7.5,
      economy <= 8.00 ~ 5,
      TRUE            ~ 1
    ),
    total_points = wickets_points + eco_points
  )

mvp_players <- bind_rows(batsman_points, bowler_points) %>%
  group_by(player) %>%
  summarise(total_points = sum(total_points), .groups = "drop") %>%
  arrange(desc(total_points)) %>%
  filter(total_points >= 200)

# 8) Visualizations ------------------------------------------------------------

# MVP bar chart
p_mvp <- ggplot(mvp_players, aes(x = reorder(player, -total_points), y = total_points)) +
  geom_col(fill = "skyblue") +
  labs(title = "Players of the Tournament (Custom Scoring)", x = "Players", y = "Points Earned") +
  theme_bw() +
  easy_rotate_x_labels(angle = 45) +
  theme(axis.text.x = element_text(hjust = 1))

# Batting Average vs Strike Rate
ba_sr <- batting_stats %>% filter(is.finite(batting_avg), !is.na(strike_rate))
p_ba_sr <- ggplot(ba_sr, aes(x = batting_avg, y = strike_rate)) +
  geom_point(aes(colour = player)) +
  labs(title = "Batting Average vs Strike Rate", x = "Batting Average", y = "Strike Rate") +
  theme_minimal()

# Virat Kohli progression
virat_kohli <- per_innings_score %>%
  filter(batsman == "V Kohli") %>%
  arrange(match_id) %>%
  mutate(
    match_id = row_number(),
    cum_score = cumsum(score)
  )

p_kohli_scores <- ggplot(virat_kohli, aes(x = match_id, y = score)) +
  geom_point(colour = "blue") +
  geom_line(colour = "skyblue") +
  labs(title = "Innings Scores Of Virat Kohli", x = "Match Number", y = "Innings Score") +
  theme_economist_white()

p_kohli_box <- ggplot(virat_kohli, aes(x = score)) +
  geom_boxplot(colour = "red", fill = "orange") +
  labs(title = "Overview of Scores of Virat Kohli", x = "Scores", y = NULL) +
  theme_minimal()

p_kohli_pareto <- ggplot(virat_kohli, aes(x = match_id, y = score)) +
  geom_col(fill = "skyblue") +
  geom_point(aes(y = cum_score), colour = "red") +
  geom_line(aes(y = cum_score), colour = "orange") +
  labs(title = "Progression Of Virat Kohli in IPL 2016", x = "Match Number", y = "Runs / Cumulative Runs") +
  theme_clean()

# Kohli vs Warner cumulative comparison
da_warner <- per_innings_score %>%
  filter(batsman == "DA Warner") %>%
  arrange(match_id) %>%
  mutate(
    match_id  = row_number(),
    cum_score = cumsum(score)
  )

p_kohli_vs_warner <- ggplot() +
  geom_line(data = virat_kohli, aes(x = match_id, y = cum_score, colour = "Virat Kohli")) +
  geom_point(data = virat_kohli, aes(x = match_id, y = cum_score, colour = "Virat Kohli")) +
  geom_line(data = da_warner, aes(x = match_id, y = cum_score, colour = "DA Warner")) +
  geom_point(data = da_warner, aes(x = match_id, y = cum_score, colour = "DA Warner")) +
  labs(title = "Comparison: Virat Kohli vs DA Warner (Cumulative Runs)", x = "Match Number", y = "Cumulative Runs", colour = "Player") +
  theme_stata()

# Team Wins vs Losses
points_long <- points_table %>%
  select(team, wins, loss) %>%
  pivot_longer(c(wins, loss), names_to = "result", values_to = "n") %>%
  mutate(n = if_else(result == "loss", -n, n),
         result = recode(result, wins = "Wins", loss = "Losses"))

p_wins_losses <- ggplot(points_long, aes(x = reorder(team, -n), y = n, fill = result)) +
  geom_col(position = "identity") +
  labs(title = "Team Wins and Losses (IPL 2016)", x = "Teams", y = "Number of Wins/Losses (+Wins / -Losses)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print plots to the viewer
print(p_mvp)
print(p_ba_sr)
print(p_kohli_scores)
print(p_kohli_box)
print(p_kohli_pareto)
print(p_kohli_vs_warner)
print(p_wins_losses)

# (Optional) Save outputs to files (uncomment if desired) ----------------------
# dir.create("figures", showWarnings = FALSE)
# readr::write_csv(batting_stats,       "batting_stats_2016.csv")
# readr::write_csv(bowling_stats,       "bowling_stats_2016.csv")
# readr::write_csv(points_table,        "points_table_2016.csv")
# readr::write_csv(mvp_players,         "mvp_players_2016.csv")
# readr::write_csv(per_innings_score,   "per_innings_batting_2016.csv")
# readr::write_csv(per_innings_wickets, "per_innings_wickets_2016.csv")
# ggsave("figures/mvp_2016.png",                 p_mvp,            width = 9, height = 6, dpi = 300)
# ggsave("figures/ba_vs_sr_2016.png",            p_ba_sr,          width = 9, height = 6, dpi = 300)
# ggsave("figures/kohli_scores_2016.png",        p_kohli_scores,   width = 9, height = 6, dpi = 300)
# ggsave("figures/kohli_box_2016.png",           p_kohli_box,      width = 9, height = 6, dpi = 300)
# ggsave("figures/kohli_pareto_2016.png",        p_kohli_pareto,   width = 9, height = 6, dpi = 300)
# ggsave("figures/kohli_vs_warner_2016.png",     p_kohli_vs_warner,width = 9, height = 6, dpi = 300)
# ggsave("figures/team_wins_losses_2016.png",    p_wins_losses,    width = 10, height = 6, dpi = 300)
