# Load Libraries
library(tidyverse)
library(StatsBombR)

# 1. Load Competitions, Matches & Events
comps <- FreeCompetitions() %>%
  filter(competition_name == "UEFA Euro")

matches <- FreeMatches(comps)
events <- free_allevents(MatchesDF = matches, Parallel = TRUE) %>%
  allclean()

# 2. Clean Match Names
matches_clean <- matches %>%
  rename(
    home_team = home_team.home_team_name,
    away_team = away_team.away_team_name
  )

# 3. Calculate Possession per Team
team_possession <- events %>%
  group_by(match_id, possession, possession_team.name) %>%
  summarise(possession_time = max(TimeInPoss, na.rm = TRUE), .groups = "drop") %>%
  group_by(match_id, possession_team.name) %>%
  summarise(total_possession_time = sum(possession_time), .groups = "drop") %>%
  group_by(match_id) %>%
  mutate(possession_pct = total_possession_time / sum(total_possession_time) * 100)

# 4. Calculate Pass Accuracy
pass_stats <- events %>%
  filter(type.name == "Pass") %>%
  group_by(match_id, possession_team.name) %>%
  summarise(
    total_passes = n(),
    successful_passes = sum(is.na(pass.outcome.name)),
    pass_accuracy = successful_passes / total_passes * 100,
    .groups = "drop"
  )

# 5. Calculate Shot Stats (including xG)
shot_stats <- events %>%
  filter(type.name == "Shot") %>%
  group_by(match_id, possession_team.name) %>%
  summarise(
    shots = n(),
    total_xG = sum(shot.statsbomb_xg, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Combine Team-Level Features
team_features <- team_possession %>%
  left_join(pass_stats, by = c("match_id", "possession_team.name")) %>%
  left_join(shot_stats, by = c("match_id", "possession_team.name"))

# 7. Create Match-Level Dataset with Home/Away Features
matches_features <- matches_clean %>%
  left_join(team_features, by = c("match_id", "home_team" = "possession_team.name")) %>%
  rename_with(~paste0("home_", .), c(total_possession_time, possession_pct, total_passes, successful_passes, pass_accuracy, shots, total_xG)) %>%
  left_join(team_features, by = c("match_id", "away_team" = "possession_team.name")) %>%
  rename_with(~paste0("away_", .), c(total_possession_time, possession_pct, total_passes, successful_passes, pass_accuracy, shots, total_xG)) %>%
  mutate(
    result = case_when(
      home_score > away_score ~ "home_win",
      home_score < away_score ~ "away_win",
      TRUE ~ "draw"
    )
  ) %>%
  select(match_id, home_team, away_team,
         home_score, away_score,
         home_possession_pct, home_pass_accuracy, home_shots, home_total_xG,
         away_possession_pct, away_pass_accuracy, away_shots, away_total_xG,
         result)

# 8. View Final Dataset
view(matches_features)
print(matches_features)



############
#Explanatory data analysis


















