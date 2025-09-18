#Loading necessary libraries

library(tidyverse)
library(StatsBombR)


# Load and transform the data set for analysis

comps <- FreeCompetitions()

comps = comps %>%
  filter(competition_name == "UEFA Euro") #we will take euro 2020 and 2024 data

matches <- FreeMatches(comps)
events <- free_allevents(MatchesDF = matches , Parallel = T)
events = allclean(events)

matches_clean_names <- matches %>%       #remane the variable
  rename(
    home_team = home_team.home_team_name,
    away_team = away_team.away_team_name
  )                                       

# Aggregate possession per team per match
team_possession <- events %>%
  group_by(match_id, possession, possession_team.name) %>%
  summarise(possession_time = max(TimeInPoss, na.rm = TRUE), .groups = "drop") %>%
  group_by(match_id, possession_team.name) %>%
  summarise(total_possession_time = sum(possession_time), .groups = "drop") %>%
  group_by(match_id) %>%
  mutate(possession_pct = total_possession_time / sum(total_possession_time) * 100)

# Join with match data
matches_joined <- matches_clean_names %>%
  left_join(team_possession,
            by = c("match_id" = "match_id", "home_team" = "possession_team.name")) %>%
  rename(home_possession_time = total_possession_time,
         home_possession_pct = possession_pct) %>%
  left_join(team_possession,
            by = c("match_id" = "match_id", "away_team" = "possession_team.name")) %>%
  rename(away_possession_time = total_possession_time,
         away_possession_pct = possession_pct)



# Select only required columns
matches_final <- matches_joined %>%
  select(match_id,
         home_team, away_team,
         home_score, away_score,
         home_possession_time, home_possession_pct,
         away_possession_time, away_possession_pct)

print(matches_final)
view(matches_final)

matches_final <- matches_joined %>%
  select(match_id,
         home_team, away_team,
         home_score, away_score,
         home_possession_time, home_possession_pct,
         away_possession_time, away_possession_pct) %>%
  mutate(
    result = case_when(
      home_score > away_score ~ "home_win",
      home_score == away_score ~ "draw",
      home_score < away_score ~ "away_win"
    )
  )

print(matches_final)



















