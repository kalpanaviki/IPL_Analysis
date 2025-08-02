#IPL Analysis
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("usethis")
install.packages("gh")

# Libraries loading
library(dplyr)
library(ggplot2)
library(readr)
library(patchwork)

# Load data
matches <- read_csv("~/Downloads/matches.csv")
deliveries <- read_csv("~/Downloads/deliveries.csv")

# Unifying team names correctly 
team_names_map <- c(
  "Delhi Daredevils" = "Delhi Capitals",
  "Kings XI Punjab" = "Punjab Kings",
  "Deccan Chargers" = "Sunrisers Hyderabad",
  "Rising Pune Supergiant" = "Rising Pune Supergiants",
  "Pune Warriors" = "Rising Pune Supergiants",
  "Gujarat Lions" = "Gujarat Titans",
  "Royal Challengers Bangalore" = "Royal Challengers Bengaluru"
)

matches <- matches %>%
  mutate(
    team1 = recode(team1, !!!team_names_map),
    team2 = recode(team2, !!!team_names_map),
    toss_winner = recode(toss_winner, !!!team_names_map),
    winner = recode(winner, !!!team_names_map)
  )


deliveries <- deliveries %>%
  mutate(
    batting_team = recode(batting_team, !!!team_names_map),
    bowling_team = recode(bowling_team, !!!team_names_map)
  )

team_colors <- c(
  "Mumbai Indians" = "#045093",
  "Chennai Super Kings" = "#f4d03f",
  "Royal Challengers Bengaluru" = "#da291c",
  "Kolkata Knight Riders" = "#3e236e",
  "Rajasthan Royals" = "#ea1a8c",
  "Delhi Capitals" = "#17449B",
  "Punjab Kings" = "#e2241a",
  "Sunrisers Hyderabad" = "#f26522",
  "Gujarat Titans" = "#0a2c3d",
  "Lucknow Super Giants" = "#00a8e1",
  "Rising Pune Supergiants" = "#ff7f0e",
  "Kochi Tuskers Kerala" = "#ff6f61"
)

abbr_team_name <- c(
  "Mumbai Indians" = "MI",
  "Chennai Super Kings" = "CSK",
  "Royal Challengers Bengaluru" = "RCB",
  "Kolkata Knight Riders" = "KKR",
  "Rajasthan Royals" = "RR",
  "Delhi Capitals" = "DC",
  "Punjab Kings" = "PBKS",
  "Sunrisers Hyderabad" = "SRH",
  "Gujarat Titans" = "GT",
  "Lucknow Super Giants" = "LSG",
  "Rising Pune Supergiants" = "RPS",
  "Kochi Tuskers Kerala" = "KTK"
)

#1 Team with most IPL titles
title_wins <- filter(matches, match_type == "Final") %>% 
  filter(!is.na(winner)) %>%
  group_by(winner) %>%
  summarise(titles_won = n()) %>%
  arrange(desc(titles_won))

ggplot(title_wins, aes(x = reorder(winner, titles_won), y = titles_won)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Most IPL Titles by Team (2008-2024)",
       x = "Team",
       y = "Number of Titles",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 1") +
  theme_minimal()

#2 Most matches played in a season
most_matches_season <- matches %>%
  group_by(season) %>%
  summarise(match_count = n()) %>%
  arrange(desc(match_count))

peak_season <- most_matches_season %>%
  slice_max(match_count)

ggplot(most_matches_season, aes(x = season, y = match_count)) +
  geom_col(fill = "lightblue")+
  geom_point(data = peak_season, aes(x = season, y = match_count), 
             color = "red", size = 4) +
  geom_text(data = peak_season, aes(x = season, y = match_count,
                                  label = paste("Most Matches:", match_count), vjust = -1),
            color = "red", fontface = "bold", size = 4.2)+
  labs(title = "Number of Matches per IPL Season",
       x = "Season",
       y = "Matches Played",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 2") +
  theme_minimal()

#3 Top 10 venues hosted the most matches
venue_counts <- matches %>%
  group_by(venue) %>%
  summarise(match_count = n()) %>%
  arrange(desc(match_count))

top_ten_venues <- venue_counts %>% slice_max(match_count, n = 10)

ggplot(top_ten_venues, aes(x = reorder(venue, match_count), y = match_count, fill = as.factor(match_count))) +
  geom_col() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  coord_flip() +
  geom_text(aes(label = match_count), hjust = -0.2, size = 3.5) +
  labs(title = "Top 10 IPL Venues by Number of Matches Hosted (2008-2024)",
       x = "Venue",
       y = "Matches Played",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 3") +
  theme_minimal() +
  theme(legend.position = "none")

#4 Comparing toss winners with match winners
toss_won_vs_match_won <- matches %>%
  mutate(toss_and_match_win = ifelse(toss_winner == winner, "Yes", "No")) %>% 
  group_by(toss_and_match_win) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(100 * count / sum(count), 1))

ggplot(toss_won_vs_match_won, aes(x = toss_and_match_win, y = percentage, fill = toss_and_match_win)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Yes" = "darkblue", "No" = "darkred")) +
  labs(title = "Toss Winner vs Match Winner (IPL 2008–2024)",
       x = "Matches Won",
       y = "Percentage of Matches",
       fill = "Result",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 4") +
  theme_minimal()

#5 Top winning teams overall
top_teams_overall <- matches %>% 
  filter(!is.na(winner)) %>% 
  group_by(winner) %>% 
  summarise(no_of_wins = n()) %>% 
  arrange(desc(no_of_wins))

ggplot(top_teams_overall, aes(x = reorder(winner, no_of_wins), y = no_of_wins, fill = winner)) +
  geom_col() +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  geom_text(aes(label = no_of_wins), hjust = -0.2, size = 3.5) +
  labs(title = "Top Winning Teams in IPL (2008–2024)",
       x = "Teams",
       y = "Number of wins",
       fill = "",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 5") +
  theme_minimal() +
  theme( legend.position = "none")


#6 Team with the best win percentage
matches_played <- matches %>% 
  select(team1, team2) %>% 
  pivot_longer(cols = c(team1, team2) , values_to = "team") %>% 
  group_by(team) %>% 
  summarize(played = n(), .groups = "drop")

matches_won <- matches %>% 
  filter(!is.na(winner)) %>% 
  group_by(winner) %>% 
  summarise(no_of_wins = n(), .groups = "drop")
  
best_win_percentage <- matches_played %>%
  left_join(matches_won, by = c("team" = "winner")) %>%
  mutate(no_of_wins = replace_na(no_of_wins, 0),
         win_percent = round((no_of_wins / played) * 100, 2)) %>%
  arrange(desc(win_percent))

ggplot(best_win_percentage, aes(x = reorder(team, win_percent), y = win_percent, fill = team)) +
  geom_col() +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  labs(title = "IPL Teams by Win Percentage (2008–2024)",
       x = "Team",
       y = "Win Percentage",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 6") +
  theme_minimal() +
  theme( legend.position = "none")

#7 Top rivalries
head_to_head <- matches %>%
  filter(!is.na(winner)) %>%
  group_by(team1, team2, winner) %>%
  summarise(matches = n(), .groups = "drop")

total_matches <- head_to_head %>%
  group_by(team1, team2) %>%
  summarise(total_played = sum(matches), .groups = "drop")

rivalry <- head_to_head %>%
  left_join(total_matches, by = c("team1", "team2")) %>%
  arrange(desc(total_played))

top_rivalries <- rivalry %>%
  filter(total_played > 10) %>% 
  mutate(    team1 = recode(team1, !!!abbr_team_name),
             team2 = recode(team2, !!!abbr_team_name)) %>% 
  unite("rivalry", team1:team2, sep = " vs ") %>%
  arrange(desc(total_played)) %>%
  slice_max(order_by = total_played, n = 5)

ggplot(top_rivalries, aes(x = rivalry, y = matches, fill = winner)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = matches), 
                position = position_stack(vjust = 0.5), 
                size = 4,
                colour = "white") +
  scale_fill_manual(values = team_colors ) +
  labs(title = "Top 5 Rivalries in IPL (2008-2024)",
       x = "Team Match-Up",
       y = "Number of Wins",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 7") +
  theme_minimal()


#8 Top run-scorers in IPL history
top_run_scorers <- deliveries %>% 
  group_by(batter) %>% 
  summarize(total_runs_scored = sum(batsman_runs, na.rm = TRUE)) %>% 
  arrange(desc(total_runs_scored)) %>% 
  slice_max(order_by = total_runs_scored, n = 10)

ggplot(top_run_scorers, aes(x = reorder(batter, total_runs_scored), y = total_runs_scored, fill = as.factor(total_runs_scored) )) +
  geom_col() +
  coord_flip()+
  scale_fill_viridis_d(option = "inferno", direction = -1) +
  geom_text(aes(label = total_runs_scored),
            hjust = -0.3,
            size = 3.5) +
  labs(title = "Top 10 Run Scorers in IPL History (2008–2024)",
       x = "Batsmen",
       y = "Total Runs",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 8") +
  theme_minimal() +
  theme(legend.position = "none")


#9 Top wicket-takers in IPL history
top_bowlers_wicket <- deliveries %>% 
  group_by(bowler) %>% 
  summarize(total_wickets = sum(is_wicket, na.rm = TRUE)) %>% 
  arrange(desc(total_wickets)) %>% 
  slice_max(order_by = total_wickets, n = 10)

ggplot(top_bowlers_wicket, aes(x = reorder(bowler, total_wickets), y = total_wickets, fill = as.factor(total_wickets))) +
  geom_col() +
  coord_flip()+
  scale_fill_viridis_d(direction = -1) +
  geom_text(aes(label = total_wickets),
            hjust = -0.3,
            size = 3.5) +
  labs(title = "Top 10 Wicket Takers in IPL History (2008–2024)",
       x = "Bowler",
       y = "Total Wickets",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 9") +
  theme_minimal() +
  theme(legend.position = "none")


#10 Most sixes and fours in IPL history
top_six_hitters <- deliveries %>% 
  filter(batsman_runs == 6) %>% 
  group_by(batter) %>% 
  summarize(sixes = n()) %>% 
  arrange(desc(sixes)) %>% 
  slice_head(n = 10)

top_four_hitters <- deliveries %>% 
  filter(batsman_runs == 4) %>% 
  group_by(batter) %>% 
  summarize(fours = n()) %>% 
  arrange(desc(fours)) %>% 
  slice_head(n = 10)

plotA <- ggplot(top_six_hitters, aes(x = reorder(batter, sixes), y = sixes, fill = as.factor(sixes))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = sixes), 
            hjust = 1.5,
            size = 3.5,
            colour = "white") +
  scale_fill_viridis_d(option = "rocket", direction = -1) +
  labs(title = "Top IPL Players by Number of Sixes",
       x = "Batsmen",
       y = "Sixes",
       tag = "Plot 10") +
  theme_minimal() +
  theme(legend.position = "none")


plotB <- ggplot(top_four_hitters, aes(x = reorder(batter, fours), y = fours, fill = as.factor(fours))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = fours), 
            hjust = 1.5,
            size = 3.5,
            colour = "white") +
  scale_fill_viridis_d(option = "cividis", direction = -1) +
  labs(title = "Top IPL Players by Number of Fours",
       x = NULL,
       y = "Fours") +
  theme_minimal() +
  theme(legend.position = "none")
  
plot9 <- plotA + plotB +
  plot_annotation(
    title = "Most Sixes and Fours",
    caption = "Source: Kaggle IPL Dataset",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
   
plot9 


#11 Powerplay vs Death Overs Scoring 
phase <- deliveries %>%
  mutate(over_phase = case_when(
    over <= 6 ~ "Powerplay",
    over >= 16 ~ "Death Overs",
    TRUE ~ "Middle Overs"
  )) %>% 
  filter(over_phase %in% c("Powerplay", "Death Overs")) %>% 
  group_by(over_phase) %>% 
  summarize(total_runs = sum(batsman_runs))

ggplot(phase, aes(x = over_phase, y = total_runs, fill = over_phase)) +
  geom_col(width = 0.6) +
  scale_fill_viridis_d(option = "turbo") +
  geom_text(aes(label = total_runs), vjust = -0.5) +
  labs(title = "Scoring Comparison in Powerplay vs Death Overs",
       x = NULL,
       y = "Total Runs",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 11") +
  theme_minimal() +
  theme(legend.position = "none")


#12 Impact of Dot Balls
dot_impact_bowlers <- deliveries %>% 
  mutate(dot_ball = if_else(total_runs == 0, 1, 0)) %>% 
  group_by(bowler) %>% 
  summarize(
    total_balls = n(),
    dot_balls = sum(dot_ball),
    dot_ball_pct = round((dot_balls / total_balls) * 100, 2)
  ) %>%
  filter(total_balls >= 300) %>%
  arrange(desc(dot_ball_pct)) %>% 
  slice_head(n=10)

ggplot(dot_impact_bowlers,
       aes(x = reorder(bowler, dot_ball_pct), y = dot_ball_pct, fill = dot_ball_pct)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(dot_ball_pct, "%")), hjust = -0.2, size = 3.5) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(title = "Top 10 Bowlers by Dot Ball Percentage (2008-2024)",
       x = "Bowler",
       y = "Dot Ball %",
       caption = "Source: Kaggle IPL Dataset",
       tag = "Plot 12") +
  theme_minimal() +
  theme(legend.position = "none")


#13 Clutch players
clutch_batters <- deliveries %>%
  filter(inning == 2) %>%
  group_by(batter) %>%
  summarise(
    runs = sum(batsman_runs),
    balls = n(),
    sr = round((runs / balls) * 100, 2)
  ) %>%
  filter(balls >= 200) %>%
  arrange(desc(sr)) %>% 
  slice_head(n=10)

clutch_bowlers <- deliveries %>%
  filter(over >= 16) %>%
  group_by(bowler) %>%
  summarise(
    runs_conceded = sum(total_runs),
    balls_bowled = n(),
    economy = round((runs_conceded / balls_bowled) * 6, 2)
  ) %>%
  filter(balls_bowled >= 100) %>%
  arrange(economy) %>% 
  slice_head(n=10)

plotC <- ggplot(clutch_batters, aes(x = reorder(batter, sr), y = sr, fill = as.factor(sr))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = sr), 
            hjust = 1.5,
            size = 3.5,
            colour = "white") +
  scale_fill_viridis_d(option = "rocket", direction = -1) +
  labs(title = "Best Batting Strike Rate in IPL",
       x = "Batsmen",
       y = "Strike Rate",
       tag = "Plot 13") +
  theme_minimal() +
  theme(legend.position = "none")


plotD <- ggplot(clutch_bowlers, aes(x = reorder(bowler, -economy), y = economy, fill = as.factor(economy))) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = economy), 
            hjust = 1.5,
            size = 3.5,
            colour = "white") +
  scale_fill_viridis_d() +
  labs(title = "Best Bowling Economic Rate in IPL",
       x = NULL,
       y = "Economy") +
  theme_minimal() +
  theme(legend.position = "none")

plot13 <- plotC + plotD +
  plot_annotation(
    title = "Clutch Players",
    caption = "Source: Kaggle IPL Dataset",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

plot13 


