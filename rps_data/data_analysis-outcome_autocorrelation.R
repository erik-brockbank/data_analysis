#' This script analyzes n-back auto-correlation of outcomes (win, tie, loss)
#' Higher auto-correlation of outcomes provides stronger evidence that people are exploiting/exploitable
#' 
#' This script should *not* be used for general data overview, cleaning etc. 
#' (that happens in `data_processing.R`) and should not be used for analyses that extend 
#' beyond transition distribution dependencies.

setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")
source('00_data_processing.R') # script used for data processing/cleanup


# GLOBALS 
MAX_LAG = 10


# ANALYSIS FUNCTIONS ===========================================================

# Function for selecting a single player's data from each game
# (avoids duplcate auto-correlation calculations for each game since outcomes 
# for each player in a given game are complementary)
get_unique_game_data = function(data) {
  data %>%
    group_by(game_id, round_index) %>%
    filter(row_number() == 1)
}


# Get auto-correlation of game outcomes at increasing round lags
get_game_acf = function(unique_game_data, max_lag) {
  # data frame for keeping track of auto-correlation by game
  acf_agg = data.frame(game = character(),
                       lag = numeric(),
                       acf = numeric())
  # code outcomes as numeric
  unique_game_data = unique_game_data %>%
    mutate(points_symmetric = case_when(player_outcome == "win" ~ 1,
                                        player_outcome == "loss" ~ -1,
                                        player_outcome == "tie" ~ 0))
  # fill in data frame
  for (game in unique(unique_game_data$game_id)) {
    game_data = unique_game_data %>%
      # TODO what's going on with these na outcomes???
      filter(game_id == game & !is.na(player_outcome))
    game_acf = acf(game_data$points_symmetric, lag.max = max_lag, plot = F)
    
    acf_agg = rbind(acf_agg, data.frame(game = game, lag = seq(0, max_lag), acf = game_acf[[1]]))
  }
  return(acf_agg)
}


# GRAPHING FUNCTIONS ===========================================================

plot_acf = function(acf_data) {
  summary_acf = acf_data %>%
    group_by(lag) %>%
    summarize(mean_acf = mean(acf))
  
  acf_data %>%
    ggplot(aes(x = lag, y = acf)) +
    geom_jitter(aes(color = "ind"), alpha = 0.5, width = 0.1, size = 2) +
    geom_point(data = summary_acf, aes(x = lag, y = mean_acf, color = "mean"), size = 4) +
    scale_x_continuous(breaks = seq(0, max(acf_data$lag))) +
    scale_color_viridis(discrete = T,
                        labels = c("ind" = "Individual dyads", "mean" = "Average across dyads"),
                        # Set color range to avoid purple and yellow contrasts...
                        name = element_blank(),
                        begin = 0.8,
                        end = 0.2) +
    labs(x = "Lag (game rounds)", y = "Outcome auto-correlation") +
    ggtitle("Auto-correlation of round outcomes") +
    individ_plot_theme
}


# ANALYSIS =====================================================================

# 1. Overall outcome analysis
# Do people have disproportionate numbers of wins/losses overall?
# Chi-squared test of win/loss/tie counts for each dyad
chisq_tests = data.frame(game_id = character(), chisq = numeric(), p_val = numeric())
outcome.summary = data %>%
  group_by(game_id, round_index) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  group_by(game_id) %>%
  count(player_outcome) %>%
  filter(!is.na(player_outcome))

for (game in unique(outcome.summary$game_id)) {
  game_data = outcome.summary %>%
    filter(game_id == game)
  tst = chisq.test(game_data$n)
  chisq_tests = rbind(chisq_tests, data.frame(game_id = game, chisq = tst$statistic, p_val = tst$p.value))
}
chisq_tests



# 2. Auto-correlation (temporal outcome analysis)

# Remove 100-round dyads and select only a single player within each game to avoid redundancy

unique(data$game_id)
unique_game_data = get_unique_game_data(data)


# Get ACF data
acf_agg = get_game_acf(unique_game_data, MAX_LAG)

# Plot ACF data
plot_acf(acf_agg)


# What's going on with the high auto-correlation dyad?
outlier = acf_agg %>%
  group_by(lag) %>%
  filter(acf == max(acf)) %>%
  select(game, acf) %>%
  filter(lag > 0) %>%
  ungroup() %>%
  distinct(game)

as.character(outlier$game)

# score differential
score_diff = data %>% 
  filter(game_id == as.character(outlier$game)  & round_index == max(round_index)) %>%
  mutate(final_score = player_total + player_points,
         point_diff = abs(final_score - lag(final_score, 1))) %>%
  filter(!is.na(point_diff)) %>%
  select(point_diff)

# win count differential
win_count_diff = data %>%
  filter(game_id == as.character(outlier$game)) %>%
  group_by(player_id) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  ungroup() %>%
  mutate(opp_win_count = lag(n, 1)) %>%
  filter(!is.na(opp_win_count)) %>%
  summarize(win_diff = abs(n - opp_win_count))

# Seems they had a huge score differential; not the highest win count differential (fewer ties?)




