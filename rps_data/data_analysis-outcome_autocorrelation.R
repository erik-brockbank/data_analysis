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
ROUNDS = 300


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
                                        player_outcome == "tie" ~ 0)) %>%
    filter(!is.na(points_symmetric)) # TODO: why do we have NA outcomes?
  # fill in data frame
  for (game in unique(unique_game_data$game_id)) {
    game_data = unique_game_data %>%
      filter(game_id == game)
    game_acf = acf(game_data$points_symmetric, lag.max = max_lag, plot = F)
    
    acf_agg = rbind(acf_agg, data.frame(game = game, lag = seq(0, max_lag), acf = game_acf[[1]]))
  }
  return(acf_agg)
}

# Function to generate a sample set of `rounds` outcomes
get_sample_game = function(rounds) {
  sample(c("loss", "tie", "win"), rounds, replace = T)
}

# Function to add a winning and losing streak of length `streak_length` to the `sample_game`
add_game_streaks = function(sample_game, streak_length) {
  # randomly set first streak_length games to be wins and middle streak_length games to be losses
  sample_game[1:streak_length] = "win"
  # sample_game[101:(100 + streak_length)] = "loss"
  sample_game[101:(100 + streak_length)] = "win"
  return(sample_game)
}

# Function to generate acf of simulated games for `n_particpants` playing `rounds`
get_sample_acf = function(streak_length, rounds, n_participants) {
  sample_df = data.frame(game_id = numeric(), round_index = numeric(), player_outcome = character(),
                         stringsAsFactors = F)
  
  for (game in seq(1, n_participants)) {
    sample_game = get_sample_game(rounds)
    sample_game = add_game_streaks(sample_game, streak_length)
    
    sample_df = rbind(sample_df, 
                      data.frame(game_id = game, round_index = seq(1, rounds), player_outcome = sample_game,
                                 stringsAsFactors = F))
  }
  
  return(sample_df)
}


# GRAPHING FUNCTIONS ===========================================================

plot_acf = function(acf_data) {
  summary_acf = acf_data %>%
    group_by(lag) %>%
    summarize(mean_acf = mean(acf))
  
  ci_thresh = 2 / sqrt(ROUNDS) # num. SDs from 0 over sqrt(N) obs to get 95% CI on mean of 0 auto-corr
  
  acf_data %>%
    ggplot(aes(x = lag, y = acf)) +
    geom_jitter(aes(color = "ind"), alpha = 0.5, width = 0.1, size = 2) +
    geom_point(data = summary_acf, aes(x = lag, y = mean_acf, color = "mean"), size = 4) +
    scale_x_continuous(breaks = seq(0, max(acf_data$lag))) +
    # significance thresholds
    geom_hline(yintercept = ci_thresh, linetype = "dashed", size = 1, color = "black") +
    geom_hline(yintercept = -ci_thresh, linetype = "dashed", size = 1, color = "black") +
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

# 1. Auto-correlation (temporal outcome analysis)

# Select only a single player within each dyad (their opponent should have identical acf)
length(unique(data$game_id))
unique_game_data = get_unique_game_data(data)


# Get ACF data
acf_agg = get_game_acf(unique_game_data, MAX_LAG)

# Plot ACF data
plot_acf(acf_agg)


# 2. Auto-correlation for top-N win count differential dyads
win_count_diff_empirical = get_emp_win_differential(data)

win_count_diff_empirical_top = win_count_diff_empirical %>%
  top_n(win_diff, n = 10)
unique(win_count_diff_empirical_top$game_id)

unique_game_data_top = unique_game_data %>%
  filter(game_id %in% win_count_diff_empirical_top$game_id)

# Get ACF data
acf_top = get_game_acf(unique_game_data_top, MAX_LAG)

# Plot ACF data
plot_acf(acf_top)


# 3. What kind of streaks are needed to produce significant auto-correlations?
streak_length = 30
streak_pct = (2 * streak_length) / ROUNDS

sample_acf_data = get_sample_acf(streak_length, ROUNDS, 58)

# Get ACF data
acf_sample = get_game_acf(sample_acf_data, MAX_LAG)

# Plot ACF data
plot_acf(acf_sample)

# Take-aways: 
# streak_pct needs to be > 10% to detect significant auto-correlations
# by 20% it's very visible



# APPENDIX =====================================================================

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




