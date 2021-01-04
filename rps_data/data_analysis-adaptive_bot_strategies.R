#'
#' RPS *adaptive* bot analysis
#' Examines human performance against bot opponents with varying
#' adaptive strategies
#'



# SETUP ========================================================================

setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")
rm(list = ls())
library(viridis)
library(patchwork)
library(tidyverse)



# GLOBALS ======================================================================

DATA_FILE = "rps_v3_data.csv" # name of file containing full dataset for all rounds
FREE_RESP_FILE = "rps_v3_data_freeResp.csv" # file containing free response data by participant
SLIDER_FILE = "rps_v3_data_sliderData.csv" # file containing slider Likert data by participant
NUM_ROUNDS = 300 # number of rounds in each complete game

STRATEGY_LEVELS = c(
  "opponent_moves",
  "opponent_prev_moves",
  "bot_prev_move",
  "opponent_bot_prev_move",
  "opponent_prev_two_moves",
  "bot_prev_two_moves",
  "opponent_transitions",
  "opponent_courn_transitions",
  "opponent_outcome_transitions",
  "opponent_outcome_prev_transition_dual"
)

STRATEGY_LOOKUP = list(
  "opponent_moves" = "Move distribution",
  "opponent_prev_moves" = "Player previous move",
  "bot_prev_move" = "Bot previous move",
  "opponent_bot_prev_move" = "Player + bot previous move",
  "opponent_prev_two_moves" = "Player previous two moves",
  "bot_prev_two_moves" = "Bot previous two moves",
  "opponent_transitions" = "Player transitions (+/-/0)",
  "opponent_courn_transitions" = "Player Cournot transitions (+/-/0)",
  "opponent_outcome_transitions" = "Player outcome transitions",
  "opponent_outcome_prev_transition_dual" = "Player outcome + previous transition"
)



# ANALYSIS FUNCTIONS ===========================================================

# Generic file reading function: more specific ones below
read_data = function(filename) {
  data = read_csv(filename)
  return(data)
}


read_game_data = function(filename) {
  data = read_csv(filename)
  data$bot_strategy = factor(data$bot_strategy, levels = STRATEGY_LEVELS)

  # Remove all incomplete games
  incomplete_games = data %>%
    group_by(game_id, player_id) %>%
    summarize(rounds = max(round_index)) %>%
    filter(rounds < NUM_ROUNDS) %>%
    select(game_id) %>%
    unique()

  data = data %>%
    filter(!(game_id %in% incomplete_games$game_id))

  # Remove any duplicate complete games that have the same SONA survey code
  # NB: this can happen if somebody played all the way through but exited before receiving credit
  # First, fetch sona survey codes with multiple complete games
  repeat_codes = data %>%
    group_by(sona_survey_code) %>%
    filter(is_bot == 0) %>%
    summarize(trials = n()) %>%
    filter(trials > 300) %>%
    select(sona_survey_code)

  # Next, get game id for the earlier complete game
  # NB: commented out code checks that we have slider/free resp data for at least one of the games
  duplicate_games = data %>%
    filter(sona_survey_code %in% repeat_codes$sona_survey_code &
             is_bot == 0  &
             round_index == NUM_ROUNDS) %>%
    select(sona_survey_code, game_id, player_id, round_begin_ts) %>%
    # remove the earlier one since the later one has free response and slider data (confirm with joins below)
    group_by(sona_survey_code) %>%
    filter(round_begin_ts == min(round_begin_ts)) %>%
    # inner_join(fr_data, by = c("game_id", "player_id")) %>%
    # inner_join(slider_data, by = c("game_id", "player_id")) %>%
    distinct(game_id)

  data = data %>%
    filter(!game_id %in% duplicate_games$game_id)

  return(data)
}

# Read in and process free response data
read_free_resp_data = function(filename, game_data) {
  data = read_csv(filename)
  # Join with game data to get bot strategy, etc.
  data = data %>%
    inner_join(game_data, by = c("game_id", "player_id")) %>%
    distinct(bot_strategy, game_id, player_id, free_resp_answer)
  # Order bot strategies
  data$bot_strategy = factor(data$bot_strategy, levels = STRATEGY_LEVELS)
  # Add plain english strategy, string process free resposne answers
  data = data %>%
    group_by(bot_strategy, player_id) %>%
    mutate(strategy = STRATEGY_LOOKUP[[bot_strategy]],
           free_resp_answer = str_replace_all(free_resp_answer, "\n" , "[newline]")) %>%
    ungroup()

  return(data)
}

# Read in and process slider data
read_slider_data = function(filename, game_data) {
  data = read_csv(filename)
  data = data %>%
    inner_join(game_data, by = c("game_id", "player_id")) %>%
    distinct(game_id, player_id, bot_strategy, index, statement, resp)
  # Order bot strategies
  data$bot_strategy = factor(data$bot_strategy, levels = STRATEGY_LEVELS)
  # Add plain english strategy
  data = data %>%
    group_by(bot_strategy, player_id, index) %>%
    mutate(strategy = STRATEGY_LOOKUP[[bot_strategy]]) %>%
    ungroup()

  return(data)
}

get_slider_summary = function(slider_data) {
  slider_data %>%
    group_by(statement, bot_strategy, strategy) %>%
    summarize(n = n(),
              mean_resp = mean(resp),
              se = sd(resp) / sqrt(n),
              se_upper = mean_resp + se,
              se_lower = mean_resp - se)
}


get_bot_strategy_win_count_differential = function(data) {
  win_diff = data %>%
    group_by(bot_strategy, game_id, player_id, is_bot) %>%
    count(win_count = player_outcome == "win") %>%
    filter(win_count == TRUE) %>%
    group_by(bot_strategy, game_id) %>%
    # Win count for bots minus win count for human opponents
    # NB: if the person or bot *never* wins, this count will fail for them
    summarize(win_count_diff = n[is_bot == 1] - n[is_bot == 0]) %>%
    as.data.frame()
  return(win_diff)
}

get_bot_strategy_win_count_differential_summary = function(strategy_data) {
  strategy_data %>%
    group_by(bot_strategy) %>%
    summarize(mean_win_count_diff = mean(win_count_diff),
              n = n(),
              se = sd(win_count_diff) / sqrt(n),
              lower_se = mean_win_count_diff - se,
              upper_se = mean_win_count_diff + se)
}

# Divide each subject's trials into blocks of size blocksize (e.g. 10 trials)
# then get each subject's win percent in each block
get_subject_block_data = function(data, blocksize) {
  data %>%
    filter(is_bot == 0) %>%
    group_by(bot_strategy, round_index) %>%
    mutate(round_block = ceiling(round_index / blocksize)) %>%
    select(bot_strategy, round_index, game_id, player_id, player_outcome, round_block) %>%
    group_by(bot_strategy, game_id, player_id, round_block) %>%
    count(win = player_outcome == "win") %>%
    mutate(total = sum(n),
           win_pct = n / total) %>%
    filter(win == TRUE)
}

# Take in subject block win percent (calculated above) and summarize by bot strategy across subjects
get_block_data_summary = function(subject_block_data) {
  subject_block_data %>%
    group_by(bot_strategy, round_block) %>%
    summarize(subjects = n(),
              mean_win_pct = mean(win_pct),
              se_win_pct = sd(win_pct) / sqrt(subjects),
              lower_ci = mean_win_pct - se_win_pct,
              upper_ci = mean_win_pct + se_win_pct)
}


# GRAPH STYLE ==================================================================

default_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 20),
  axis.title.y = element_text(face = "bold", size = 16),
  axis.title.x = element_text(face = "bold", size = 16),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 14, face = "bold"),
  axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, face = "bold"),
  # legend text
  legend.text = element_text(size = 16, face = "bold"),
  # facet text
  strip.text = element_text(size = 12),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),

  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom",
  legend.key = element_rect(colour = "transparent", fill = "transparent")
)


# GRAPH FUNCTIONS ==============================================================

plot_bot_strategy_win_count_differential_summary = function(wcd_summary) {
  label_width = 10
  strategy_labels = c("opponent_moves" = str_wrap(STRATEGY_LOOKUP[["opponent_moves"]], label_width),
                      "opponent_prev_move" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move"]], label_width),
                      "bot_prev_move" = str_wrap(STRATEGY_LOOKUP[["bot_prev_move"]], label_width),
                      "opponent_bot_prev_move" = str_wrap(STRATEGY_LOOKUP[["opponent_bot_prev_move"]], label_width),
                      "opponent_prev_two_moves" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_two_moves"]], label_width),
                      "bot_prev_two_moves" = str_wrap(STRATEGY_LOOKUP[["bot_prev_two_moves"]], label_width),
                      "opponent_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_transitions"]], label_width),
                      "opponent_courn_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_courn_transitions"]], label_width),
                      "opponent_outcome_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_outcome_transitions"]], label_width),
                      "opponent_outcome_prev_transition_dual" = str_wrap(STRATEGY_LOOKUP[["opponent_outcome_prev_transition_dual"]], label_width))

  wcd_summary %>%
    ggplot(aes(x = bot_strategy, y = mean_win_count_diff)) +
    geom_point(aes(color = bot_strategy),
               size = 6) +
    geom_errorbar(aes(color = bot_strategy, ymin = lower_se, ymax = upper_se),
                  width = 0.25, size = 1) +
    geom_hline(yintercept = 0, size = 1, linetype = "dashed", color = "red") +
    labs(x = "", y = "Mean win count differential") +
    ggtitle("Win count differential across bot strategies") +
    scale_x_discrete(name = element_blank(),
                     labels = strategy_labels) +
    scale_color_viridis(discrete = TRUE,
                        name = element_blank()) +
    default_plot_theme +
    theme(
      # plot.title = element_text(size = 32, face = "bold"),
      axis.title.y = element_text(size = 24, face = "bold"),
      # axis.text.x = element_text(size = 20, face = "bold", angle = 0, vjust = 1),
      # axis.text.x = element_text(size = 20, face = "bold", angle = 0, vjust = 1),
      axis.text.x = element_blank(),
      # axis.text.y = element_text(face = "bold", size = 20),
      legend.position = "none"
    )
}

# Plot average of each participant's win percent in blocks of trials by strategy
plot_bot_strategy_win_pct_by_block = function(block_data_summary) {
  label_width = 12
  strategy_labels = c("opponent_moves" = str_wrap(STRATEGY_LOOKUP[["opponent_moves"]], label_width),
                     "opponent_prev_move" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move"]], label_width),
                     "bot_prev_move" = str_wrap(STRATEGY_LOOKUP[["bot_prev_move"]], label_width),
                     "opponent_bot_prev_move" = str_wrap(STRATEGY_LOOKUP[["opponent_bot_prev_move"]], label_width),
                     "opponent_prev_two_moves" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_two_moves"]], label_width),
                     "bot_prev_two_moves" = str_wrap(STRATEGY_LOOKUP[["bot_prev_two_moves"]], label_width),
                     "opponent_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_transitions"]], label_width),
                     "opponent_courn_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_courn_transitions"]], label_width),
                     "opponent_outcome_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_outcome_transitions"]], label_width),
                     "opponent_outcome_prev_transition_dual" = str_wrap(STRATEGY_LOOKUP[["opponent_outcome_prev_transition_dual"]], label_width))

  block_labels = c("1" = "30", "2" = "60", "3" = "90", "4" = "120", "5" = "150",
                   "6" = "180", "7" = "210", "8" = "240", "9" = "270", "10" = "300")

  block_data_summary %>%
    ggplot(aes(x = round_block, y = mean_win_pct, color = bot_strategy)) +
    geom_point(size = 6, alpha = 0.75) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), size = 1, width = 0.25, alpha = 0.75) +
    geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
    labs(x = "Game round", y = "Mean win percentage") +
    ggtitle("Participant win percentage against bot strategies") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        labels = strategy_labels) +
    scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
    default_plot_theme +
    theme(#axis.text.x = element_blank(),
      axis.title.y = element_text(size = 24, face = "bold"),
      legend.text = element_text(face = "bold", size = 14),
      legend.position = "right",
      legend.spacing.y = unit(1.0, 'lines'),
      #legend.key = element_rect(size = 2),
      legend.key.size = unit(4.75, 'lines'))
}


plot_slider_data = function(slider_data) {
  label_width = 10
  strategy_labels = c("opponent_moves" = str_wrap(STRATEGY_LOOKUP[["opponent_moves"]], label_width),
                      "opponent_prev_move" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move"]], label_width),
                      "bot_prev_move" = str_wrap(STRATEGY_LOOKUP[["bot_prev_move"]], label_width),
                      "opponent_bot_prev_move" = str_wrap(STRATEGY_LOOKUP[["opponent_bot_prev_move"]], label_width),
                      "opponent_prev_two_moves" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_two_moves"]], label_width),
                      "bot_prev_two_moves" = str_wrap(STRATEGY_LOOKUP[["bot_prev_two_moves"]], label_width),
                      "opponent_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_transitions"]], label_width),
                      "opponent_courn_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_courn_transitions"]], label_width),
                      "opponent_outcome_transitions" = str_wrap(STRATEGY_LOOKUP[["opponent_outcome_transitions"]], label_width),
                      "opponent_outcome_prev_transition_dual" = str_wrap(STRATEGY_LOOKUP[["opponent_outcome_prev_transition_dual"]], label_width))
  q = unique(slider_data$statement)
  slider_data %>%
    ggplot(aes(x = bot_strategy, y = mean_resp, color = bot_strategy)) +
    geom_point(size = 6) +
    geom_errorbar(aes(ymin = se_lower, ymax = se_upper), size = 1, width = 0.25) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        labels = element_blank()) +
    scale_x_discrete(name = element_blank(),
                     labels = strategy_labels) +
    ylim(c(1, 7)) +
    labs(y = "Mean response (1: Strongly disagree, 7: Strongly agree)") +
    ggtitle(str_wrap(q, 50)) +
    default_plot_theme +
    theme(axis.text.x = element_text(angle = 0, vjust = 1),
          axis.title.x = element_blank(),
          legend.position = "none")
}



# PROCESS DATA =================================================================

# Read in data
# data = read_game_data(DATA_FILE)
data = read_data(DATA_FILE)
data$bot_strategy = factor(data$bot_strategy, levels = STRATEGY_LEVELS)
table(data$bot_strategy)


fr_data = read_free_resp_data(FREE_RESP_FILE, data)
slider_data = read_slider_data(SLIDER_FILE, data)
slider_summary = get_slider_summary(slider_data)



# ANALYSIS: Bot strategy win count differentials ===============================
wcd_all = get_bot_strategy_win_count_differential(data)
wcd_summary = get_bot_strategy_win_count_differential_summary(wcd_all)

plot_bot_strategy_win_count_differential_summary(wcd_summary)



# ANALYSIS: Bot strategy win percentages by block ==============================

subject_block_data = get_subject_block_data(data, blocksize = 30)
block_data_summary = get_block_data_summary(subject_block_data)

plot_bot_strategy_win_pct_by_block(block_data_summary)




# ANALYSIS: Free response ======================================================

fr_data %>%
  arrange(bot_strategy, strategy, game_id, player_id, free_resp_answer) %>%
  select(strategy, game_id, player_id, free_resp_answer)


# ANALYSIS: Slider scales ======================================================

slider_qs = unique(slider_summary$statement)

q1_plot = slider_summary %>%
  filter(statement == slider_qs[1]) %>%
  plot_slider_data()

q2_plot = slider_summary %>%
  filter(statement == slider_qs[2]) %>%
  plot_slider_data()

q3_plot = slider_summary %>%
  filter(statement == slider_qs[3]) %>%
  plot_slider_data()

q4_plot = slider_summary %>%
  filter(statement == slider_qs[4]) %>%
  plot_slider_data()

q5_plot = slider_summary %>%
  filter(statement == slider_qs[5]) %>%
  plot_slider_data()


q1_plot + q2_plot + q3_plot + q4_plot + q5_plot +
  plot_layout(ncol = 2)
