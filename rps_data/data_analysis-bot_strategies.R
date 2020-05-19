#' 
#' RPS bot analysis
#' Examines human performance against bot opponents with varying strategies
#' 



setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")
rm(list = ls())
library(tidyverse)
library(viridis)
library(patchwork)

# To copy latest data to this remote machine:
# 1. on local host: cd /Users/erikbrockbank/web/vullab/data_analysis/rps_data
# 2. on local host: scp rps_v2_data.csv ebrockba@ssrde.ucsd.edu:/home/AD/ebrockba/rps/
# 3. Repeat 2 for `rps_v2_data_freeResp.csv` and `rps_v2_data_sliderData.csv`


# GLOBALS =========================================================================================

DATA_FILE = "rps_v2_data.csv" # name of file containing full dataset for all rounds
FREE_RESP_FILE = "rps_v2_data_freeResp.csv" # file containing free response data by participant
SLIDER_FILE = "rps_v2_data_sliderData.csv" # file containing slider Likert data by participant
NUM_ROUNDS = 300 # number of rounds in each complete game
BLOCKSIZE = NUM_ROUNDS / 10 # number of bins to use for segmented analysis of performance over the full game
STRATEGY_LEVELS = c("prev_move_positive", "prev_move_negative",
                    "opponent_prev_move_positive", "opponent_prev_move_nil",
                    "win_nil_lose_positive", "win_positive_lose_negative",
                    "outcome_transition_dual_dependency")
STRATEGY_LOOKUP = list("prev_move_positive" = "Previous move (+)",
                       "prev_move_negative" = "Previous move (-)",
                       "opponent_prev_move_positive" = "Opponent previous move (+)",
                       "opponent_prev_move_nil" = "Opponent previous move (0)",
                       "win_nil_lose_positive" = "Win-stay-lose-positive",
                       "win_positive_lose_negative" = "Win-positive-lose-negative",
                       "outcome_transition_dual_dependency" = "Outcome-transition dual dependency")

# ANALYSIS FUNCTIONS ==============================================================================

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

get_empirical_win_count_differential = function(data) {
  # NB: this is different from the empirical win count differential in v1 because
  # we care about human wins - bot wins, not absolute value between each player
  win_diff = data %>%
    group_by(bot_strategy, game_id, player_id, is_bot) %>%
    count(win_count = player_outcome == "win") %>%
    filter(win_count == TRUE) %>%
    group_by(bot_strategy, game_id) %>%
    summarize(win_count_diff = n[is_bot == 0] - n[is_bot == 1]) %>%
    as.data.frame()
  return(win_diff)
}

get_null_win_count_differential = function(reps) {
  # NB: this differs slightly from the empirical null from v1 because
  # we care about humans *beating* bots, not absolute value between both opponents
  win_diff_null = data.frame(
    win_count_diff = replicate(reps, sum(sample(c(-1, 0, 1), NUM_ROUNDS, replace = T)))
  )
  return(win_diff_null)
}

get_win_count_differential_summary = function(strategy_data) {
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


# Get loss percent for each bot dependent on their previous move
get_bot_prev_move_loss_pct = function(data) {
  data %>%
    filter(bot_strategy == "prev_move_positive" | bot_strategy == "prev_move_negative") %>%
    group_by(player_id) %>%
    mutate(prev_move = lag(player_move, 1)) %>%
    filter(is_bot == 1, # look only at bot prev moves
           !is.na(prev_move), # lag call above sets NA for lag on first move: ignore it here
           prev_move != "none") %>%
    group_by(bot_strategy, game_id, player_id, prev_move) %>%
    count(player_outcome) %>%
    filter(!is.na(player_outcome)) %>% # TODO why do we have NA game outcomes??
    group_by(bot_strategy, game_id, player_id, prev_move) %>%
    # player win percent calculated as bot loss percent
    summarize(player_win_pct = n[player_outcome == "loss"] / sum(n))
}

# Get win percent for each player dependent on their own previous move
get_player_prev_move_win_pct = function(data) {
  data %>%
    filter(is_bot == 0,
           bot_strategy == "opponent_prev_move_nil" | bot_strategy == "opponent_prev_move_positive") %>%
    group_by(player_id) %>%
    mutate(prev_move = lag(player_move, 1)) %>%
    filter(!is.na(prev_move), # lag call above sets NA for lag on first move: ignore it here
           prev_move != "none") %>%
    group_by(bot_strategy, game_id, player_id, prev_move) %>%
    count(player_outcome) %>%
    filter(!is.na(player_outcome)) %>% # TODO why do we have NA game outcomes??
    group_by(bot_strategy, game_id, player_id, prev_move) %>%
    # player win percent calculated using player win outcomes
    summarize(player_win_pct = n[player_outcome == "win"] / sum(n))
}


# Get loss percent for each bot dependent on the bot's previous outcome
get_bot_prev_outcome_loss_pct = function(data) {
  data %>%
    filter(bot_strategy == "win_nil_lose_positive" | bot_strategy == "win_positive_lose_negative") %>%
    group_by(player_id) %>%
    mutate(prev_outcome = lag(player_outcome, 1)) %>%
    filter(is_bot == 1, # look only at bot prev moves
           !is.na(prev_outcome)) %>% # lag call above sets NA for lag on first move: ignore it here
    group_by(bot_strategy, game_id, player_id, prev_outcome) %>%
    count(player_outcome) %>%
    filter(!is.na(player_outcome)) %>% # TODO why do we have NA game outcomes??
    group_by(bot_strategy, game_id, player_id, prev_outcome) %>%
    # player win percent calculated as bot loss percent
    summarize(player_win_pct = n[player_outcome == "loss"] / sum(n))
}

# Get loss percent for each bot dependent on the bot's outcome two rounds ago
get_bot_prev2_outcome_loss_pct = function(data) {
  data %>%
    filter(bot_strategy == "win_nil_lose_positive" | bot_strategy == "win_positive_lose_negative") %>%
    group_by(player_id) %>%
    mutate(prev_outcome = lag(player_outcome, 2)) %>%
    filter(is_bot == 1, # look only at bot prev moves
           !is.na(prev_outcome)) %>% 
    group_by(bot_strategy, game_id, player_id, prev_outcome) %>%
    count(player_outcome) %>%
    filter(!is.na(player_outcome)) %>% # TODO why do we have NA game outcomes??
    group_by(bot_strategy, game_id, player_id, prev_outcome) %>%
    # player win percent calculated as bot loss percent
    summarize(player_win_pct = n[player_outcome == "loss"] / sum(n))
}

# Get loss percent for each bot dependent on the transition generated in this round
get_bot_transition_loss_pct = function(data) {
  data %>%
    filter(bot_strategy == "win_nil_lose_positive" | bot_strategy == "win_positive_lose_negative") %>%
    group_by(player_id) %>%
    mutate(prev_move = lag(player_move, 1)) %>%
           #prev_move2 = lag(player_move, 2)) %>%
    filter(is_bot == 1, # look only at bot prev moves
           !is.na(prev_move), # lag call above sets NA for lag on first move: ignore it here
           prev_move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(transition = case_when(prev_move == player_move ~ "0",
                                       ((prev_move == "rock" & player_move == "paper") |
                                          (prev_move == "paper" & player_move == "scissors") |
                                          (prev_move == "scissors" & player_move == "rock")) ~ "+",
                                       ((prev_move == "rock" & player_move == "scissors") |
                                          (prev_move == "paper" & player_move == "rock") |
                                          (prev_move == "scissors" & player_move == "paper")) ~ "-")) %>%
    group_by(bot_strategy, game_id, player_id, transition) %>%
    count(player_outcome) %>%
    filter(!is.na(player_outcome)) %>% # TODO why do we have NA game outcomes??
    group_by(bot_strategy, game_id, player_id, transition) %>%
    summarize(loss_pct = n[player_outcome == "loss"] / sum(n))
}

# Get loss percent for each bot dependent on the transition generated by the *previous* round
get_bot_prev_transition_loss_pct = function(data) {
  data %>%
    filter(bot_strategy == "win_nil_lose_positive" | bot_strategy == "win_positive_lose_negative") %>%
    group_by(player_id) %>%
    mutate(prev_move = lag(player_move, 1),
           prev_move2 = lag(player_move, 2)) %>%
    filter(is_bot == 1, # look only at bot prev moves
           !is.na(prev_move), !is.na(prev_move2), # lag call above sets NA for lag on first two moves: ignore it here
           prev_move2 != "none", prev_move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(transition = case_when(prev_move2 == prev_move ~ "0",
                                  ((prev_move2 == "rock" & prev_move == "paper") |
                                     (prev_move2 == "paper" & prev_move == "scissors") |
                                     (prev_move2 == "scissors" & prev_move == "rock")) ~ "+",
                                  ((prev_move2 == "rock" & prev_move == "scissors") |
                                     (prev_move2 == "paper" & prev_move == "rock") |
                                     (prev_move2 == "scissors" & prev_move == "paper")) ~ "-")) %>%
    group_by(bot_strategy, game_id, player_id, transition) %>%
    count(player_outcome) %>%
    filter(!is.na(player_outcome)) %>% # TODO why do we have NA game outcomes??
    group_by(bot_strategy, game_id, player_id, transition) %>%
    summarize(loss_pct = n[player_outcome == "loss"] / sum(n))
}

# Get summary win percent by strategy (dependent on previous move by bot or player)
get_prev_move_win_pct_summary = function(prev_move_data) {
  prev_move_data %>%
    group_by(bot_strategy, prev_move) %>%
    summarize(mean_player_win_pct = mean(player_win_pct),
              n = n(),
              se = sd(player_win_pct) / sqrt(n),
              se_lower = mean_player_win_pct - se,
              se_upper = mean_player_win_pct + se)
}

# Get summary win percent by strategy (dependent on previous outcome by bot or player)
get_prev_outcome_win_pct_summary = function(bot_loss_prev_outcome) {
  bot_loss_prev_outcome %>%
    group_by(bot_strategy, prev_outcome) %>%
    summarize(mean_player_win_pct = mean(player_win_pct),
              n = n(),
              se = sd(player_win_pct) / sqrt(n),
              se_lower = mean_player_win_pct - se,
              se_upper = mean_player_win_pct + se)
}


# Get summary loss percent (win percent for participants) aggregating over all bots, by strategy (dependent on transition)
get_bot_transition_loss_pct_summary = function(bot_loss_transition) {
  bot_loss_transition %>%
    group_by(bot_strategy, transition) %>%
    summarize(mean_player_win_pct = mean(loss_pct),
              n = n(),
              se = sd(loss_pct) / sqrt(n),
              se_lower = mean_player_win_pct - se,
              se_upper = mean_player_win_pct + se)
}


# GRAPH STYLE =====================================================================================

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


# GRAPH FUNCTIONS =================================================================================

# Histogram of win count differentials for each dyad in a given strategy group
plot_win_count_differential = function(win_diff, group, min, max, labelx, labely) {
  avg = round(mean(win_diff$win_count_diff), 1)
  sd = round(sd(win_diff$win_count_diff), 1)
  win_diff %>%
    ggplot(aes(x = win_count_diff, color = group, fill = group)) +
    geom_histogram(alpha = 0.5, breaks = c(seq(min, max, by = 25))) +
    geom_text(aes(x = labelx, y = labely, label = str_c("Mean: ", avg, " (sd: ", sd, ")")), color = "red", size = 4) +
    labs(x = "win count differential", y = "count") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        begin = 0.2,
                        end = 0.8) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       begin = 0.2, 
                       end = 0.8) +
    ggtitle(group) +
    default_plot_theme +
    theme(legend.position = "none")
}

# Histogram of null win count differential 
# NB: similar to above but with some formatting changes that make it easier to use a separate function
plot_null_win_count_differential = function(win_diff, group, min, max) {
  win_diff %>%
    ggplot(aes(x = win_count_diff, color = group, fill = group)) +
    geom_histogram(alpha = 0.5, breaks = c(seq(min, max, by = 5))) +
    labs(x = "win count differential", y = "count") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        begin = 0.2,
                        end = 0.8) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       begin = 0.2, 
                       end = 0.8) +
    ggtitle(group) +
    default_plot_theme +
    theme(legend.position = "none")
}

# Plot mean + SEM of each strategy
plot_win_count_differential_summary = function(wcd_summary) {
  label_width = 10
  summary_labels = c("prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["prev_move_positive"]], label_width),
                     "prev_move_negative" = str_wrap(STRATEGY_LOOKUP[["prev_move_negative"]], label_width),
                     "opponent_prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_positive"]], label_width),
                     "opponent_prev_move_nil" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_nil"]], label_width),
                     "win_nil_lose_positive" = str_wrap(STRATEGY_LOOKUP[["win_nil_lose_positive"]], label_width),
                     "win_positive_lose_negative" = str_wrap(STRATEGY_LOOKUP[["win_positive_lose_negative"]], label_width),
                     "outcome_transition_dual_dependency" = str_wrap(STRATEGY_LOOKUP[["outcome_transition_dual_dependency"]], label_width))
  
  wcd_summary %>%
    ggplot(aes(x = bot_strategy, y = mean_win_count_diff)) +
    geom_point(aes(color = bot_strategy),
               size = 6) +
    geom_errorbar(aes(color = bot_strategy, ymin = lower_se, ymax = upper_se),
                  width = 0.25, size = 1) +
    geom_hline(yintercept = 0, size = 1, linetype = "dashed", color = "red") +
    labs(x = "", y = "mean win count differential") +
    ggtitle("Win count differential across bot strategies") +
    scale_x_discrete(labels = summary_labels) +
    scale_color_viridis(discrete = TRUE,
                        name = element_blank()) +
    default_plot_theme +
    theme(
      # plot.title = element_text(size = 32, face = "bold"),
      # axis.title.y = element_text(size = 24, face = "bold"),
      # axis.text.x = element_text(size = 20, face = "bold", angle = 0, vjust = 1),
      axis.text.x = element_text(angle = 0, vjust = 1),
      # axis.text.y = element_text(face = "bold", size = 20),
      legend.position = "none"
    )
}

# Plot average of each participant's win percent in blocks of trials by strategy
plot_win_pct_by_block = function(block_data_summary) {
  label_width = 20
  strategy_labels = c("prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["prev_move_positive"]], label_width), 
                      "prev_move_negative" = str_wrap(STRATEGY_LOOKUP[["prev_move_negative"]], label_width),
                      "opponent_prev_move_nil" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_nil"]], label_width),
                      "opponent_prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_positive"]], label_width),
                      "win_nil_lose_positive" = str_wrap(STRATEGY_LOOKUP[["win_nil_lose_positive"]], label_width),
                      "win_positive_lose_negative" = str_wrap(STRATEGY_LOOKUP[["win_positive_lose_negative"]], label_width),
                      "outcome_transition_dual_dependency" = str_wrap(STRATEGY_LOOKUP[["outcome_transition_dual_dependency"]], label_width))
  
  block_data_summary %>%
    ggplot(aes(x = round_block, y = mean_win_pct, color = bot_strategy)) +
    geom_point(size = 6, alpha = 0.75) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), size = 1, width = 0.25, alpha = 0.75) +
    geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
    labs(x = "Block of N rounds", y = "Mean win percentage") +
    ggtitle("Participant win percentage against bot strategies") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        labels = strategy_labels) +
    default_plot_theme +
    theme(axis.text.x = element_blank(),
          legend.text = element_text(face = "plain"))
}

# Plot average win percent based on previous move dependency
plot_prev_move_win_pct = function(bot_loss_summary_prev_move, strategy, xlabel) {
  bot_loss_summary_prev_move %>%
    filter(bot_strategy == strategy) %>%
    ggplot(aes(x = prev_move, y = mean_player_win_pct)) +
    geom_bar(stat = "identity", alpha = 0.5, color = "grey50", fill = "steelblue") +
    geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0.5, size = 1, color = "midnightblue") +
    geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
    labs(x = xlabel, y = "Avg. player win pct.") +
    ggtitle(STRATEGY_LOOKUP[[strategy]]) +
    default_plot_theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 0, vjust = 1))
}

# Plot average win percent based on transition dependency
# NB: this is almost identical to the above
plot_transition_win_pct = function(bot_loss_summary_transition, strategy, xlabel) {
  bot_loss_summary_transition %>%
    filter(bot_strategy == strategy) %>%
    ggplot(aes(x = transition, y = mean_player_win_pct)) +
    geom_bar(stat = "identity", alpha = 0.5, color = "grey50", fill = "steelblue") +
    geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0.5, size = 1, color = "midnightblue") +
    geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
    labs(x = xlabel, y = "Avg. player win pct") +
    ggtitle(STRATEGY_LOOKUP[[strategy]]) +
    default_plot_theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 0, vjust = 1))
}

# Plot average win percent based on outcome dependency
plot_outcome_win_pct = function(bot_loss_summary_prev_outcome, strategy, xlabel) {
  bot_loss_summary_prev_outcome %>%
    filter(bot_strategy == strategy) %>%
    ggplot(aes(x = prev_outcome, y = mean_player_win_pct)) +
    geom_bar(stat = "identity", alpha = 0.5, color = "grey50", fill = "steelblue") +
    geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0.5, size = 1, color = "midnightblue") +
    geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
    labs(x = xlabel, y = "Avg. player win pct") +
    ggtitle(STRATEGY_LOOKUP[[strategy]]) +
    default_plot_theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 0, vjust = 1))
}

plot_slider_data = function(slider_data) {
  label_width = 10
  strategy_labels = c("prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["prev_move_positive"]], label_width), 
                      "prev_move_negative" = str_wrap(STRATEGY_LOOKUP[["prev_move_negative"]], label_width),
                      "opponent_prev_move_nil" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_nil"]], label_width),
                      "opponent_prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_positive"]], label_width),
                      "win_nil_lose_positive" = str_wrap(STRATEGY_LOOKUP[["win_nil_lose_positive"]], label_width),
                      "win_positive_lose_negative" = str_wrap(STRATEGY_LOOKUP[["win_positive_lose_negative"]], label_width),
                      "outcome_transition_dual_dependency" = str_wrap(STRATEGY_LOOKUP[["outcome_transition_dual_dependency"]], label_width))
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



# DATA PROCESSING =================================================================================


# Check for participants with missing data
# data = read_data(DATA_FILE)
# incomplete_codes = c(24156)
# data %>%
#   filter(sona_survey_code %in% incomplete_codes) %>%
#   group_by(sona_survey_code, game_id) %>%
#   filter(is_bot == 0) %>%
#   summarize(rows = n())

# Read in data
data = read_game_data(DATA_FILE)
length(unique(data$game_id))

slider_data = read_data(SLIDER_FILE)
fr_data = read_data(FREE_RESP_FILE)



  


# How many complete participants do we have for each bot strategy?
data %>%
  filter(is_bot == 0) %>%
  group_by(bot_strategy) %>%
  summarize(n = n() / NUM_ROUNDS)

length(unique(data$game_id))


# ANALYSIS ========================================================================================

# Histogram of empirical win count differential for each strategy
# Get win count differentials for each bot strategy
wcd_all = get_empirical_win_count_differential(data)
wcd_null = get_null_win_count_differential(10000)

# Generate histograms of each win count differential
hist_prev_move_positive = plot_win_count_differential(wcd_all[wcd_all$bot_strategy == "prev_move_positive",], STRATEGY_LOOKUP[["prev_move_positive"]], -50, NUM_ROUNDS, 50, 5)
hist_prev_move_negative = plot_win_count_differential(wcd_all[wcd_all$bot_strategy == "prev_move_negative",], STRATEGY_LOOKUP[["prev_move_negative"]], -50, NUM_ROUNDS, 50, 5)
hist_opponent_prev_move_positive = plot_win_count_differential(wcd_all[wcd_all$bot_strategy == "opponent_prev_move_positive",], STRATEGY_LOOKUP[["opponent_prev_move_positive"]], -50, NUM_ROUNDS, 200, 7)
hist_opponent_prev_move_nil = plot_win_count_differential(wcd_all[wcd_all$bot_strategy == "opponent_prev_move_nil",], STRATEGY_LOOKUP[["opponent_prev_move_nil"]], -50, NUM_ROUNDS, 50, 4)
hist_win_nil_lose_positive = plot_win_count_differential(wcd_all[wcd_all$bot_strategy == "win_nil_lose_positive",], STRATEGY_LOOKUP[["win_nil_lose_positive"]], -50, NUM_ROUNDS, 150, 5)
hist_win_positive_lose_negative = plot_win_count_differential(wcd_all[wcd_all$bot_strategy == "win_positive_lose_negative",], STRATEGY_LOOKUP[["win_positive_lose_negative"]], -50, NUM_ROUNDS, 180, 4)
hist_outcome_transition_dual_dep = plot_win_count_differential(wcd_all[wcd_all$bot_strategy == "outcome_transition_dual_dependency",], STRATEGY_LOOKUP[["outcome_transition_dual_dependency"]], -50, NUM_ROUNDS, 100, 8)
hist_null = plot_null_win_count_differential(wcd_null, "Chance behavior", -50, 300)


# Generate plots with patchwork
hist_prev_move_positive + hist_prev_move_negative +
  hist_opponent_prev_move_positive + hist_opponent_prev_move_nil +
  hist_win_nil_lose_positive + hist_win_positive_lose_negative +
  hist_outcome_transition_dual_dep +
  hist_null +
  plot_layout(ncol = 2)





# Mean + SE of win count differential for each strategy
wcd_summary = get_win_count_differential_summary(wcd_all)
plot_win_count_differential_summary(wcd_summary)




# Accuracy by round
subject_block_data = get_subject_block_data(data, blocksize = 30)
block_data_summary = get_block_data_summary(subject_block_data)
# TODO why are we missing subject data for some intermittent blocks?
# block_data_summary %>% 
#   filter(bot_strategy == "opponent_prev_move_nil") %>% 
#   select(subjects) %>% 
#   unique()

plot_win_pct_by_block(block_data_summary)




# Which aspects of each strategy did players detect?

# 1. Bot previous move strategies
bot_loss_prev_move = get_bot_prev_move_loss_pct(data)
bot_loss_summary_prev_move = get_prev_move_win_pct_summary(bot_loss_prev_move)
  
# Generate plots
prev_move_positive_plot = plot_prev_move_win_pct(bot_loss_summary_prev_move, "prev_move_positive", "Bot previous move")
prev_move_negative_plot = plot_prev_move_win_pct(bot_loss_summary_prev_move, "prev_move_negative", "Bot previous move")

# 2. Player previous move strategies
player_win_prev_move = get_player_prev_move_win_pct(data)
player_win_summary_prev_move = get_prev_move_win_pct_summary(player_win_prev_move)

# Generate plots
opponent_prev_move_positive_plot = plot_prev_move_win_pct(player_win_summary_prev_move, "opponent_prev_move_positive", "Player previous move")
opponent_prev_move_nil_plot = plot_prev_move_win_pct(player_win_summary_prev_move, "opponent_prev_move_nil", "Player previous move")

# 3. Bot previous outcome
bot_loss_prev_outcome = get_bot_prev_outcome_loss_pct(data)
bot_loss_summary_prev_outcome = get_prev_outcome_win_pct_summary(bot_loss_prev_outcome)

# Generate plots
win_nil_lose_positive_plot_outcome = plot_outcome_win_pct(bot_loss_summary_prev_outcome, "win_nil_lose_positive", "Bot previous outcome")
win_positive_lose_negative_plot_outcome = plot_outcome_win_pct(bot_loss_summary_prev_outcome, "win_positive_lose_negative", "Bot previous outcome")

# 4. Bot two outcomes back
bot_loss_prev2_outcome = get_bot_prev2_outcome_loss_pct(data)
bot_loss_summary_prev2_outcome = get_prev_outcome_win_pct_summary(bot_loss_prev2_outcome)

# Generate plots
win_nil_lose_positive_plot_outcome_prev = plot_outcome_win_pct(bot_loss_summary_prev2_outcome, "win_nil_lose_positive", "Bot ante-previous outcome")
win_positive_lose_negative_plot_outcome_prev = plot_outcome_win_pct(bot_loss_summary_prev2_outcome, "win_positive_lose_negative", "Bot ante-previous outcome")



# Plot using patchwork
prev_move_positive_plot + prev_move_negative_plot +
  opponent_prev_move_positive_plot + opponent_prev_move_nil_plot +
  win_nil_lose_positive_plot_outcome + win_positive_lose_negative_plot_outcome +
  win_nil_lose_positive_plot_outcome_prev + win_positive_lose_negative_plot_outcome_prev +
  # win_nil_lose_positive_plot + win_positive_lose_negative_plot +
  # win_nil_lose_positive_plot_prev + win_positive_lose_negative_plot_prev +
  plot_layout(ncol = 2)





# ANALYSIS: Slider ================================================================================

slider_data = read_slider_data(SLIDER_FILE, data)

slider_summary = get_slider_summary(slider_data)

# Generate plots
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
  


# ANALYSIS: Free Response =========================================================================

fr_data = read_free_resp_data(FREE_RESP_FILE, data)

fr_data %>%
  arrange(bot_strategy, strategy, game_id, player_id, free_resp_answer) %>%
  select(strategy, game_id, player_id, free_resp_answer)


