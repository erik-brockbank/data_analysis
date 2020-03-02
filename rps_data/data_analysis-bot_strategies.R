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

NUM_ROUNDS = 300

# ANALYSIS FUNCTIONS ==============================================================================

read_data = function(filename) {
  data = read_csv(filename)
  return(data)
}

get_empirical_win_count_differential = function(data) {
  # NB: this is different from the empirical win count differential in v1 because
  # we care about human wins - bot wins, not absolute value between each player
  win_diff = data %>%
    group_by(game_id, player_id, is_bot) %>%
    count(win_count = player_outcome == "win") %>%
    filter(win_count == TRUE) %>%
    group_by(game_id) %>%
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

get_win_count_differential_summary = function(strategy_data, strategy) {
  strategy_data %>%
    summarize(strategy = strategy,
              mean_win_count_diff = mean(win_count_diff),
              n = n(),
              se = sd(win_count_diff) / sqrt(n),
              lower_se = mean_win_count_diff - se,
              upper_se = mean_win_count_diff + se)
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
plot_win_count_differential = function(win_diff, group, min, max) {
  avg = round(mean(win_diff$win_count_diff), 2)
  sd = round(sd(win_diff$win_count_diff), 1)
  win_diff %>%
    ggplot(aes(x = win_count_diff, color = group, fill = group)) +
    geom_histogram(alpha = 0.5, breaks = c(seq(min, max, by = 25))) +
    geom_text(aes(x = 5, y = 4, label = str_c("Mean: ", avg, " (sd: ", sd, ")")), color = "red", size = 4) +
    labs(x = "win count differential", y = "count") +
    # Explicit ylim values used to optimize graphs
    # ylim(c(0, 6000)) +
    # ylim(c(0, 20)) +
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

plot_null_win_count_differential = function(win_diff, group, min, max) {
  win_diff %>%
    ggplot(aes(x = win_count_diff, color = group, fill = group)) +
    geom_histogram(alpha = 0.5, breaks = c(seq(min, max, by = 5))) +
    labs(x = "win count differential", y = "count") +
    # Explicit ylim values used to optimize graphs
    # ylim(c(0, 6000)) +
    # ylim(c(0, 20)) +
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

# Mean + error of win count differentials for each strategy group
label_width = 10
summary_labels = c("Previous move (+)" = str_wrap("Previous move (+)", label_width),
                   "Previous move (-)" = str_wrap("Previous move (-)", label_width),
                   "Opponent previous move (+)" = str_wrap("Opponent previous move (+)", label_width),
                   "Opponent previous move (0)" = str_wrap("Opponent previous move (0)", label_width),
                   "Win-stay-lose-positive" = str_wrap("Win-stay-lose-positive", label_width),
                   "Win-positive-lose-negative" = str_wrap("Win-positive-lose-negative", label_width))

summary_labels_ordered = c("Previous move (+)", "Previous move (-)", 
                           "Opponent previous move (+)", "Opponent previous move (0)")
                           # "Win-stay-lose-positive", "Win-positive-lose-negative")

plot_win_count_differential_summary = function(wcd_summary) {
  wcd_summary %>%
    ggplot(aes(x = strategy, y = mean_win_count_diff)) +
    geom_point(aes(color = strategy),
               size = 6) +
    geom_errorbar(aes(color = strategy, ymin = lower_se, ymax = upper_se),
                  width = 0.25, size = 1) +
    geom_hline(yintercept = 0, size = 2, linetype = "dashed", color = "red") +
    labs(x = "", y = "mean win count differential") +
    ggtitle("Win count differential across bot strategies") +
    scale_x_discrete(limits = summary_labels_ordered, labels = summary_labels) +
    scale_color_viridis(discrete = TRUE,
                        name = element_blank()) +
    default_plot_theme +
    theme(
      plot.title = element_text(size = 32, face = "bold"),
      axis.title.y = element_text(size = 24, face = "bold"),
      axis.text.x = element_text(size = 20, face = "bold", angle = 0, vjust = 1),
      axis.text.y = element_text(face = "bold", size = 20),
      legend.position = "none"
    )
}


# DATA PROCESSING =================================================================================

# Read in data
data = read_data(DATA_FILE)
length(unique(data$game_id))

slider_data = read_data(SLIDER_FILE)
fr_data = read_data(FREE_RESP_FILE)


# Remove incomplete data
incomplete_subjects = data %>%
  # filter(is_bot == 0) %>%
  group_by(player_id) %>%
  summarize(rounds = max(round_index)) %>%
  filter(rounds < NUM_ROUNDS) %>%
  select(player_id)

data = data %>%
  filter(!(player_id %in% incomplete_subjects$player_id))
length(unique(data$game_id))

# Remove incomplete slider and free response data participants
slider_data = slider_data %>%
  filter(!(player_id %in% incomplete_subjects$player_id))

fr_data = fr_data %>%
  filter(!(player_id %in% incomplete_subjects$player_id))


# any repeat survey codes?
repeat_codes = data %>%
  group_by(sona_survey_code) %>%
  filter(is_bot == 0) %>%
  summarize(trials = n()) %>%
  filter(trials > 300) %>%
  select(sona_survey_code)


duplicate_sona = data %>%
  filter(sona_survey_code %in% repeat_codes$sona_survey_code &
           is_bot == 0  &
           round_index == NUM_ROUNDS) %>%
  select(sona_survey_code, game_id, player_id, round_begin_ts) %>%
  # remove the earlier one since the later one has free response and slider data
  group_by(sona_survey_code) %>%
  filter(round_begin_ts == min(round_begin_ts)) %>%
  # inner_join(fr_data, by = c("game_id", "player_id")) %>%
  # inner_join(slider_data, by = c("game_id", "player_id")) %>%
  distinct(game_id)

data = data %>%
  filter(!game_id %in% duplicate_sona$game_id)
length(unique(data$game_id))

# TODO should we include the data for these people where they also filled out the survey after? Or the earliest data?


# How many complete participants do we have for each bot strategy?
data %>%
  filter(is_bot == 0) %>%
  group_by(bot_strategy) %>%
  summarize(n = n() / NUM_ROUNDS)


# ANALYSIS ========================================================================================

# Histogram of empirical win count differential for each strategy
wcd_prev_move_positive = data %>%
  filter(bot_strategy == "prev_move_positive") %>%
  get_empirical_win_count_differential()
wcd_prev_move_negative = data %>%
  filter(bot_strategy == "prev_move_negative") %>%
  get_empirical_win_count_differential()
wcd_opponent_prev_move_positive = data %>%
  filter(bot_strategy == "opponent_prev_move_positive") %>%
  get_empirical_win_count_differential()
wcd_opponent_prev_move_nil = data %>%
  filter(bot_strategy == "opponent_prev_move_nil") %>%
  get_empirical_win_count_differential()
wcd_win_nil_lose_positive = data %>%
  filter(bot_strategy == "win_nil_lose_positive") %>%
  get_empirical_win_count_differential()
wcd_win_positive_lose_negative = data %>%
  filter(bot_strategy == "win_positive_lose_negative") %>%
  get_empirical_win_count_differential()

# What should histogram bins start at?
min(wcd_prev_move_positive$win_count_diff)
min(wcd_prev_move_negative$win_count_diff)
min(wcd_opponent_prev_move_positive$win_count_diff)
min(wcd_opponent_prev_move_nil$win_count_diff)
min(wcd_win_nil_lose_positive$win_count_diff)
min(wcd_win_positive_lose_negative$win_count_diff)

hist_prev_move_positive = plot_win_count_differential(wcd_prev_move_positive, "Previous move (+)", -50, NUM_ROUNDS)
hist_prev_move_negative = plot_win_count_differential(wcd_prev_move_negative, "Previous move (-)", -50, NUM_ROUNDS)
hist_opponent_prev_move_positive = plot_win_count_differential(wcd_opponent_prev_move_positive, "Opponent previous move (+)", -50, NUM_ROUNDS)
hist_opponent_prev_move_nil = plot_win_count_differential(wcd_opponent_prev_move_nil, "Opponent previous move (0)", -50, NUM_ROUNDS)
hist_win_nil_lose_positive = plot_win_count_differential(wcd_win_nil_lose_positive, "Win-stay-lose-positive", -50, NUM_ROUNDS)
hist_win_positive_lose_negative = plot_win_count_differential(wcd_win_positive_lose_negative, "Win-positive-lose-negative", -50, NUM_ROUNDS)

# Null win count differential
wcd_null = get_null_win_count_differential(10000)
hist_null = plot_null_win_count_differential(wcd_null, "Chance behavior", -50, 300)

# Generate plots with patchwork
hist_prev_move_positive + hist_prev_move_negative +
  hist_opponent_prev_move_positive + hist_opponent_prev_move_nil +
  # hist_win_nil_lose_positive + hist_win_positive_lose_negative +
  hist_null +
  plot_layout(ncol = 2)





# Mean + SE of win count differential for each strategy
wcd_summary = bind_rows(get_win_count_differential_summary(wcd_prev_move_positive, "Previous move (+)"),
                        get_win_count_differential_summary(wcd_prev_move_negative, "Previous move (-)"),
                        get_win_count_differential_summary(wcd_opponent_prev_move_positive, "Opponent previous move (+)"),
                        get_win_count_differential_summary(wcd_opponent_prev_move_nil, "Opponent previous move (0)")
                        #get_win_count_differential_summary(wcd_win_nil_lose_positive, "Win-stay-lose-positive"),
                        #get_win_count_differential_summary(wcd_win_positive_lose_negative, "Win-positive-lose-negative")
)

plot_win_count_differential_summary(wcd_summary)



# ANALYSIS: Slider ================================================================================

glimpse(slider_data)
unique(slider_data$player_id)
table(slider_data$player_id)

slider_summary = slider_data %>%
  inner_join(data, by = c("game_id", "player_id")) %>%
  distinct(game_id, player_id, bot_strategy, statement, resp)
slider_summary

# TODO graph mean + SE for each strategy on each question (one plot for each question, showing strategies side by side)


# ANALYSIS: Free Response =========================================================================

fr_summary = fr_data %>%
  inner_join(data, by = c("game_id", "player_id")) %>%
  distinct(game_id, player_id, bot_strategy, free_resp_prompt, free_resp_answer)

fr_summary

# TODO make table of responses string wrapped and listed by strategy

