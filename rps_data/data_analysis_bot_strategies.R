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


# ANALYSIS FUNCTIONS ==============================================================================

read_data = function(filename) {
  data = read_csv(filename)
  return(data)
}

get_empirical_win_count_differential = function(data) {
  win_diff = data %>%
    group_by(game_id, player_id) %>%
    count(win_count = player_outcome == "win") %>%
    filter(win_count == TRUE) %>%
    group_by(game_id) %>%
    mutate(opp_win_count = lag(n, 1)) %>%
    filter(!is.na(opp_win_count)) %>%
    summarize(win_diff = abs(n - opp_win_count))
  return(win_diff)
}

get_null_win_count_differential = function(reps) {
  win_diff_null = data.frame(
    win_diff = replicate(reps, abs(sum(sample(c(-1, 0, 1), 300, replace = T))))
  )
  return(win_diff_null)
}

get_win_count_differential_summary = function(strategy_data, strategy) {
  strategy_data %>%
    summarize(strategy = strategy,
              mean_win_diff = mean(win_diff),
              n = n(),
              se = sd(win_diff) / sqrt(n),
              lower_se = mean_win_diff - se,
              upper_se = mean_win_diff + se)
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
plot_win_count_differential = function(win_diff, group) {
  win_diff %>%
    ggplot(aes(x = win_diff, color = group, fill = group)) +
    geom_histogram(alpha = 0.5, breaks = c(seq(0, 60, by = 10))) +
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
summary_labels = c("Sampled null data" = str_wrap("Sampled null data", label_width),
                   "Previous move (+)" = str_wrap("Previous move (+)", label_width),
                   "Previous move (-)" = str_wrap("Previous move (-)", label_width),
                   "Opponent previous move (+)" = str_wrap("Opponent previous move (+)", label_width),
                   "Opponent previous move (0)" = str_wrap("Opponent previous move (0)", label_width))

plot_win_count_differential_summary = function(wcd_summary) {
  wcd_summary %>%
    ggplot(aes(x = strategy, y = mean_win_diff)) +
    geom_point(aes(color = strategy),
               size = 6) +
    geom_errorbar(aes(color = strategy, ymin = lower_se, ymax = upper_se),
                  width = 0.25, size = 1) +
    labs(x = "", y = "mean win count differential") +
    ggtitle("Win count differential across bot strategies") +
    scale_x_discrete(labels = summary_labels) +
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


# ANALYSIS ========================================================================================

# Read in data
data = read_data(DATA_FILE)
length(unique(data$game_id))

# Remove incomplete data
incomplete_subjects = data %>%
  group_by(player_id) %>%
  summarize(rounds = max(round_index)) %>%
  filter(rounds < 300) %>%
  select(player_id)
# NB: ignore this step for test data
data = data %>%
  filter(!(player_id %in% incomplete_subjects$player_id))



# Histogram of empirical win count differential for each strategy
unique(data$bot_strategy)
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

hist_prev_move_positive = plot_win_count_differential(wcd_prev_move_positive, "Previous move (+)")
hist_prev_move_negative = plot_win_count_differential(wcd_prev_move_negative, "Previous move (-)")
hist_opponent_prev_move_positive = plot_win_count_differential(wcd_opponent_prev_move_positive, "Opponent previous move (+)")
hist_opponent_prev_move_nil = plot_win_count_differential(wcd_opponent_prev_move_nil, "Opponent previous move (0)")

# Null win count differential
wcd_null = get_null_win_count_differential(10000)
hist_null = plot_win_count_differential(wcd_null, "Sampled null data")

# Generate plots with patchwork
hist_prev_move_positive + hist_prev_move_negative +
  hist_opponent_prev_move_positive + hist_opponent_prev_move_nil +
  hist_null +
  plot_layout(ncol = 2)





# Mean + SE of win count differential for each strategy
wcd_summary = bind_rows(get_win_count_differential_summary(wcd_null, "Sampled null data"),
                        get_win_count_differential_summary(wcd_prev_move_positive, "Previous move (+)"),
                        get_win_count_differential_summary(wcd_prev_move_negative, "Previous move (-)"),
                        get_win_count_differential_summary(wcd_opponent_prev_move_positive, "Opponent previous move (+)"),
                        get_win_count_differential_summary(wcd_opponent_prev_move_nil, "Opponent previous move (0)")
                        )

plot_win_count_differential_summary(wcd_summary)



