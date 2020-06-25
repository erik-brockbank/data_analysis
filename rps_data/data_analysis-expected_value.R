#' 
#' This script does analysis similar to the entropy analyses, except it computes 
#' a distribution over win count differentials by dyad if each person maximizes 
#' expected utility using the probabilities associated with moves given a particular dependency
#' 

rm(list=ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")
source('02_data_analysis-transition_entropy.R') # NB: nested sourcing here. this sources 01 which sources 00


# Globals
# number of RPS rounds played in each complete game
RPS_ROUNDS = 300

# Score differential outcome for each move combination (player in rows, opponent in cols)
OUTCOME_MATRIX = matrix(c(0, -1, 1, 1, 0, -1, -1, 1, 0), nrow = 3, byrow = T)
rownames(OUTCOME_MATRIX) = c("rock", "paper", "scissors")
colnames(OUTCOME_MATRIX) = c("opp_rock", "opp_paper", "opp_scissors")


# Analysis functions

# Get maximum expected win count differential based on move probabilities in player_summary
get_expected_win_count_differential_moves = function(player_summary) {
  player_summary %>%
    group_by(player_id) %>%
    summarize(max_util = max(
      rowSums(matrix(rep(pmove, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))) %>%
    mutate(win_diff = max_util * RPS_ROUNDS)
}

# Get maximum expected win count differential based on transition probabilities in player_trans_summary
# TODO can we unify this with the above? only difference is column name (pmove, p.transition)
get_expected_win_count_differential_trans = function(player_summary) {
  player_summary %>%
    group_by(player_id) %>%
    summarize(max_util = max(
      rowSums(matrix(rep(p.transition, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))) %>%
    mutate(win_diff = max_util * RPS_ROUNDS)
}

# # Get maximum expected win count differential based on transition probabilities *with respect to opponent's prevoius move*
# # TODO again this is identical to the above but for column names...
# get_expected_win_count_differential_trans_cournot = function(player_summary) {
#   player_summary %>%
#     group_by(player_id) %>%
#     summarize(max_util = max(
#       rowSums(matrix(rep(p.transition, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))) %>%
#     mutate(win_diff = max_util * RPS_ROUNDS)
#   
# }

# Get maximum expected win count differential based on distribution of moves given opponent's previous move
get_expected_win_count_differential_opponent_prev_move = function(player_summary) {
  player_summary %>%
    group_by(player_id, opponent.prev.move) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(pmove_opponent.prev.move, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))) %>%
    # normalize expected value for each opponent previous move (uniform)
    mutate(max_util_norm = max_util * (1 / 3)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(player_id) %>%
    summarize(win_diff = sum(max_util_norm) * RPS_ROUNDS)
}

# Get maximum expected win count differential based on distribution of moves given player's previous move
get_expected_win_count_differential_prev_move = function(player_summary) {
  player_summary %>%
    group_by(player_id, prev.move) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(pmove_prev.move, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))) %>%
    # normalize expected value for each previous move (uniform)
    mutate(max_util_norm = max_util * (1 / 3)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(player_id) %>%
    summarize(win_diff = sum(max_util_norm) * RPS_ROUNDS)
}

# Get maximum expected win count differential based on distribution of transitions given player's previous outcome
get_expected_win_count_differential_prev_outcome = function(player_summary) {
  player_summary %>%
    group_by(player_id, prev.outcome) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(p.transition.outcome, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))) %>%
    # normalize expected value for each previous outcome (uniform)
    mutate(max_util_norm = max_util * (1 / 3)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(player_id) %>%
    summarize(win_diff = sum(max_util_norm) * RPS_ROUNDS)
}

# Get maximum expected win count differential based on distribution of moves given player's previous two moves
get_expected_win_count_differential_prev_2moves = function(player_summary) {
  player_summary %>%
    group_by(player_id, prev.move, prev.move2) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(pmove_2prev.move, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))) %>%
    # normalize expected value for each previous two-move combination (uniform)
    mutate(max_util_norm = max_util * (1 / 9)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(player_id) %>%
    summarize(win_diff = sum(max_util_norm) * RPS_ROUNDS)
}

# Get maximum expected win count differential based on distribution of moves given player's previous move, opponent's previous move
get_expected_win_count_differential_prev_move_opponent_prev_move = function(player_summary) {
  player_summary %>%
    group_by(player_id, prev.move, opponent.prev.move) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(pmove_prev.move_opponent.prev.move, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))) %>%
    # normalize expected value for each previous two-move combination (uniform)
    mutate(max_util_norm = max_util * (1 / 9)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(player_id) %>%
    summarize(win_diff = sum(max_util_norm) * RPS_ROUNDS)
}

# Get maximum expected win count differential based on distribution of moves given player's previous transition, previous outcome
get_expected_win_count_differential_prev_transition_prev_outcome = function(player_summary) {
  player_summary %>%
    group_by(player_id, player.prev.transition, prev.outcome) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(p.transition.prev.transition.prev.outcome, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))) %>%
    # normalize expected value for each previous two-move combination (uniform)
    mutate(max_util_norm = max_util * (1 / 9)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(player_id) %>%
    summarize(win_diff = sum(max_util_norm) * RPS_ROUNDS)
}


# Get summary stats for win count differential
get_win_count_differential_summary = function(data, category) {
  data %>%
    summarize(
      category = category,
      mean_wins = mean(win_diff),
      n = n(),
      se = sd(win_diff) / sqrt(n),
      ci_lower = mean_wins - se,
      ci_upper = mean_wins + se
    )
}


# Graphing functions

legend.width = 10
summary_labels = c("empirical" = str_wrap("Empirical results", legend.width),
                   "null_sample" = str_wrap("Random behavior", legend.width),
                   "move_probability" = str_wrap("Choice baserate (R/P/S)", legend.width),
                   "trans_probability" = str_wrap("Transition baserate (+/-/0)", legend.width),
                   "cournot_probability" = str_wrap("Opponent transition baserate (+/-/0)", legend.width),
                   "opponent_prev_move_probability" = str_wrap("Choice given opponent's prior choice", legend.width),
                   "prev_move_probability" = str_wrap("Choice given player's prior choice", legend.width),
                   "player_transition_prev_outcome_probability" = str_wrap("Transition given prior outcome (W/L/T)", legend.width),
                   "prev_2move_probability" = str_wrap("Choice given player's prior two choices", legend.width),
                   "prev_move_opponent_prev_move_probability" = str_wrap("Choice given player's prior choice & opponent's prior choice", legend.width),
                   "player_transition_prev_transition_prev_outcome_probability" = str_wrap("Transition given prior transition & prior outcome", legend.width))

summary_values_legend = c("player_transition_prev_transition_prev_outcome_probability",
                   "prev_move_opponent_prev_move_probability", "prev_2move_probability",
                   "player_transition_prev_outcome_probability",
                   "prev_move_probability", "opponent_prev_move_probability",
                   "trans_probability", "cournot_probability", 
                   "move_probability",
                   "null_sample",
                   "empirical")

summary_values = c("move_probability", 
  "cournot_probability", "trans_probability",
  "opponent_prev_move_probability", "prev_move_probability",
  "player_transition_prev_outcome_probability",
  "prev_2move_probability", "prev_move_opponent_prev_move_probability",
  "player_transition_prev_transition_prev_outcome_probability")

x_values = c("empirical", 
  "null_sample", "move_probability", 
  "cournot_probability", "trans_probability", 
  "opponent_prev_move_probability", "prev_move_probability",
  "player_transition_prev_outcome_probability",
  "prev_2move_probability", "prev_move_opponent_prev_move_probability",
  "player_transition_prev_transition_prev_outcome_probability")

plot_win_differential_summary = function(win_diff_summary, win_diff_empirical, win_diff_null) {
  win_diff_empirical_summary = get_win_count_differential_summary(win_diff_empirical, "empirical")
  
  win_diff_summary %>%
    ggplot(aes(x = factor(category, 
                          # TODO extract these from the object above rather than copy-pasta
                          levels = summary_values), 
               y = mean_wins)) +
    # points for expected value win count diffs
    geom_point(aes(color = factor(category, 
                                  levels = summary_values)), size = 6) +
    # errorbars for expected value win count diffs
    geom_errorbar(aes(color = factor(category, 
                                     levels = summary_values),
                      ymin = ci_lower, ymax = ci_upper), width = 0.25, size = 1) +
    # raw data for empirical win count diffs
    geom_jitter(data = win_diff_empirical, aes(x = factor("empirical"), y = win_diff),
                color = "blue", alpha = 0.5, width = 0.2, size = 4) +
    geom_point(data = win_diff_empirical, aes(x = factor("empirical"), y = mean(win_diff)),
               color = "red", size = 6) +
    geom_errorbar(data = win_diff_empirical_summary, aes(x = factor("empirical"), 
                                                 ymin = ci_lower, ymax = ci_upper),
                  color = "red", width = 0.25, size = 1) +
    # point for mean null win count diff
    geom_point(data = win_diff_null, aes(x = factor("null_sample"), y = mean(win_diff)), size = 6, color = "black") +
    labs(x = "", y = "Dyad win count differential") +
    ggtitle("Theoretical and empirical exploitability of player moves") +
    scale_x_discrete(limits = x_values,
                     labels = summary_labels) +
    scale_color_viridis(discrete = TRUE,
                        name = element_blank()) +
    individ_plot_theme +
    theme(plot.title = element_text(size = 32, face = "bold"),
          axis.title.y = element_text(size = 24, face = "bold"),
          axis.text.x = element_text(size = 20, face = "bold", angle = 0, vjust = 1),
          axis.text.y = element_text(face = "bold", size = 20),
          legend.position = "none")
}


# Analysis 


## 1. Distribution of moves (3 cells)
# get overall probability of each move (for each player)
player_summary = get.player.move.dist(data)
# get max utility value for opponent of each player based on each player's move probabilities
player_utils = get_expected_win_count_differential_moves(player_summary)
# plot expected win differentials based on move probability data
plot.win.differentials(player_utils, "Distribution of win count differentials", "Maximum expected (move probabilities)")


## 2. Distribution of transitions (3 cells)
# get overall probability of each transition (for each player)
player_transition_summary = get.player.transition.dist(data)
# get max utility value for opponent of each player based on each player's transition probabilities
player_transition_utils = get_expected_win_count_differential_trans(player_transition_summary)

## 2.5 Distribution of transitions *relative to opponent*, i.e. Cournot responses (3 cells)
# get overall probability of each transition (for each player)
player_transition_cournot_summary = get.player.transition.cournot.dist(data)
# get max utility value for opponent of each player based on each player's transition probabilities
player_transition_cournot_utils = get_expected_win_count_differential_trans(player_transition_cournot_summary)


## 3. Distribution of moves given opponent's previous move (9 cells)
# get probability of each move for each player given their opponent's previous move
opponent_prev_move_summary = get.opponent.prev.move.cond.probs(data)
# get max utility value for opponent of each player based on each player's move probabilities *given their opponent's previous move*
opponent_prev_move_utils = get_expected_win_count_differential_opponent_prev_move(opponent_prev_move_summary)


## 4. Distribution of moves given player's previous move (9 cells)
# get probability of each move for each player given their own previous move
player_prev_move_summary = get.player.prev.move.cond.probs(data)
# get max utility value for opponent of each player based on each player's move probabilities *given their previous move*
player_prev_move_utils = get_expected_win_count_differential_prev_move(player_prev_move_summary)


## 5. Distribution of transitions given previous outcome (9 cells)
# get probability of each transition for each player given their previous outcome
player_transition_prev_outcome_summary = get.player.transition.outcome.cond(data) 
# get max utility value for opponent of ech player based on each player's transition probabilities *given their previous outcome*
player_transition_prev_outcome_utils = get_expected_win_count_differential_prev_outcome(player_transition_prev_outcome_summary)
  

## 6. Distribution of moves given player's previous two moves (27 cells)
# get probability of each move for each player given their previous two moves
player_prev_2move_summary = get.player.prev.2move.cond.probs(data)
# get max utility value for opponent of each player based on each player's move probabiliteis *given their previous two moves*
player_prev_2move_utils = get_expected_win_count_differential_prev_2moves(player_prev_2move_summary)



## 7. Distribution of moves given player's previous move, opponent's previous move (27 cells)
# get probability of each move for each player given their previous move, their opponent's previous move
player_opponent_prev_move_summary = get.player.opponent.prev.move.cond.probs(data) 
# get max utility value for opponent of each player based on each player's move probabilities *given their previous move and their opponent's previous move*
player_opponent_prev_move_utils = get_expected_win_count_differential_prev_move_opponent_prev_move(player_opponent_prev_move_summary)


## 8. Distribution of transitions given player's previous transition and previous outcome (27 cells)
# get probability of each transition for each player given their previous transition and player's previous outcome
player_transition_prev_transition_prev_outcome_summary = get.player.transition.prev.transition.prev.outcome.cond(data) 
# get max utility value for opponent of each player based on each player's transition probabilities *given their previous transition and previous outcome*
player_transition_prev_transition_prev_outcome_utils = get_expected_win_count_differential_prev_transition_prev_outcome(player_transition_prev_transition_prev_outcome_summary)
  


# combine summary win count differentials for empirical data, null sample, and expected value calcs
win_count_diff_summary = bind_rows(
  get_win_count_differential_summary(player_utils, "move_probability"),
  get_win_count_differential_summary(player_transition_utils, "trans_probability"),
  get_win_count_differential_summary(player_transition_cournot_utils, "cournot_probability"),
  get_win_count_differential_summary(opponent_prev_move_utils, "opponent_prev_move_probability"),
  get_win_count_differential_summary(player_prev_move_utils, "prev_move_probability"),
  get_win_count_differential_summary(player_transition_prev_outcome_utils, "player_transition_prev_outcome_probability"),
  get_win_count_differential_summary(player_prev_2move_utils, "prev_2move_probability"),
  get_win_count_differential_summary(player_opponent_prev_move_utils, "prev_move_opponent_prev_move_probability"),
  get_win_count_differential_summary(player_transition_prev_transition_prev_outcome_utils, "player_transition_prev_transition_prev_outcome_probability")
)


# plot summary of win differentials for EV alongside empirical and null data
plot_win_differential_summary(win_count_diff_summary, win_count_diff_empirical, win_count_diff_null)





### Sanity Check ###
pl_test = player_summary %>% filter(player_id == "0333c73a-6d1f-4cc9-afde-d0d833bc47f8")

pl_null = data.frame(
  player_move = c("rock", "paper", "scissors"),
  pmove = c(1/3, 1/3, 1/3))

pl_mat = matrix(rep(c(pl_test$pmove[pl_test$player_move == "rock"], 
                      pl_test$pmove[pl_test$player_move == "paper"],
                      pl_test$pmove[pl_test$player_move == "scissors"]), 3), nrow = 3, byrow = T)

pl_mat * OUTCOME_MATRIX
rowSums(pl_mat * OUTCOME_MATRIX)
max(rowSums(pl_mat * OUTCOME_MATRIX))



### Appendix ###


# instead of maximizing, assume some kind of probability matching
player_utils_softmax = player_summary %>%
  group_by(player_id) %>%
  summarize(
    min_util = sort(rowSums(matrix(rep(pmove, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))[1],
    mid_util = sort(rowSums(matrix(rep(pmove, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))[2],
    max_util = sort(rowSums(matrix(rep(pmove, 3), nrow = 3, byrow = T) * OUTCOME_MATRIX))[3]
  ) %>%
  group_by(player_id) %>%
  mutate(win_diff = sum((max_util * 300 * .8), (mid_util * 300 * .1), (min_util * 300 * .1))) %>%
  ungroup()

# plot expected win differentials based on revised "softmax" above against move probability data
plot.win.differentials(player_utils_softmax, "Distribution of win count differentials", "Maximum expected (move probabilities)")
