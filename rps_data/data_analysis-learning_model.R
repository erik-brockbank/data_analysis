#' 
#' RPS agent learning model
#' Models process of learning bot strategies in order to capture human adaptive reasoning
#' 


#### SETUP ####
setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")
rm(list = ls())
library(tidyverse)
library(viridis)
library(patchwork)


#### GLOBALS ####

DATA_FILE = "rps_v2_data.csv" # name of file containing full dataset for all rounds
GAME_ROUNDS = 300 # number of rounds in each complete game
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


#### DATA PROCESSING FUNCTIONS ####

read_bot_data = function(filename, strategies, game_rounds) {
  data = read_csv(filename)
  data$bot_strategy = factor(data$bot_strategy, levels = strategies)
  
  # Remove all incomplete games
  incomplete_games = data %>%
    group_by(game_id, player_id) %>%
    summarize(rounds = max(round_index)) %>%
    filter(rounds < game_rounds) %>%
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
             round_index == game_rounds) %>%
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

# Identical function above to divide an agent's trials into blocks of size blocksize
# then get each *agent's* win percentage in each block
# TODO consider replacing this functionality with new column set to 1 for win, 0 otherwise; then just get sum of column
get_agent_block_data = function(data, blocksize) {
  data %>%
    group_by(bot_strategy, round_index) %>%
    mutate(round_block = ceiling(round_index / blocksize)) %>%
    select(bot_strategy, round_index, game_id, player_id, agent_outcome, round_block) %>%
    group_by(bot_strategy, game_id, player_id, round_block) %>%
    count(win = agent_outcome == "win") %>%
    mutate(total = sum(n),
           win_pct = n / total) %>%
    filter(win == TRUE)
}

# Take in subject or agent block win percent (calculated above) and summarize by bot strategy across subjects
get_block_data_summary = function(block_data) {
  block_data %>%
    group_by(bot_strategy, round_block) %>%
    summarize(subjects = n(),
              mean_win_pct = mean(win_pct),
              se_win_pct = sd(win_pct) / sqrt(subjects),
              lower_ci = mean_win_pct - se_win_pct,
              upper_ci = mean_win_pct + se_win_pct)
}

# Add counts of each bot's transitions each round relative to its own move
# `prior_count` determines how strongly the agent assumes people are random to start,
# or how much evidence is needed to find strategies more likely
add_transition_counts = function(data, prior_count) {
  # Fill in transition count columns
  data %>%
    group_by(bot_strategy, player_id) %>%
    mutate(c_prev_move_up = cumsum(ifelse(is.na(bot_prev_move),
                                          prior_count, # set count to prior on first move
                                          bot_transition_prev_move == "+")),
           c_prev_move_down = cumsum(ifelse(is.na(bot_prev_move),
                                            prior_count, # set count to prior on first move
                                            bot_transition_prev_move == "-")),
           c_prev_move_stay = cumsum(ifelse(is.na(bot_prev_move),
                                            prior_count, # set count to prior on first move
                                            bot_transition_prev_move == "0"))
           )
  
}

# Add counts of each bot's transitions each round relative to its opponent's previous move
# `prior_count` determines how strongly the agent assumes people are random to start,
# or how much evidence is needed to find strategies more likely
add_opponent_transition_counts = function(data, prior_count) {
  data %>%
    group_by(bot_strategy, player_id) %>%
    mutate(c_opponent_prev_move_up = cumsum(ifelse(is.na(opponent_prev_move),
                                                   prior_count, # set count to prior on first move
                                                   bot_transition_opponent_prev_move == "+")),
           c_opponent_prev_move_down = cumsum(ifelse(is.na(opponent_prev_move),
                                                     prior_count, # set count to prior on first move
                                                     bot_transition_opponent_prev_move == "-")),
           c_opponent_prev_move_stay = cumsum(ifelse(is.na(opponent_prev_move),
                                                     prior_count, # set count to prior on first move
                                                     bot_transition_opponent_prev_move == "0"))
    )
}


# Add counts of each bot's transitions each round given its previous outcome
add_outcome_transition_counts = function(data, prior_count) {
  data %>%
    group_by(bot_strategy, player_id) %>%
    # TODO this is hacky but works...
    mutate(c_win_up = cumsum(ifelse(is.na(bot_prev_outcome),
                                    prior_count, # set count to prior on first move
                                    (bot_prev_outcome == "win" & 
                                       bot_transition_prev_move == "+"))),
           c_win_down = cumsum(ifelse(is.na(bot_prev_outcome),
                                      prior_count, # set count to prior on first move
                                      (bot_prev_outcome == "win" & 
                                         bot_transition_prev_move == "-"))),
           c_win_stay = cumsum(ifelse(is.na(bot_prev_outcome),
                                      prior_count, # set count to prior on first move
                                      (bot_prev_outcome == "win" & 
                                         bot_transition_prev_move == "0"))),
           c_lose_up = cumsum(ifelse(is.na(bot_prev_outcome),
                                     prior_count, # set count to prior on first move
                                     (bot_prev_outcome == "loss" & 
                                        bot_transition_prev_move == "+"))),
           c_lose_down = cumsum(ifelse(is.na(bot_prev_outcome),
                                       prior_count, # set count to prior on first move
                                       (bot_prev_outcome == "loss" & 
                                          bot_transition_prev_move == "-"))),
           c_lose_stay = cumsum(ifelse(is.na(bot_prev_outcome),
                                       prior_count, # set count to prior on first move
                                       (bot_prev_outcome == "loss" & 
                                          bot_transition_prev_move == "0"))),
           c_tie_up = cumsum(ifelse(is.na(bot_prev_outcome),
                                    prior_count, # set count to prior on first move
                                    (bot_prev_outcome == "tie" & 
                                       bot_transition_prev_move == "+"))),
           c_tie_down = cumsum(ifelse(is.na(bot_prev_outcome),
                                      prior_count, # set count to prior on first move
                                      (bot_prev_outcome == "tie" & 
                                         bot_transition_prev_move == "-"))),
           c_tie_stay = cumsum(ifelse(is.na(bot_prev_outcome),
                                      prior_count, # set count to prior on first move
                                      (bot_prev_outcome == "tie" & 
                                         bot_transition_prev_move == "0")))
    )
    
}



# Calculate normalized probabilities of bot making each transition each round based on counts 
# determined in functions above
calculate_probabilities_from_counts = function(data) {
  data %>%
    group_by(bot_strategy, player_id, round_index) %>%
    mutate(
      # Probabilities of each transition relative to previous move
      p_prev_move_up = c_prev_move_up / sum(c_prev_move_up, c_prev_move_down, c_prev_move_stay),
      p_prev_move_down = c_prev_move_down / sum(c_prev_move_up, c_prev_move_down, c_prev_move_stay),
      p_prev_move_stay = c_prev_move_stay / sum(c_prev_move_up, c_prev_move_down, c_prev_move_stay),
      # Probabilities of each transition relative to opponent previous move
      p_opponent_prev_move_up = c_opponent_prev_move_up / sum(c_opponent_prev_move_up, c_opponent_prev_move_down, c_opponent_prev_move_stay),
      p_opponent_prev_move_down = c_opponent_prev_move_down / sum(c_opponent_prev_move_up, c_opponent_prev_move_down, c_opponent_prev_move_stay),
      p_opponent_prev_move_stay = c_opponent_prev_move_stay / sum(c_opponent_prev_move_up, c_opponent_prev_move_down, c_opponent_prev_move_stay),
      # Probabilities of each transition given each previous outcome
      p_win_up = c_win_up / sum(c_win_up, c_win_down, c_win_stay),
      p_win_down = c_win_down / sum(c_win_up, c_win_down, c_win_stay),
      p_win_stay = c_win_stay / sum(c_win_up, c_win_down, c_win_stay),
      p_lose_up = c_lose_up / sum(c_lose_up, c_lose_down, c_lose_stay),
      p_lose_down = c_lose_down / sum(c_lose_up, c_lose_down, c_lose_stay),
      p_lose_stay = c_lose_stay / sum(c_lose_up, c_lose_down, c_lose_stay),
      p_tie_up = c_tie_up / sum(c_tie_up, c_tie_down, c_tie_stay),
      p_tie_down = c_tie_down / sum(c_tie_up, c_tie_down, c_tie_stay),
      p_tie_stay = c_tie_stay / sum(c_tie_up, c_tie_down, c_tie_stay)
    )
}

# # Add expected value for agent of each move based on bot transition probabilities (relative to bot's own previous move)
# # NB: this takes about 30s+ (case eval. seems slow...)
# # TODO: replace POINTS_TIE below with something like points_lookup[outcome_lookup["rock", move_lookup[bot_prev_move, "0"]]]
# calculate_ev_transitions = function(data) {
#   data %>%
#     group_by(bot_strategy, player_id, round_index) %>%
#     mutate(
#       # EV for the agent choosing rock based on bot's transition probabilities (relative to its own prev move)
#       ev_rock_prev_move = case_when(is.na(bot_prev_move) ~ 
#                                       (((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN)),
#                                     bot_prev_move == "rock" ~ 
#                                       # bot's previous move was rock: probability of each bot transition from rock times points agent gets for playing rock against that transition
#                                       ((p_prev_move_stay * POINTS_TIE) + (p_prev_move_up * POINTS_LOSS) + (p_prev_move_down * POINTS_WIN)),
#                                     bot_prev_move == "paper" ~ 
#                                       # bot's previous move was paper: probability of each bot transition from paper times points agent gets for playing rock against that transition
#                                       ((p_prev_move_stay * POINTS_LOSS) + (p_prev_move_up * POINTS_WIN) + (p_prev_move_down * POINTS_TIE)),
#                                     bot_prev_move == "scissors" ~ 
#                                       # bot's previous move was scissors: probability of each bot transition from scissors times points agent gets for playing rock against that transition
#                                       ((p_prev_move_stay * POINTS_WIN) + (p_prev_move_up * POINTS_TIE) + (p_prev_move_down * POINTS_LOSS))),
#       ev_paper_prev_move = case_when(is.na(bot_prev_move) ~ 
#                                        (((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN)),
#                                      bot_prev_move == "rock" ~ 
#                                        # bot's previous move was rock: probability of each bot transition from rock times points agent gets for playing paper against that transition
#                                        ((p_prev_move_stay * POINTS_WIN) + (p_prev_move_up * POINTS_TIE) + (p_prev_move_down * POINTS_LOSS)),
#                                      bot_prev_move == "paper" ~ 
#                                        ((p_prev_move_stay * POINTS_TIE) + (p_prev_move_up * POINTS_LOSS) + (p_prev_move_down * POINTS_WIN)),
#                                      bot_prev_move == "scissors" ~ 
#                                        ((p_prev_move_stay * POINTS_LOSS) + (p_prev_move_up * POINTS_WIN) + (p_prev_move_down * POINTS_TIE))),
#       ev_scissors_prev_move = case_when(is.na(bot_prev_move) ~ 
#                                           (((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN)),
#                                         bot_prev_move == "rock" ~ 
#                                           ((p_prev_move_stay * POINTS_LOSS) + (p_prev_move_up * POINTS_WIN) + (p_prev_move_down * POINTS_TIE)),
#                                         bot_prev_move == "paper" ~ 
#                                           ((p_prev_move_stay * POINTS_WIN) + (p_prev_move_up * POINTS_TIE) + (p_prev_move_down * POINTS_LOSS)),
#                                         bot_prev_move == "scissors" ~ 
#                                           ((p_prev_move_stay * POINTS_TIE) + (p_prev_move_up * POINTS_LOSS) + (p_prev_move_down * POINTS_WIN)))
#     )
# }

# Add expected value for agent of each move based on bot transition probabilities (relative to bot's *opponent's* previous move)
# NB: this takes about 30s+ (case eval. seems slow...)
# TODO: replace POINTS_TIE below with something like points_lookup[outcome_lookup["rock", move_lookup[bot_prev_move, "0"]]]
# calculate_ev_opponent_transitions = function(data) {data %>%
#     group_by(bot_strategy, player_id, round_index) %>%
#     mutate(
#       # EV for the agent choosing rock based on bot's transition probabilities (relative to its opponent's prev move)
#       ev_rock_opponent_prev_move = case_when((is.na(opponent_prev_move) | opponent_prev_move == "none") ~ 
#                                                as.numeric((((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN))),
#                                              opponent_prev_move == "rock" ~ 
#                                                # bot's opponent's previous move was rock: probability of each bot transition (from opponent rock) times points agent gets for playing rock against that transition
#                                                ((p_opponent_prev_move_stay * POINTS_TIE) + (p_opponent_prev_move_up * POINTS_LOSS) + (p_opponent_prev_move_down * POINTS_WIN)),
#                                              opponent_prev_move == "paper" ~ 
#                                                # bot's opponent's previous move was paper: probability of each bot transition (from opponent paper) times points agent gets for playing rock against that transition
#                                                ((p_opponent_prev_move_stay * POINTS_LOSS) + (p_opponent_prev_move_up * POINTS_WIN) + (p_opponent_prev_move_down * POINTS_TIE)),
#                                              opponent_prev_move == "scissors" ~ 
#                                                # bot's opponent's previous move was scissors: probability of each bot transition (from opponent scissors) times points agent gets for playing rock against that transition
#                                                ((p_opponent_prev_move_stay * POINTS_WIN) + (p_opponent_prev_move_up * POINTS_TIE) + (p_opponent_prev_move_down * POINTS_LOSS))),
#       ev_paper_opponent_prev_move = case_when((is.na(opponent_prev_move) | opponent_prev_move == "none") ~ 
#                                                 (((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN)),
#                                               opponent_prev_move == "rock" ~ 
#                                                 # bot's opponent's previous move was rock: probability of each bot transition (from opponent rock) times points agent gets for playing scissors against that transition
#                                                 ((p_opponent_prev_move_stay * POINTS_WIN) + (p_opponent_prev_move_up * POINTS_TIE) + (p_opponent_prev_move_down * POINTS_LOSS)),
#                                               opponent_prev_move == "paper" ~ 
#                                                 ((p_opponent_prev_move_stay * POINTS_TIE) + (p_opponent_prev_move_up * POINTS_LOSS) + (p_opponent_prev_move_down * POINTS_WIN)),
#                                               opponent_prev_move == "scissors" ~ 
#                                                 ((p_opponent_prev_move_stay * POINTS_LOSS) + (p_opponent_prev_move_up * POINTS_WIN) + (p_opponent_prev_move_down * POINTS_TIE))),
#       ev_scissors_opponent_prev_move = case_when((is.na(opponent_prev_move) | opponent_prev_move == "none") ~ 
#                                                    (((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN)),
#                                                  opponent_prev_move == "rock" ~ 
#                                                    ((p_opponent_prev_move_stay * POINTS_LOSS) + (p_opponent_prev_move_up * POINTS_WIN) + (p_opponent_prev_move_down * POINTS_TIE)),
#                                                  opponent_prev_move == "paper" ~ 
#                                                    ((p_opponent_prev_move_stay * POINTS_WIN) + (p_opponent_prev_move_up * POINTS_TIE) + (p_opponent_prev_move_down * POINTS_LOSS)),
#                                                  opponent_prev_move == "scissors" ~ 
#                                                    ((p_opponent_prev_move_stay * POINTS_TIE) + (p_opponent_prev_move_up * POINTS_LOSS) + (p_opponent_prev_move_down * POINTS_WIN)))
#     )
# }




# globals for vector-based EV calculations
POINTS_TIE = 0 # TODO put these somewhere useful... maybe pass them into the functions as well, or replace with a lookup
POINTS_WIN = 3
POINTS_LOSS = -1

MOVE_CHOICES = c(0, 1, 2) # rock, paper, scissors
MOVE_NAMES = c("rock", "paper", "scissors")
MOVE_LOOKUP = c("0" = "rock", "1" = "paper", "2" = "scissors")

BOT_NEXT_MOVE = matrix(rep(MOVE_CHOICES, 3), 
                       nrow = 3,
                       dimnames = list(MOVE_CHOICES, MOVE_CHOICES))
AGENT_NEXT_MOVE = matrix(rep(MOVE_CHOICES, each = 3), 
                         nrow = 3,
                         dimnames = list(MOVE_CHOICES, MOVE_CHOICES))

OUTCOME_MAT = (AGENT_NEXT_MOVE - BOT_NEXT_MOVE) %% 3 # rows = opponent move, cols = agent move
OUTCOME_MAT[OUTCOME_MAT == 1] = POINTS_WIN
OUTCOME_MAT[OUTCOME_MAT == 2] = POINTS_LOSS

WIN_LOOKUP = OUTCOME_MAT # opponent's move in rows, player's move in cols
WIN_LOOKUP[WIN_LOOKUP == POINTS_WIN] = 1
WIN_LOOKUP[WIN_LOOKUP == POINTS_LOSS | WIN_LOOKUP == POINTS_TIE] = 0 # TODO there must be a better way to do this...
rownames(WIN_LOOKUP) = MOVE_NAMES
colnames(WIN_LOOKUP) = MOVE_NAMES


TRANSITION_LOOKUP = matrix(c(c("0", "+", "-"), 
                             c("-", "0", "+"),
                             c("+", "-", "0")),
                           byrow = TRUE,
                           nrow = 3,
                           dimnames = list(MOVE_NAMES, MOVE_NAMES))



softmax = function(vals, k) {
  x = 0:(length(vals) - 1)
  sample(x, size = 1, prob = exp(vals * k) / sum(exp(vals * k)))
}

# transition_probs = (p_transition_up, p_transition_down, p_transition_stay)
# prev_move = "rock", "paper" or "scissors"; determines what next move would be for each transition probability
# TODO consider further splitting this out to add opponent move probabilities to data in one function, then calculate EV in another
calculate_ev_vec = function(transition_probs, prev_move) {
  # get probability of one move to next given transition probabilities
  # TODO should be possible not to have to declare this every time
  transition_mat = matrix(c(transition_probs[3], transition_probs[1], transition_probs[2], # row = previous move, col = next move
                            transition_probs[2], transition_probs[3], transition_probs[1],
                            transition_probs[1], transition_probs[2], transition_probs[3]), 
                          byrow = T,
                          nrow = 3,
                          dimnames = list(MOVE_CHOICES, MOVE_CHOICES))
  
  prev_move = as.character(which(MOVE_NAMES == prev_move) - 1) # this can be 0, 1, or 2 (as string) and can be bot prev move or bot opponent prev move
  p_bot_next_move = matrix(rep(transition_mat[prev_move,], 3), # rows are bot next move probabilities, columns are agent moves
                           nrow = 3)
  EV = colSums(p_bot_next_move * OUTCOME_MAT)
  return(EV)
}


calculate_ev_vals = function(data) {
  data %>%
    mutate(ev_vals_prev_move = ifelse(is.na(bot_prev_move),
                                      list(c(2 / 3, 2 / 3, 2 / 3)),
                                      list(calculate_ev_vec(c(p_prev_move_up, p_prev_move_down, p_prev_move_stay), bot_prev_move))),
           ev_vals_opponent_prev_move = ifelse((is.na(opponent_prev_move) | opponent_prev_move == "none"),
                                               list(c(2 / 3, 2 / 3, 2 / 3)),
                                               list(calculate_ev_vec(c(p_opponent_prev_move_up, p_opponent_prev_move_down, p_opponent_prev_move_stay),
                                                                     opponent_prev_move))),
           ev_vals_combined_prev_moves = list(ev_vals_prev_move[[1]] + ev_vals_opponent_prev_move[[1]]),
           # TODO should this be split out across outcomes?
           ev_vals_prev_outcome = ifelse(is.na(bot_prev_outcome),
                                         list(c(2 / 3, 2 / 3, 2 / 3)),
                                         # TODO can we get rid of case_when here? ...
                                         case_when(
                                           bot_prev_outcome == "win" ~ 
                                             list(calculate_ev_vec(c(p_win_up, p_win_down, p_win_stay), bot_prev_move)),
                                           bot_prev_outcome == "loss" ~ 
                                             list(calculate_ev_vec(c(p_lose_up, p_lose_down, p_lose_stay), bot_prev_move)),
                                           bot_prev_outcome == "tie" ~ 
                                             list(calculate_ev_vec(c(p_tie_up, p_tie_down, p_tie_stay), bot_prev_move)))
                                         ),
           ev_vals_total = list(ev_vals_prev_move[[1]] + ev_vals_opponent_prev_move[[1]] + ev_vals_prev_outcome[[1]])
    )
}


choose_agent_move = function(data) {
  data %>%
    group_by(bot_strategy, player_id, round_index) %>%
    # TODO keep this as numeric value
    mutate(agent_move_prev_move = MOVE_LOOKUP[as.character(softmax(ev_vals_prev_move[[1]], k = SOFTMAX))],
           agent_move_opponent_prev_move = MOVE_LOOKUP[as.character(softmax(ev_vals_opponent_prev_move[[1]], k = SOFTMAX))],
           agent_move_combined_prev_moves = MOVE_LOOKUP[as.character(softmax(ev_vals_combined_prev_moves[[1]], k = SOFTMAX))],
           agent_move_prev_outcome = MOVE_LOOKUP[as.character(softmax(ev_vals_prev_outcome[[1]], k = SOFTMAX))],
           agent_move_combined = MOVE_LOOKUP[as.character(softmax(ev_vals_total[[1]], k = SOFTMAX))]
    )
}








# # Function to aggregate EV calculations across all bot strategies for each move
# calculate_total_evs = function(data) {
#   data %>%
#     group_by(bot_strategy, player_id, round_index) %>%
#     mutate(ev_rock_total = sum(ev_rock_prev_move, ev_rock_opponent_prev_move),
#            ev_paper_total = sum(ev_paper_prev_move, ev_paper_opponent_prev_move),
#            ev_scissors_total = sum(ev_scissors_prev_move, ev_scissors_opponent_prev_move))
# }

# # Function to choose move based on max. EV across all strategies
# choose_max_ev_move_all_transitions = function(data) {
#   data %>%
#     group_by(bot_strategy, player_id, round_index) %>%
#     mutate(agent_move = case_when((ev_rock_total == ev_paper_total & ev_paper_total == ev_scissors_total) ~ 
#                                     # when all EVs are equal, choose randomly
#                                     sample(c("rock", "paper", "scissors"), size = 1, prob = c(1/3, 1/3, 1/3)),
#                                   max(ev_rock_total, ev_paper_total, ev_scissors_total) == ev_rock_total ~ "rock",
#                                   max(ev_rock_total, ev_paper_total, ev_scissors_total) == ev_paper_total ~ "paper",
#                                   max(ev_rock_total, ev_paper_total, ev_scissors_total) == ev_scissors_total ~ "scissors")
#     )
# }

# # Function to choose move based on EV across only bot previous move transition strategies
# choose_max_ev_move_self_transitions = function(data) {
#   data %>%
#     group_by(bot_strategy, player_id, round_index) %>%
#     mutate(agent_move = case_when((ev_rock_prev_move == ev_paper_prev_move & ev_paper_prev_move == ev_scissors_prev_move) ~ 
#                                     # when all EVs are equal, choose randomly
#                                     sample(c("rock", "paper", "scissors"), size = 1, prob = c(1/3, 1/3, 1/3)),
#                                   max(ev_rock_prev_move, ev_paper_prev_move, ev_scissors_prev_move) == ev_rock_prev_move ~ "rock",
#                                   max(ev_rock_prev_move, ev_paper_prev_move, ev_scissors_prev_move) == ev_paper_prev_move ~ "paper",
#                                   max(ev_rock_prev_move, ev_paper_prev_move, ev_scissors_prev_move) == ev_scissors_prev_move ~ "scissors")
#     )
#   
# }

# Function to choose move based on EV across only opponent previous move transition strategies
# choose_max_ev_move_opponent_transitions = function(data) {
#   data %>%
#     group_by(bot_strategy, player_id, round_index) %>%
#     mutate(agent_move = case_when((ev_rock_opponent_prev_move == ev_paper_opponent_prev_move & ev_paper_opponent_prev_move == ev_scissors_opponent_prev_move) ~ 
#                                     # when all EVs are equal, choose randomly
#                                     sample(c("rock", "paper", "scissors"), size = 1, prob = c(1/3, 1/3, 1/3)),
#                                   max(ev_rock_opponent_prev_move, ev_paper_opponent_prev_move, ev_scissors_opponent_prev_move) == ev_rock_opponent_prev_move ~ "rock",
#                                   max(ev_rock_opponent_prev_move, ev_paper_opponent_prev_move, ev_scissors_opponent_prev_move) == ev_paper_opponent_prev_move ~ "paper",
#                                   max(ev_rock_opponent_prev_move, ev_paper_opponent_prev_move, ev_scissors_opponent_prev_move) == ev_scissors_opponent_prev_move ~ "scissors")
#     )
#   
# }

# # Function to determine agent's result from choosing max EV move against each bot
# get_agent_outcomes = function(data) {
#   data %>%
#     group_by(bot_strategy, player_id, round_index) %>%
#     mutate(agent_outcome = case_when(agent_move == player_move ~ "tie",
#                                      ((agent_move == "rock" & player_move == "paper") |
#                                         (agent_move == "paper" & player_move == "scissors") |
#                                         (agent_move == "scissors" & player_move == "rock")) ~ "loss",
#                                      ((agent_move == "rock" & player_move == "scissors") |
#                                         (agent_move == "paper" & player_move == "rock") |
#                                         (agent_move == "scissors" & player_move == "paper")) ~ "win"))
# }


#### GRAPHING FUNCTIONS ####

individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 24),
  axis.title.y = element_text(face = "bold", size = 20),
  axis.title.x = element_text(face = "bold", size = 20),
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

# Plot average of each participant's win percent in blocks of trials by strategy
plot_bot_strategy_win_pct_by_block = function(block_data_summary, title, legend_pos) {
  label_width = 20
  strategy_labels = c("prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["prev_move_positive"]], label_width), 
                      "prev_move_negative" = str_wrap(STRATEGY_LOOKUP[["prev_move_negative"]], label_width),
                      "opponent_prev_move_nil" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_nil"]], label_width),
                      "opponent_prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_positive"]], label_width),
                      "win_nil_lose_positive" = str_wrap(STRATEGY_LOOKUP[["win_nil_lose_positive"]], label_width),
                      "win_positive_lose_negative" = str_wrap(STRATEGY_LOOKUP[["win_positive_lose_negative"]], label_width),
                      "outcome_transition_dual_dependency" = str_wrap(STRATEGY_LOOKUP[["outcome_transition_dual_dependency"]], label_width))
  
  block_labels = c("1" = "30", "2" = "60", "3" = "90", "4" = "120", "5" = "150",
                   "6" = "180", "7" = "210", "8" = "240", "9" = "270", "10" = "300")
  
  block_data_summary %>%
    ggplot(aes(x = round_block, y = mean_win_pct, color = bot_strategy)) +
    geom_point(size = 6, alpha = 0.75) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), size = 1, width = 0.25, alpha = 0.75) +
    geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
    labs(x = "Game round", y = "Mean win percentage") +
    ggtitle(title) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        labels = strategy_labels) +
    scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
    scale_y_continuous(breaks = seq(0.25, 0.85, by = 0.1), 
                       labels = seq(0.25, 0.85, by = 0.1),
                       limits = c(0.25, 0.85)) +
    individ_plot_theme +
    theme(
      legend.text = element_text(face = "bold", size = 14),
      legend.position = legend_pos,
      legend.spacing.y = unit(5.0, 'cm'),
      legend.key.size = unit(4, 'lines'))
}



#### ANALYSIS ####

# Read in and process data
bot_data = read_bot_data(DATA_FILE, STRATEGY_LEVELS, GAME_ROUNDS)
length(unique(bot_data$game_id))

# How many complete participants do we have for each bot strategy?
bot_data %>%
  filter(is_bot == 0) %>%
  group_by(bot_strategy) %>%
  summarize(n = n() / GAME_ROUNDS)

# Get data for bots only, with added column for opponent (participant) move
bot_results = bot_data %>%
  group_by(bot_strategy, game_id, round_index) %>%
  mutate(opponent_move = ifelse(is.na(lag(player_move, 1)), lead(player_move, 1), lag(player_move, 1))) %>%
  filter(is_bot == 1)

# Add bot's previous move, opponent's previous move to data, bot's previous outcome
bot_results = bot_results %>%
  group_by(bot_strategy, player_id) %>%
  mutate(bot_prev_move = lag(player_move, 1),
         opponent_prev_move = lag(opponent_move, 1),
         bot_prev_outcome = lag(player_outcome, 1)
         )

bot_results = bot_results %>%
  group_by(bot_strategy, player_id, round_index) %>%
  mutate(bot_transition_prev_move = ifelse(is.na(bot_prev_move),
                                           "none", # this is a bit hacky but avoids dealing with NAs
                                           TRANSITION_LOOKUP[bot_prev_move, player_move]),
         bot_transition_opponent_prev_move = ifelse((is.na(opponent_prev_move) | opponent_prev_move == "none" | is.na(player_move)),
                                                    "none", # this is a bit hacky but avoids dealing with NAs
                                                    TRANSITION_LOOKUP[opponent_prev_move, player_move])
         )

glimpse(bot_results)




#### Bot Strategy Empirical Learning Curves ####

# We want to try and capture these empirical learning curves with our learning agents
subject_block_data = get_subject_block_data(bot_data, blocksize = 30)
block_data_summary = get_block_data_summary(subject_block_data)
human_perf = plot_bot_strategy_win_pct_by_block(block_data_summary, "Participants", "none")
human_perf



#### Bot Strategy Agent Learning Curves ####

# # Add bot counts of each transition relative to previous move, opponent previous move
# bot_results = add_transition_counts(bot_results, 1)
# bot_results = add_opponent_transition_counts(bot_results, 1)
# glimpse(bot_results)
# 
# # Compute probabilities of bot making each transition relative to previous move, opponent previous move
# # TODO add exponential decay to probability calculations
# bot_results = calculate_probabilities_from_counts(bot_results)
# glimpse(bot_results)
# 
# # Get expected value of each next move according to probabilities dictated by bot's transition probabilities
# bot_results = calculate_ev_transitions(bot_results) # NB: this can take 30-45s
# bot_results = calculate_ev_opponent_transitions(bot_results) # NB: this can take 30-45s
# glimpse(bot_results)
# 
# # Choose max expected value move across all predictive models (currently just transitions)
#   # and calculate outcome for agent making this choice
# bot_results = calculate_total_evs(bot_results)
# bot_results_all = choose_max_ev_move_all_transitions(bot_results) # NB: this can take ~30s
# bot_results_prev_move = choose_max_ev_move_self_transitions(bot_results) # NB: this can take ~15-20s
# bot_results_opponent_prev_move = choose_max_ev_move_opponent_transitions(bot_results) # NB: this can take ~15-20s
# 
# glimpse(bot_results_all)
# # sanity check
# table(bot_results_all$agent_move)
# 
# # Compute outcomes for agent moves based on max expected value choices above
# bot_results_all = get_agent_outcomes(bot_results_all) # NB: this can take ~15-20s
# bot_results_prev_move = get_agent_outcomes(bot_results_prev_move) # NB: this can take ~15-20s
# bot_results_opponent_prev_move = get_agent_outcomes(bot_results_opponent_prev_move) # NB: this can take ~15-20s
# 
# glimpse(bot_results_all)







#### Vector analysis ####
# Add bot counts of each transition relative to previous move, opponent previous move
TRANSITION_PRIOR = 25
bot_results = add_transition_counts(bot_results, TRANSITION_PRIOR)
bot_results = add_opponent_transition_counts(bot_results, TRANSITION_PRIOR)
# Add bot counts of each transition relative to previous outcome
OUTCOME_PRIOR = 150
bot_results = add_outcome_transition_counts(bot_results, OUTCOME_PRIOR)
glimpse(bot_results)


# Compute probabilities of bot making each transition relative to previous move, opponent previous move
# TODO add exponential decay to probability calculations
bot_results = calculate_probabilities_from_counts(bot_results)
glimpse(bot_results)

# NEW: replaces above EV calculations
SOFTMAX = 0.75

bot_results_new = calculate_ev_vals(bot_results) # takes ~30s
bot_results_new = choose_agent_move(bot_results_new) # takes ~5s

bot_results_new = bot_results_new %>%
  mutate(agent_prev_move_win = WIN_LOOKUP[player_move, agent_move_prev_move],
         agent_opponent_prev_move_win = WIN_LOOKUP[player_move, agent_move_opponent_prev_move],
         agent_combined_prev_moves_win = WIN_LOOKUP[player_move, agent_move_combined_prev_moves],
         agent_prev_outcome_win = WIN_LOOKUP[player_move, agent_move_prev_outcome],
         agent_combined_win = WIN_LOOKUP[player_move, agent_move_combined])


# Get block summaries
BLOCKSIZE = 30

bot_results_agent_block_summary = bot_results_new %>%
  group_by(bot_strategy, round_index) %>%
  mutate(round_block = ceiling(round_index / BLOCKSIZE)) %>%
  select(bot_strategy, round_index, game_id, player_id, 
         agent_prev_move_win, agent_opponent_prev_move_win, agent_combined_prev_moves_win,
         agent_prev_outcome_win, agent_combined_win, 
         round_block) %>%
  group_by(bot_strategy, game_id, player_id, round_block) %>%
  summarize(total_rounds = n(),
            # win counts
            agent_prev_move_win_count = sum(agent_prev_move_win),
            agent_opponent_prev_move_win_count = sum(agent_opponent_prev_move_win),
            agent_combined_prev_moves_win_count = sum(agent_combined_prev_moves_win),
            agent_prev_outcome_win_count = sum(agent_prev_outcome_win),
            agent_combined_win_count = sum(agent_combined_win),
            # win percents
            agent_prev_move_win_pct = agent_prev_move_win_count / total_rounds,
            agent_opponent_prev_move_win_pct = agent_opponent_prev_move_win_count / total_rounds,
            agent_combined_prev_moves_win_pct = agent_combined_prev_moves_win_count / total_rounds,
            agent_prev_outcome_win_pct = agent_prev_outcome_win_count / total_rounds,
            agent_combined_win_pct = agent_combined_win_count / total_rounds
            )

bot_results_block_summary = bot_results_agent_block_summary %>%
  group_by(bot_strategy, round_block) %>%
  summarize(subjects = n(),
            # Summary stats for prev move agent
            mean_prev_move_win_pct = mean(agent_prev_move_win_pct),
            se_prev_move_win_pct = sd(agent_prev_move_win_pct) / sqrt(subjects),
            lower_ci_prev_move = mean_prev_move_win_pct - se_prev_move_win_pct,
            upper_ci_prev_move = mean_prev_move_win_pct + se_prev_move_win_pct,
            # Summary stats for opponent prev move agent
            mean_opponent_prev_move_win_pct = mean(agent_opponent_prev_move_win_pct),
            se_opponent_prev_move_win_pct = sd(agent_opponent_prev_move_win_pct) / sqrt(subjects),
            lower_ci_opponent_prev_move = mean_opponent_prev_move_win_pct - se_opponent_prev_move_win_pct,
            upper_ci_opponent_prev_move = mean_opponent_prev_move_win_pct + se_opponent_prev_move_win_pct,
            # Summary stats for combined prev moves agent
            mean_combined_prev_moves_win_pct = mean(agent_combined_prev_moves_win_pct),
            se_combined_prev_moves_win_pct = sd(agent_combined_prev_moves_win_pct) / sqrt(subjects),
            lower_ci_combined_prev_moves = mean_combined_prev_moves_win_pct - se_combined_prev_moves_win_pct,
            upper_ci_combined_prev_moves = mean_combined_prev_moves_win_pct + se_combined_prev_moves_win_pct,
            # Summary stats for prev outcome agent
            mean_prev_outcome_win_pct = mean(agent_prev_outcome_win_pct),
            se_prev_outcome_win_pct = sd(agent_prev_outcome_win_pct) / sqrt(subjects),
            lower_ci_prev_outcome = mean_prev_outcome_win_pct - se_prev_outcome_win_pct,
            upper_ci_prev_outcome = mean_prev_outcome_win_pct + se_prev_outcome_win_pct,
            # Summary stats for combined agent
            mean_combined_win_pct = mean(agent_combined_win_pct),
            se_combined_win_pct = sd(agent_combined_win_pct) / sqrt(subjects),
            lower_ci_combined = mean_combined_win_pct - se_combined_win_pct,
            upper_ci_combined = mean_combined_win_pct + se_combined_win_pct
            )


# Plot results

label_width = 20
strategy_labels = c("prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["prev_move_positive"]], label_width), 
                    "prev_move_negative" = str_wrap(STRATEGY_LOOKUP[["prev_move_negative"]], label_width),
                    "opponent_prev_move_nil" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_nil"]], label_width),
                    "opponent_prev_move_positive" = str_wrap(STRATEGY_LOOKUP[["opponent_prev_move_positive"]], label_width),
                    "win_nil_lose_positive" = str_wrap(STRATEGY_LOOKUP[["win_nil_lose_positive"]], label_width),
                    "win_positive_lose_negative" = str_wrap(STRATEGY_LOOKUP[["win_positive_lose_negative"]], label_width),
                    "outcome_transition_dual_dependency" = str_wrap(STRATEGY_LOOKUP[["outcome_transition_dual_dependency"]], label_width))

block_labels = c("1" = "30", "2" = "60", "3" = "90", "4" = "120", "5" = "150",
                 "6" = "180", "7" = "210", "8" = "240", "9" = "270", "10" = "300")

# Prev move agent plot
prev_move_plot = bot_results_block_summary %>%
  ggplot(aes(x = round_block, y = mean_prev_move_win_pct, color = bot_strategy)) +
  geom_point(size = 6, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_ci_prev_move, ymax = upper_ci_prev_move), size = 1, width = 0.25, alpha = 0.75) +
  geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
  labs(x = "Game round", y = "Mean win percentage") +
  ggtitle("Previous move transition agent") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = strategy_labels) +
  scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
  scale_y_continuous(breaks = seq(0.25, 0.85, by = 0.1),
                     labels = seq(0.25, 0.85, by = 0.1),
                     limits = c(0.25, 0.85)) +
  individ_plot_theme +
  theme(
    legend.text = element_text(face = "bold", size = 14),
    legend.position = "right",
    legend.spacing.y = unit(5.0, 'cm'),
    legend.key.size = unit(4, 'lines'))

# Opponent prev move plot
opponent_prev_move_plot = bot_results_block_summary %>%
  ggplot(aes(x = round_block, y = mean_opponent_prev_move_win_pct, color = bot_strategy)) +
  geom_point(size = 6, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_ci_opponent_prev_move, ymax = upper_ci_opponent_prev_move), size = 1, width = 0.25, alpha = 0.75) +
  geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
  labs(x = "Game round", y = "Mean win percentage") +
  ggtitle("Opponent previous move transition agent") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = strategy_labels) +
  scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
  scale_y_continuous(breaks = seq(0.25, 0.85, by = 0.1),
                     labels = seq(0.25, 0.85, by = 0.1),
                     limits = c(0.25, 0.85)) +
  individ_plot_theme +
  theme(
    legend.text = element_text(face = "bold", size = 14),
    legend.position = "right",
    legend.spacing.y = unit(5.0, 'cm'),
    legend.key.size = unit(4, 'lines'))

# Combined prev moves (agent and opponent) plot
combined_prev_moves_plot = bot_results_block_summary %>%
  ggplot(aes(x = round_block, y = mean_combined_prev_moves_win_pct, color = bot_strategy)) +
  geom_point(size = 6, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_ci_combined_prev_moves, ymax = upper_ci_combined_prev_moves), size = 1, width = 0.25, alpha = 0.75) +
  geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
  labs(x = "Game round", y = "Mean win percentage") +
  ggtitle("All prev. moves transition agent") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = strategy_labels) +
  scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
  scale_y_continuous(breaks = seq(0.25, 0.85, by = 0.1),
                     labels = seq(0.25, 0.85, by = 0.1),
                     limits = c(0.25, 0.85)) +
  individ_plot_theme +
  theme(
    legend.text = element_text(face = "bold", size = 14),
    legend.position = "right",
    legend.spacing.y = unit(5.0, 'cm'),
    legend.key.size = unit(4, 'lines'))

# Prev outcome plot
prev_outcome_plot = bot_results_block_summary %>%
  ggplot(aes(x = round_block, y = mean_prev_outcome_win_pct, color = bot_strategy)) +
  geom_point(size = 6, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_ci_prev_outcome, ymax = upper_ci_prev_outcome), size = 1, width = 0.25, alpha = 0.75) +
  geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
  labs(x = "Game round", y = "Mean win percentage") +
  ggtitle("Prev. outcome transition agent") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = strategy_labels) +
  scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
  scale_y_continuous(breaks = seq(0.25, 0.85, by = 0.1),
                     labels = seq(0.25, 0.85, by = 0.1),
                     limits = c(0.25, 0.85)) +
  individ_plot_theme +
  theme(
    legend.text = element_text(face = "bold", size = 14),
    legend.position = "right",
    legend.spacing.y = unit(5.0, 'cm'),
    legend.key.size = unit(4, 'lines'))

# Combined agent plot
combined_agent_plot = bot_results_block_summary %>%
  ggplot(aes(x = round_block, y = mean_combined_win_pct, color = bot_strategy)) +
  geom_point(size = 6, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_ci_combined, ymax = upper_ci_combined), size = 1, width = 0.25, alpha = 0.75) +
  geom_hline(yintercept = 1 / 3, linetype = "dashed", color = "red", size = 1) +
  labs(x = "Game round", y = "Mean win percentage") +
  ggtitle("Combined transition agent") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = strategy_labels) +
  scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
  scale_y_continuous(breaks = seq(0.25, 0.85, by = 0.1),
                     labels = seq(0.25, 0.85, by = 0.1),
                     limits = c(0.25, 0.85)) +
  individ_plot_theme +
  theme(
    legend.text = element_text(face = "bold", size = 14),
    legend.position = "right",
    legend.spacing.y = unit(5.0, 'cm'),
    legend.key.size = unit(4, 'lines'))


human_perf + prev_move_plot
human_perf + opponent_prev_move_plot
human_perf + combined_prev_moves_plot
human_perf + prev_outcome_plot
human_perf + combined_agent_plot








# # Make identical plot as above but based on agent outcomes rather than player outcomes
# agent_block_data_all = get_agent_block_data(bot_results_all, blocksize = 30)
# agent_block_data_summary_all = get_block_data_summary(agent_block_data_all)
# agent_perf_all = plot_bot_strategy_win_pct_by_block(agent_block_data_summary_all, "Agent win percentage", "right")
# agent_perf_all

# Plot human and agent performance alongside each other with patchwork
# human_perf + agent_perf_all


# # Make identical plot as above but based on agent outcomes rather than player outcomes
# agent_block_data_prev_move = get_agent_block_data(bot_results_prev_move, blocksize = 30)
# agent_block_data_summary_prev_move = get_block_data_summary(agent_block_data_prev_move)
# agent_perf_prev_move = plot_bot_strategy_win_pct_by_block(agent_block_data_summary_prev_move, "Agent win percentage: prev move", "right")
# agent_perf_prev_move

# Plot human and agent performance alongside each other with patchwork
# human_perf + agent_perf_prev_move


# # Make identical plot as above but based on agent outcomes rather than player outcomes
# agent_block_data_opponent_prev_move = get_agent_block_data(bot_results_opponent_prev_move, blocksize = 30)
# agent_block_data_summary_opponent_prev_move = get_block_data_summary(agent_block_data_opponent_prev_move)
# agent_perf_opponent_prev_move = plot_bot_strategy_win_pct_by_block(agent_block_data_summary_opponent_prev_move, "Agent win percentage: opp. prev move", "right")
# agent_perf_opponent_prev_move

# Plot human and agent performance alongside each other with patchwork
# human_perf + agent_perf_opponent_prev_move





#### SCRATCH ####


# Compare trials where combined agent won but the other two didn't both win as well
glimpse(bot_results_all)
glimpse(bot_results_prev_move)
glimpse(bot_results_opponent_prev_move)

bot_results_combined = bot_results_all %>%
  inner_join(bot_results_prev_move, by = c("game_id", "player_id", "round_index", "bot_strategy")) %>%
  select(game_id, player_id, round_index, bot_strategy, 
         player_move.x, player_outcome.x, opponent_move.x, 
         bot_prev_move.x, opponent_prev_move.x,
         agent_move.x, agent_outcome.x,
         agent_move.y, agent_outcome.y) %>%
  inner_join(bot_results_opponent_prev_move, by = c("game_id", "player_id", "round_index", "bot_strategy")) %>%
  select(game_id, player_id, round_index, bot_strategy, 
         player_move, player_outcome, opponent_move, bot_prev_move, opponent_prev_move, 
         agent_move.x, agent_outcome.x, agent_move.y, agent_outcome.y, agent_move, agent_outcome,
         ev_rock_total, ev_paper_total, ev_scissors_total,
         ev_rock_prev_move, ev_paper_prev_move, ev_scissors_prev_move,
         ev_rock_opponent_prev_move, ev_paper_opponent_prev_move, ev_scissors_opponent_prev_move) %>%
  rename("bot_move" = player_move,
         "bot_outcome" = player_outcome,
         "full_agent_move" = agent_move.x,
         "full_agent_outcome" = agent_outcome.x,
         "prev_move_agent_move" = agent_move.y,
         "prev_move_agent_outcome" = agent_outcome.y,
         "opponent_prev_move_agent_move" = agent_move,
         "opponent_prev_move_agent_outcome" = agent_outcome)

glimpse(bot_results_combined)

bot_results_combined %>%
  filter(bot_strategy == "win_nil_lose_positive") %>%
  glimpse()

# What happens when the full agent wins despite the individual transition agents not both winning
bot_results_combined %>%
  filter(full_agent_outcome == "win" & prev_move_agent_outcome != "win" & opponent_prev_move_agent_outcome != "win") %>%
  filter(bot_strategy == "win_nil_lose_positive") %>%
  glimpse()

# How many times does the full agent win when one *or the other* individual agent doesn't win
bot_results_combined %>%
  filter(full_agent_outcome == "win" & 
           ((prev_move_agent_outcome == "win" & opponent_prev_move_agent_outcome != "win") |
           (prev_move_agent_outcome != "win" & opponent_prev_move_agent_outcome == "win"))) %>%
  filter(bot_strategy == "win_nil_lose_positive") %>%
  glimpse()

# How many times do both agents win and the full agent wins?
bot_results_combined %>%
  filter(full_agent_outcome == "win" & prev_move_agent_outcome == "win" & opponent_prev_move_agent_outcome == "win") %>%
  filter(bot_strategy == "win_nil_lose_positive") %>%
  glimpse()


# Does it ever happen that both individual agents win and the full agent doesn't?
bot_results_combined %>%
  filter(full_agent_outcome != "win" & prev_move_agent_outcome == "win" & opponent_prev_move_agent_outcome == "win") %>%
  filter(bot_strategy == "win_nil_lose_positive")
# Answer: only once on a first round where the agents are choosing randomly
# TODO when looking at all strategies, this happens once in round 4 against the win-positive... bot: WHY???


# Given the above, how many times does the full agent *not* win and one of the individual agents wins?
bot_results_combined %>%
  filter(full_agent_outcome != "win" & 
           ((prev_move_agent_outcome == "win" & opponent_prev_move_agent_outcome != "win") |
              (prev_move_agent_outcome != "win" & opponent_prev_move_agent_outcome == "win"))) %>%
  filter(bot_strategy == "win_nil_lose_positive") %>%
  glimpse()

bot_results_combined %>%
  filter(full_agent_outcome != "win" & prev_move_agent_outcome != "win" & opponent_prev_move_agent_outcome != "win") %>%
  filter(bot_strategy == "win_nil_lose_positive") %>%
  glimpse()




# Win, loss, tie percents for human players against WSLS bots
pcts = bot_data %>%
  filter(is_bot == 0, !is.na(player_outcome)) %>%
  group_by(bot_strategy, round_index) %>%
  # filter(bot_strategy == "win_positive_lose_negative") %>%
  mutate(round_block = ceiling(round_index / 30)) %>%
  select(bot_strategy, round_index, game_id, player_id, player_outcome, round_block) %>%
  group_by(bot_strategy, game_id, player_id, round_block) %>%
  count(win = player_outcome == "win",
        tie = player_outcome == "tie",
        loss = player_outcome == "loss") %>%
  mutate(total = sum(n),
         win_pct = max(0, n[win == TRUE] / total),
         loss_pct = max(0, n[loss == TRUE] / total),
         tie_pct = max(0, n[tie == TRUE] / total)) %>%
  filter(win == TRUE) # this is hacky but we only need one row


block_labels = c("1" = "30", "2" = "60", "3" = "90", "4" = "120", "5" = "150",
                 "6" = "180", "7" = "210", "8" = "240", "9" = "270", "10" = "300")

pcts %>%
  group_by(bot_strategy, round_block) %>%
  summarize(mean_win_pct = mean(win_pct),
            mean_loss_pct = mean(loss_pct),
            mean_tie_pct = mean(tie_pct)) %>%
  filter(bot_strategy == "win_positive_lose_negative") %>%
  # filter(bot_strategy == "win_nil_lose_positive") %>%
  ggplot(aes(x = round_block)) +
  geom_point(aes(y = mean_win_pct, color = "wins")) +
  geom_line(aes(y = mean_win_pct, color = "wins")) +
  geom_point(aes(y = mean_loss_pct, color = "ties")) +
  geom_line(aes(y = mean_loss_pct, color = "ties")) +
  geom_point(aes(y = mean_tie_pct, color = "losses")) +
  geom_line(aes(y = mean_tie_pct, color = "losses")) +
  labs(y = "Mean percentage", x = "Round", color = "Outcome") +
  ggtitle("Strategy: win-positive, lose-negative") +
  # ggtitle("Strategy: win-stay, lose-positive") +
  scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
  individ_plot_theme



# Look at win, loss, tie percents for agents against WSLS bots
glimpse(bot_results_all)

pct_agent_all = bot_results_all %>%
  group_by(bot_strategy, round_index) %>%
  # filter(bot_strategy == "win_positive_lose_negative") %>%
  mutate(round_block = ceiling(round_index / 30)) %>%
  select(bot_strategy, round_index, game_id, player_id, agent_outcome, round_block) %>%
  group_by(bot_strategy, game_id, player_id, round_block) %>%
  count(win = agent_outcome == "win",
        tie = agent_outcome == "tie",
        loss = agent_outcome == "loss") %>%
  mutate(total = sum(n),
         win_pct = max(0, n[win == TRUE] / total),
         loss_pct = max(0, n[loss == TRUE] / total),
         tie_pct = max(0, n[tie == TRUE] / total)) %>%
  filter(win == TRUE) # this is hacky but we only need one row

pct_agent_all %>%
  group_by(bot_strategy, round_block) %>%
  summarize(mean_win_pct = mean(win_pct),
            mean_loss_pct = mean(loss_pct),
            mean_tie_pct = mean(tie_pct)) %>%
  # filter(bot_strategy == "win_positive_lose_negative") %>%
  filter(bot_strategy == "win_nil_lose_positive") %>%
  ggplot(aes(x = round_block)) +
  geom_point(aes(y = mean_win_pct, color = "wins")) +
  geom_line(aes(y = mean_win_pct, color = "wins")) +
  geom_point(aes(y = mean_loss_pct, color = "ties")) +
  geom_line(aes(y = mean_loss_pct, color = "ties")) +
  geom_point(aes(y = mean_tie_pct, color = "losses")) +
  geom_line(aes(y = mean_tie_pct, color = "losses")) +
  labs(y = "Mean percentage", x = "Round", color = "Outcome") +
  ggtitle("Agent outcome percentage (against win-stay, lose-positive)") +
  scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
  individ_plot_theme


pct_agent_prev_move = bot_results_prev_move %>%
  group_by(bot_strategy, round_index) %>%
  # filter(bot_strategy == "win_positive_lose_negative") %>%
  mutate(round_block = ceiling(round_index / 30)) %>%
  select(bot_strategy, round_index, game_id, player_id, agent_outcome, round_block) %>%
  group_by(bot_strategy, game_id, player_id, round_block) %>%
  count(win = agent_outcome == "win",
        tie = agent_outcome == "tie",
        loss = agent_outcome == "loss") %>%
  mutate(total = sum(n),
         win_pct = max(0, n[win == TRUE] / total),
         loss_pct = max(0, n[loss == TRUE] / total),
         tie_pct = max(0, n[tie == TRUE] / total)) %>%
  filter(win == TRUE) # this is hacky but we only need one row

pct_agent_prev_move %>%
  group_by(bot_strategy, round_block) %>%
  summarize(mean_win_pct = mean(win_pct),
            mean_loss_pct = mean(loss_pct),
            mean_tie_pct = mean(tie_pct)) %>%
  filter(bot_strategy == "win_positive_lose_negative") %>%
  # filter(bot_strategy == "win_nil_lose_positive") %>%
  ggplot(aes(x = round_block)) +
  geom_point(aes(y = mean_win_pct, color = "wins")) +
  geom_line(aes(y = mean_win_pct, color = "wins")) +
  geom_point(aes(y = mean_loss_pct, color = "ties")) +
  geom_line(aes(y = mean_loss_pct, color = "ties")) +
  geom_point(aes(y = mean_tie_pct, color = "losses")) +
  geom_line(aes(y = mean_tie_pct, color = "losses")) +
  labs(y = "Mean percentage", x = "Round", color = "Outcome") +
  ggtitle("Agent outcome percentage (against win-positive, lose-negative)") +
  scale_x_continuous(labels = block_labels, breaks = seq(1:10)) +
  individ_plot_theme




# Pull out sample data for particular strategies/bots
table(bot_results$bot_strategy)
bot_results %>% filter(bot_strategy == "prev_move_positive") %>% distinct(player_id)
sample_bot = bot_results %>% filter(player_id == "eb059987-95a3-41ef-9621-b31bdc348483")
glimpse(sample_bot)

bot_results %>% filter(bot_strategy == "opponent_prev_move_nil") %>% distinct(player_id)
sample_bot = bot_results %>% filter(player_id == "6bf4f7cc-cad4-452b-8ab9-4c567e51333f")
glimpse(sample_bot)



# Matrix version of EV calculations

move_choices = c(0, 1, 2) # rock, paper, scissors
# get probability of one move to next given transition probabilities
transitions = c(0.5, 0.3, 0.2) # p_transition_up, p_transition_down, p_transition_stay
transition_probs = matrix(c(c(transitions[3], transitions[1], transitions[2]), # row = previous move, col = next move
                          c(transitions[2], transitions[3], transitions[1]),
                          c(transitions[1], transitions[2], transitions[3])), 
                          nrow = 3,
                          dimnames = list(move_choices, move_choices))

# globals: move calculate outcome matrix
bot_next_move = matrix(rep(move_choices, 3), 
                       nrow = 3,
                       dimnames = list(move_choices, move_choices))
agent_next_move = matrix(rep(move_choices, each = 3), 
                         nrow = 3,
                         dimnames = list(move_choices, move_choices))
outcome_mat = (agent_next_move - bot_next_move) %% 3 # rows = opponent move, cols = agent move
outcome_mat[outcome_mat == 1] = POINTS_WIN
outcome_mat[outcome_mat == 2] = POINTS_LOSS

prev_move = "0" # this can be 0, 1, or 2 (as string) and can be bot prev move or bot opponent prev move
p_bot_next_move = matrix(rep(transition_probs[prev_move,], 3), 
                         nrow = 3)

EV = colSums(p_bot_next_move * outcome_mat)

softmax = function(vals, k) {
  x = 0:(length(vals) - 1)
  sample(x, size = 1, prob = exp(vals * k) / sum(exp(vals * k)))
}

# Choose move based on EV
softmax(EV, 10)
