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
add_transition_counts = function(data) {
  # Fill in transition count columns
  data %>%
    group_by(bot_strategy, player_id) %>%
    mutate(c_prev_move_up = cumsum(ifelse(is.na(bot_prev_move),
                                        TRUE, # set count to 1 on first move
                                        ((bot_prev_move == "rock" & player_move == "paper") |
                                           (bot_prev_move == "paper" & player_move == "scissors") |
                                           (bot_prev_move == "scissors" & player_move == "rock")))),
           c_prev_move_down = cumsum(ifelse(is.na(bot_prev_move),
                                          TRUE, # set count to 1 on first move
                                          ((bot_prev_move == "rock" & player_move == "scissors") |
                                             (bot_prev_move == "paper" & player_move == "rock") |
                                             (bot_prev_move == "scissors" & player_move == "paper")))),
           c_prev_move_stay = cumsum(ifelse(is.na(bot_prev_move),
                                          TRUE, # set count to 1 on first move
                                          ((bot_prev_move == player_move)))))
  
}

# Add counts of each bot's transitions each round relative to its opponent's previous move
add_opponent_transition_counts = function(data) {
  data %>%
    group_by(bot_strategy, player_id) %>%
    mutate(c_opponent_prev_move_up = cumsum(ifelse(is.na(opponent_prev_move),
                                                 TRUE, # set count to 1 on first move
                                                 ((opponent_prev_move == "rock" & player_move == "paper") |
                                                    (opponent_prev_move == "paper" & player_move == "scissors") |
                                                    (opponent_prev_move == "scissors" & player_move == "rock")))),
           c_opponent_prev_move_down = cumsum(ifelse(is.na(opponent_prev_move),
                                                   TRUE, # set count to 1 on first move
                                                   ((opponent_prev_move == "rock" & player_move == "scissors") |
                                                      (opponent_prev_move == "paper" & player_move == "rock") |
                                                      (opponent_prev_move == "scissors" & player_move == "paper")))),
           c_opponent_prev_move_stay = cumsum(ifelse(is.na(opponent_prev_move),
                                                   TRUE, # set count to 1 on first move
                                                   ((opponent_prev_move == player_move)))))
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
      p_opponent_prev_move_stay = c_opponent_prev_move_stay / sum(c_opponent_prev_move_up, c_opponent_prev_move_down, c_opponent_prev_move_stay)
    )
}

# Add expected value for agent of each move based on bot transition probabilities (relative to bot's own previous move)
# NB: this takes about 30s+ (case eval. seems slow...)
# TODO: replace POINTS_TIE below with something like points_lookup[outcome_lookup["rock", move_lookup[bot_prev_move, "0"]]]
calculate_ev_transitions = function(data) {
  data %>%
    group_by(bot_strategy, player_id, round_index) %>%
    mutate(
      # EV for the agent choosing rock based on bot's transition probabilities (relative to its own prev move)
      ev_rock_prev_move = case_when(is.na(bot_prev_move) ~ 
                                      (((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN)),
                                    bot_prev_move == "rock" ~ 
                                      # bot's previous move was rock: probability of each bot transition from rock times points agent gets for playing rock against that transition
                                      ((p_prev_move_stay * POINTS_TIE) + (p_prev_move_up * POINTS_LOSS) + (p_prev_move_down * POINTS_WIN)),
                                    bot_prev_move == "paper" ~ 
                                      # bot's previous move was paper: probability of each bot transition from paper times points agent gets for playing rock against that transition
                                      ((p_prev_move_stay * POINTS_LOSS) + (p_prev_move_up * POINTS_WIN) + (p_prev_move_down * POINTS_TIE)),
                                    bot_prev_move == "scissors" ~ 
                                      # bot's previous move was scissors: probability of each bot transition from scissors times points agent gets for playing rock against that transition
                                      ((p_prev_move_stay * POINTS_WIN) + (p_prev_move_up * POINTS_TIE) + (p_prev_move_down * POINTS_LOSS))),
      ev_paper_prev_move = case_when(is.na(bot_prev_move) ~ 
                                       (((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN)),
                                     bot_prev_move == "rock" ~ 
                                       # bot's previous move was rock: probability of each bot transition from rock times points agent gets for playing paper against that transition
                                       ((p_prev_move_stay * POINTS_WIN) + (p_prev_move_up * POINTS_TIE) + (p_prev_move_down * POINTS_LOSS)),
                                     bot_prev_move == "paper" ~ 
                                       ((p_prev_move_stay * POINTS_TIE) + (p_prev_move_up * POINTS_LOSS) + (p_prev_move_down * POINTS_WIN)),
                                     bot_prev_move == "scissors" ~ 
                                       ((p_prev_move_stay * POINTS_LOSS) + (p_prev_move_up * POINTS_WIN) + (p_prev_move_down * POINTS_TIE))),
      ev_scissors_prev_move = case_when(is.na(bot_prev_move) ~ 
                                          (((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN)),
                                        bot_prev_move == "rock" ~ 
                                          ((p_prev_move_stay * POINTS_LOSS) + (p_prev_move_up * POINTS_WIN) + (p_prev_move_down * POINTS_TIE)),
                                        bot_prev_move == "paper" ~ 
                                          ((p_prev_move_stay * POINTS_WIN) + (p_prev_move_up * POINTS_TIE) + (p_prev_move_down * POINTS_LOSS)),
                                        bot_prev_move == "scissors" ~ 
                                          ((p_prev_move_stay * POINTS_TIE) + (p_prev_move_up * POINTS_LOSS) + (p_prev_move_down * POINTS_WIN)))
    )
}

# Add expected value for agent of each move based on bot transition probabilities (relative to bot's *opponent's* previous move)
# NB: this takes about 30s+ (case eval. seems slow...)
# TODO: replace POINTS_TIE below with something like points_lookup[outcome_lookup["rock", move_lookup[bot_prev_move, "0"]]]
calculate_ev_opponent_transitions = function(data) {data %>%
    group_by(bot_strategy, player_id, round_index) %>%
    mutate(
      # EV for the agent choosing rock based on bot's transition probabilities (relative to its opponent's prev move)
      ev_rock_opponent_prev_move = case_when(is.na(opponent_prev_move) ~ 
                                               as.numeric((((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN))),
                                             opponent_prev_move == "rock" ~ 
                                               # bot's opponent's previous move was rock: probability of each bot transition (from opponent rock) times points agent gets for playing rock against that transition
                                               ((p_opponent_prev_move_stay * POINTS_TIE) + (p_opponent_prev_move_up * POINTS_LOSS) + (p_opponent_prev_move_down * POINTS_WIN)),
                                             opponent_prev_move == "paper" ~ 
                                               # bot's opponent's previous move was paper: probability of each bot transition (from opponent paper) times points agent gets for playing rock against that transition
                                               ((p_opponent_prev_move_stay * POINTS_LOSS) + (p_opponent_prev_move_up * POINTS_WIN) + (p_opponent_prev_move_down * POINTS_TIE)),
                                             opponent_prev_move == "scissors" ~ 
                                               # bot's opponent's previous move was scissors: probability of each bot transition (from opponent scissors) times points agent gets for playing rock against that transition
                                               ((p_opponent_prev_move_stay * POINTS_WIN) + (p_opponent_prev_move_up * POINTS_TIE) + (p_opponent_prev_move_down * POINTS_LOSS))),
      ev_paper_opponent_prev_move = case_when(is.na(opponent_prev_move) ~ 
                                                (((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN)),
                                              opponent_prev_move == "rock" ~ 
                                                # bot's opponent's previous move was rock: probability of each bot transition (from opponent rock) times points agent gets for playing scissors against that transition
                                                ((p_opponent_prev_move_stay * POINTS_WIN) + (p_opponent_prev_move_up * POINTS_TIE) + (p_opponent_prev_move_down * POINTS_LOSS)),
                                              opponent_prev_move == "paper" ~ 
                                                ((p_opponent_prev_move_stay * POINTS_TIE) + (p_opponent_prev_move_up * POINTS_LOSS) + (p_opponent_prev_move_down * POINTS_WIN)),
                                              opponent_prev_move == "scissors" ~ 
                                                ((p_opponent_prev_move_stay * POINTS_LOSS) + (p_opponent_prev_move_up * POINTS_WIN) + (p_opponent_prev_move_down * POINTS_TIE))),
      ev_scissors_opponent_prev_move = case_when(is.na(opponent_prev_move) ~ 
                                                   (((1 / 3) * POINTS_TIE) + ((1 / 3) * POINTS_LOSS) + ((1 / 3) * POINTS_WIN)),
                                                 opponent_prev_move == "rock" ~ 
                                                   ((p_opponent_prev_move_stay * POINTS_LOSS) + (p_opponent_prev_move_up * POINTS_WIN) + (p_opponent_prev_move_down * POINTS_TIE)),
                                                 opponent_prev_move == "paper" ~ 
                                                   ((p_opponent_prev_move_stay * POINTS_WIN) + (p_opponent_prev_move_up * POINTS_TIE) + (p_opponent_prev_move_down * POINTS_LOSS)),
                                                 opponent_prev_move == "scissors" ~ 
                                                   ((p_opponent_prev_move_stay * POINTS_TIE) + (p_opponent_prev_move_up * POINTS_LOSS) + (p_opponent_prev_move_down * POINTS_WIN)))
    )
}


# Function to aggregate EV calculations across all bot strategies for each move
calculate_total_evs = function(data) {
  data %>%
    group_by(bot_strategy, player_id, round_index) %>%
    mutate(ev_rock_total = sum(ev_rock_prev_move, ev_rock_opponent_prev_move),
           ev_paper_total = sum(ev_paper_prev_move, ev_paper_opponent_prev_move),
           ev_scissors_total = sum(ev_scissors_prev_move, ev_scissors_opponent_prev_move))
}

# Function to choose move based on max. EV across all strategies
choose_max_ev_move = function(data) {
  data %>%
    group_by(bot_strategy, player_id, round_index) %>%
    mutate(agent_move = case_when((ev_rock_total == ev_paper_total & ev_paper_total == ev_scissors_total) ~ 
                                    # when all EVs are equal, choose randomly
                                    sample(c("rock", "paper", "scissors"), size = 1, prob = c(1/3, 1/3, 1/3)),
                                  max(ev_rock_total, ev_paper_total, ev_scissors_total) == ev_rock_total ~ "rock",
                                  max(ev_rock_total, ev_paper_total, ev_scissors_total) == ev_paper_total ~ "paper",
                                  max(ev_rock_total, ev_paper_total, ev_scissors_total) == ev_scissors_total ~ "scissors")
    )
}

# Function to determine agent's result from choosing max EV move against each bot
get_agent_outcomes = function(data) {
  data %>%
    group_by(bot_strategy, player_id, round_index) %>%
    mutate(agent_outcome = case_when(agent_move == player_move ~ "tie",
                                     ((agent_move == "rock" & player_move == "paper") |
                                        (agent_move == "paper" & player_move == "scissors") |
                                        (agent_move == "scissors" & player_move == "rock")) ~ "loss",
                                     ((agent_move == "rock" & player_move == "scissors") |
                                        (agent_move == "paper" & player_move == "rock") |
                                        (agent_move == "scissors" & player_move == "paper")) ~ "win"))
}


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
    ylim(c(0.3, 1.0)) +
    individ_plot_theme +
    theme(#axis.text.x = element_blank(),
      legend.text = element_text(face = "bold", size = 14),
      legend.position = legend_pos,
      legend.spacing.y = unit(5.0, 'cm'),
      #legend.key = element_rect(size = 5),
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

# Add bot's previous move, opponent's previous move to data
bot_results = bot_results %>%
  group_by(bot_strategy, player_id) %>%
  mutate(bot_prev_move = lag(player_move, 1),
         opponent_prev_move = lag(opponent_move, 1))

glimpse(bot_results)


#### Bot Strategy Empirical Learning Curves ####

# We want to try and capture these empirical learning curves with our learning agents
subject_block_data = get_subject_block_data(bot_data, blocksize = 30)
block_data_summary = get_block_data_summary(subject_block_data)
human_perf = plot_bot_strategy_win_pct_by_block(block_data_summary, "Participant win percentage", "none")


#### Bot Strategy Agent Learning Curves ####

# Add bot counts of each transition relative to previous move, opponent previous move
bot_results = add_transition_counts(bot_results)
bot_results = add_opponent_transition_counts(bot_results)
glimpse(bot_results)

# Compute probabilities of bot making each transition relative to previous move, opponent previous move
# TODO add exponential decay to probability calculations
bot_results = calculate_probabilities_from_counts(bot_results)
glimpse(bot_results)

# Get expected value of each next move according to probabilities dictated by bot's transition probabilities
POINTS_TIE = 0 # TODO put these somewhere useful... maybe pass them into the functions as well, or replace with a lookup
POINTS_WIN = 3
POINTS_LOSS = -1

bot_results = calculate_ev_transitions(bot_results) # NB: this can take 30-45s
bot_results = calculate_ev_opponent_transitions(bot_results) # NB: this can take 30-45s
glimpse(bot_results)

# Choose max expected value move across all predictive models (currently just transitions)
  # and calculate outcome for agent making this choice
bot_results = calculate_total_evs(bot_results)
bot_results = choose_max_ev_move(bot_results) # NB: this can take ~30s
glimpse(bot_results)
# sanity check
table(bot_results$agent_move)

# Compute outcomes for agent moves based on max expected value choices above
bot_results = get_agent_outcomes(bot_results) # NB: this can take ~30s

# Make identical plot as above but based on agent outcomes rather than player outcomes
agent_block_data = get_agent_block_data(bot_results, blocksize = 30)
agent_block_data_summary = get_block_data_summary(agent_block_data)
agent_perf = plot_bot_strategy_win_pct_by_block(agent_block_data_summary, "Agent win percentage", "right")

# Plot human and agent performance alongside each other with patchwork
human_perf + agent_perf



# TODO should there be more weighting about which kind of bot this most likely is? 
# or is the EV underlying probabilities enough to do that?





#### SCRATCH ####


# Pull out sample data for particular strategies/bots
table(bot_results$bot_strategy)
bot_results %>% filter(bot_strategy == "prev_move_positive") %>% distinct(player_id)
sample_bot = bot_results %>% filter(player_id == "eb059987-95a3-41ef-9621-b31bdc348483")
glimpse(sample_bot)

bot_results %>% filter(bot_strategy == "opponent_prev_move_nil") %>% distinct(player_id)
sample_bot = bot_results %>% filter(player_id == "6bf4f7cc-cad4-452b-8ab9-4c567e51333f")
glimpse(sample_bot)

