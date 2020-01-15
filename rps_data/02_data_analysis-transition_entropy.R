#' README
#' This is a second level data analysis script for RPS data, looking at dependencies 
#' in participant *transition distributions* {+, -, 0} based on previous combinations
#' of participant and opponent transitions (+, -, 0). 
#' 
#' This script should *not* be used for general data overview, cleaning etc. 
#' (that happens in `data_processing.R`) and should not be used for analyses that extend 
#' beyond transition distribution dependencies.



#' NOMENCLATURE
#' With {`R`, `S`, `P`} arranged clockwise:
#' A `+` transition indicates a *counter-clockwise* move (towards what wins), 
#' i.e., `R` -> `P`, `P` -> `S`, `S` -> `R`
#' A `-` transition indicates a *clockwise* move (towards what loses),
#' i.e. `R` -> `S`, `S` -> `P`, `P` -> `R`
#' A `0` transition indicates *staying in place* (`R` -> `R`, etc.)
#' 



setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")
source('01_data_analysis-move_entropy.R') # script used for calculating entropy over move distributions


PLAYER_SET = unique(data$player_id)
OUTCOME_SET = unique(data$player_outcome)
TRANSITION_SET = c("+", "-", "0")



##########################
### ANALYSIS FUNCTIONS ###
##########################


#' Function to get marginal probability of each transition (+/-/0) for each participant
get.player.transition.dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move), # lag call above sets NA for lag on first move: ignore it here
           prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-")) %>%
    count(player.transition) %>%
    mutate(total.transitions = sum(n),
           p.transition = n / total.transitions)
}

#' Function to get the distribution over each player's *previous* transition (+/-/0)
#' Note this will be almost identical to the function above, except
#' it will evaluate the transition between a player's move two moves back and their
#' previous move, rather than the transition implied by previous move, current move
get.player.prev.transition.dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2)) %>%
    filter(!is.na(prev.move), !is.na(prev.move2), # lag call above sets NA for lag on first two moves: ignore it here
           prev.move2 != "none", prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                         ((prev.move2 == "rock" & prev.move == "paper") |
                                            (prev.move2 == "paper" & prev.move == "scissors") |
                                            (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                         ((prev.move2 == "rock" & prev.move == "scissors") |
                                            (prev.move2 == "paper" & prev.move == "rock") |
                                            (prev.move2 == "scissors" & prev.move == "paper")) ~ "-")) %>%
    count(player.prev.transition) %>%
    mutate(total.transitions = sum(n),
           p.prev.transition = n / total.transitions)
}


#' Function to get the distribution over each player's *previous two* transitions (+/-/0, +/-/0)
get.player.prev.2transition.dist = function(data) {
  sep = "_"
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2),
           prev.move3 = lag(player_move, 3)) %>%
    filter(!is.na(prev.move), !is.na(prev.move2), !is.na(prev.move2), # lag call above sets NA for lag on first three moves: ignore it here
           prev.move3 != "none", prev.move2 != "none", prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-"),
           player.prev.2transition = case_when(prev.move3 == prev.move2 ~ "0",
                                              ((prev.move3 == "rock" & prev.move2 == "paper") |
                                                 (prev.move3 == "paper" & prev.move2 == "scissors") |
                                                 (prev.move3 == "scissors" & prev.move2 == "rock")) ~ "+",
                                              ((prev.move3 == "rock" & prev.move2 == "scissors") |
                                                 (prev.move3 == "paper" & prev.move2 == "rock") |
                                                 (prev.move3 == "scissors" & prev.move2 == "paper")) ~ "-"),
           player.prev.transition.prev.transition2 = paste(player.prev.transition, player.prev.2transition, sep = sep)) %>%
    count(player.prev.transition.prev.transition2) %>%
    mutate(total.transitions = sum(n),
           p.prev.2transition = n / total.transitions)
}


#' Function to get marginal probability of each outcome (win, loss, tie) for each participant
get.player.prev.outcome.dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.outcome = lag(player_outcome, 1)) %>%
    filter(!is.na(prev.outcome)) %>% # lag call above sets NA for lag on first outcome: ignore it here
    count(prev.outcome) %>%
    mutate(total.outcomes = sum(n),
           p.outcome = n / total.outcomes)
}


#' Function to get the distribution over each player's *opponent's previous* transition (+/-/0)
get.opponent.prev.transition.dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2)) %>%
    filter(!is.na(prev.move), !is.na(prev.move2), # lag call above sets NA for lag on first two moves: ignore it here
           prev.move2 != "none", prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-")) %>%
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.transition = ifelse(is.na(lag(player.prev.transition, 1)), lead(player.prev.transition, 1), lag(player.prev.transition, 1))) %>% # opponent's previous transition
    group_by(player_id) %>%
    count(opponent.prev.transition) %>%
    mutate(total.transitions = sum(n),
           p.opponent.prev.transition = n / total.transitions)
}


#' Function to get the distribution over each player's *opponent's previous two* transitions (+/-/0, +/-/0)
get.opponent.prev.2transition.dist = function(data) {
  sep = "_"
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2),
           prev.move3 = lag(player_move, 3)) %>%
    filter(!is.na(prev.move), !is.na(prev.move2), !is.na(prev.move3), # lag call above sets NA for lag on first three moves: ignore it here
           prev.move3 != "none", prev.move2 != "none", prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-"),
           player.prev.2transition = case_when(prev.move3 == prev.move2 ~ "0",
                                              ((prev.move3 == "rock" & prev.move2 == "paper") |
                                                 (prev.move3 == "paper" & prev.move2 == "scissors") |
                                                 (prev.move3 == "scissors" & prev.move2 == "rock")) ~ "+",
                                              ((prev.move3 == "rock" & prev.move2 == "scissors") |
                                                 (prev.move3 == "paper" & prev.move2 == "rock") |
                                                 (prev.move3 == "scissors" & prev.move2 == "paper")) ~ "-")) %>%
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.transition = ifelse(is.na(lag(player.prev.transition, 1)), lead(player.prev.transition, 1), lag(player.prev.transition, 1)), # opponent's previous transition
           opponent.prev.transition2 = ifelse(is.na(lag(player.prev.2transition, 1)), lead(player.prev.2transition, 1), lag(player.prev.2transition, 1)), # opponent's previous transition 2
           opponent.prev.transition.prev.transition2 = paste(opponent.prev.transition, opponent.prev.transition2, sep = sep)) %>% 
    group_by(player_id) %>%
    count(opponent.prev.transition.prev.transition2) %>%
    mutate(total.transitions = sum(n),
           p.opponent.prev.2transition = n / total.transitions)
}


#' Function to get the distribution over each player's *previous transition, outcome* combination,
#' e.g. "+_win" where the previous transition "+" lead to a "win" in the previous outcome
get.player.prev.transition.prev.outcome.dist = function(data) {
  sep = "_"
  data %>%
    group_by(player_id) %>%
    mutate(prev.outcome = lag(player_outcome, 1),
           prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2)) %>%
    filter(!is.na(prev.outcome), # lag call above sets NA for lag on first outcome: ignore it here
           !is.na(prev.move), !is.na(prev.move2), # lag call above sets NA for lag on first two moves: ignore it here
           prev.move2 != "none", prev.move != "none", player_move != "none") %>% 
    # TODO move to a model where we add all these cols once at the beginning then just summarize in each analysis
    mutate(player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-"),
           player.prev.transition.prev.outcome = paste(player.prev.transition, prev.outcome, sep = sep)) %>%
    count(player.prev.transition.prev.outcome) %>%
    mutate(total.transition.outcomes = sum(n),
           p.transition.outcome = n / total.transition.outcomes)
}

#' Function to get the distribution over each player's *previous outcome, opponent's previous transition*,
#' e.g. "+_win" where the opponent's previous transition "+" lead to a "win" for the participant
get.opponent.prev.transition.prev.outcome.dist = function(data) {
  sep = "_"
  data %>%
    group_by(player_id) %>%
    mutate(prev.outcome = lag(player_outcome, 1),
           prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2)) %>%
    filter(!is.na(prev.outcome), # lag call above sets NA for lag on first outcome: ignore it here
           !is.na(prev.move), !is.na(prev.move2), # lag call above sets NA for lag on first two moves: ignore it here
           prev.move2 != "none", prev.move != "none", player_move != "none") %>% 
    # TODO move to a model where we add all these cols once at the beginning then just summarize in each analysis
    mutate(player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-")) %>%
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.transition = ifelse(is.na(lag(player.prev.transition, 1)), lead(player.prev.transition, 1), lag(player.prev.transition, 1)), # opponent's previous transition
           opponent.prev.transition.player.prev.outcome = paste(opponent.prev.transition, prev.outcome, sep = sep)) %>% # combination of opponent's previous transition, player's previous outcome, e.g. "+_win"
    group_by(player_id) %>%
    count(opponent.prev.transition.player.prev.outcome) %>%
    mutate(total.opponent.transition.outcomes = sum(n),
           p.opponent.prev.transition.player.prev.outcome = n / total.opponent.transition.outcomes)
}


#' Function to get conditional distribution of each player's transition (+/-/0),
#' given their previous outcome (win, tie, loss)
get.player.transition.outcome.cond = function(data) {
  sep = "_"
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.outcome = lag(player_outcome, 1)) %>%
    filter(!is.na(prev.outcome), # lag call above sets NA for lag on first oucome: ignore it here
           !is.na(prev.move), # lag call above sets NA for lag on first move: ignore it here
           prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-"),
           player.outcome.transition = paste(prev.outcome, player.transition, sep = sep)) %>%
    count(player.outcome.transition) %>%
    group_by(player_id, player.outcome.transition) %>%
    mutate(prev.outcome = strsplit(player.outcome.transition, sep)[[1]][1], # add prev.outcome back in because we lose it in the count() call above
           player.transition = strsplit(player.outcome.transition, sep)[[1]][2]) %>% # add player.transition back in because we lose it in the count() call above
    group_by(player_id, prev.outcome) %>%
    mutate(row.totals = sum(n),
           # probability of this player transition, conditioned on previous outcome
           p.transition.outcome = n / row.totals)
}


#' Function to get conditional distribution of each player's transition (+/-/0), 
#' given their previous transition
get.player.transition.prev.transition.cond = function(data) {
  sep = "_"
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2)) %>%
    filter(!is.na(prev.move), !is.na(prev.move2), # lag call above sets NA for lag on first two moves: ignore it here
           prev.move2 != "none", prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-"),
           player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-"),
           # NB: the order of these is opposite of what's done in similar function above (mostly for naming reasons)
           player.transition.prev.transition = paste(player.transition, player.prev.transition, sep = sep)) %>%
    count(player.transition.prev.transition) %>%
    group_by(player_id, player.transition.prev.transition) %>%
    mutate(player.transition = strsplit(player.transition.prev.transition, sep)[[1]][1], # add transition back in because we lose it in the count() call above
           player.prev.transition = strsplit(player.transition.prev.transition, sep)[[1]][2]) %>% # add prev. transition back in because we lose it in the count() call above
    group_by(player_id, player.prev.transition) %>%
    mutate(row.totals = sum(n),
           # probability of this player transition, conditioned on previous transition
           p.transition.prev.transition = n / row.totals)
}


#' Function to get conditional distribution of each player's transition (+/-/0), 
#' given their *previous two* transitions
get.player.transition.prev.2transition.cond = function(data) {
  player.transition.prev.2transition.df = data.frame(player_id = character(), player.transition = character(), player.prev.transition = character(), player.prev.2transition = character(),
                                                                 n = numeric(), row.totals = numeric())
  # TODO can we do this without a nested loop....
  # Bayesian smoothing, put count of 1 in each combination before adding true counts
  for (player.trans in TRANSITION_SET) {
    for (prev.trans in TRANSITION_SET) {
      for (prev.trans2 in TRANSITION_SET) {
        player.transition.prev.2transition.df = rbind(player.transition.prev.2transition.df, 
                                                      data.frame(player_id = PLAYER_SET,
                                                                 player.transition = player.trans,
                                                                 player.prev.transition = prev.trans,
                                                                 player.prev.2transition = prev.trans2,
                                                                 n = 1, row.totals = length(TRANSITION_SET)))
        
      }
    }
  }
  sep = "_"
  tmp = data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2),
           prev.move3 = lag(player_move, 3)) %>%
    filter(!is.na(prev.move), !is.na(prev.move2), !is.na(prev.move3), # lag call above sets NA for lag on first three moves: ignore it here
           prev.move3 != "none", prev.move2 != "none", prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-"),
           player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-"),
           player.prev.2transition = case_when(prev.move3 == prev.move2 ~ "0",
                                              ((prev.move3 == "rock" & prev.move2 == "paper") |
                                                 (prev.move3 == "paper" & prev.move2 == "scissors") |
                                                 (prev.move3 == "scissors" & prev.move2 == "rock")) ~ "+",
                                              ((prev.move3 == "rock" & prev.move2 == "scissors") |
                                                 (prev.move3 == "paper" & prev.move2 == "rock") |
                                                 (prev.move3 == "scissors" & prev.move2 == "paper")) ~ "-"),
           # NB: the order of these is opposite of what's done in similar function above (mostly for naming reasons)
           player.transition.prev.2transition = paste(player.transition, player.prev.transition, player.prev.2transition, sep = sep)) %>%
    count(player.transition.prev.2transition) %>%
    group_by(player_id, player.transition.prev.2transition) %>%
    mutate(player.transition = strsplit(player.transition.prev.2transition, sep)[[1]][1], # add transition back in because we lose it in the count() call above
           player.prev.transition = strsplit(player.transition.prev.2transition, sep)[[1]][2], # add prev. transition back in because we lose it in the count() call above
           player.prev.2transition = strsplit(player.transition.prev.2transition, sep)[[1]][3]) %>% # add prev. transition 2 back in because we lose it in the count() call above
    group_by(player_id, player.prev.transition, player.prev.2transition) %>%
    mutate(row.totals = sum(n),
           # probability of this player transition, conditioned on previous transition
           p.transition.prev.2transition = n / row.totals)
  
  # return initial counts set to 1 in smoothing df plus counts calculated in tmp above
  left_join(player.transition.prev.2transition.df, tmp, by = c("player_id", "player.transition", "player.prev.transition", "player.prev.2transition")) %>%
    mutate(n.agg = ifelse(is.na(n.y), n.x, n.x + n.y),
           row.totals.agg = ifelse(is.na(row.totals.y), row.totals.x, row.totals.x + row.totals.y),
           p.transition.prev.2transition = n.agg / row.totals.agg) %>%
    select(player_id, player.transition, player.prev.transition, player.prev.2transition, 
           n.agg, row.totals.agg, p.transition.prev.2transition)
}


#' Function to get conditional distribution of each player's transition (+/-/0), 
#' given their *opponent's* previous transition
get.player.transition.opponent.prev.transition.cond = function(data) {
  sep = "_"
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2)) %>%
    filter(!is.na(prev.move), !is.na(prev.move2), # lag call above sets NA for lag on first two moves: ignore it here
           prev.move2 != "none", prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-"),
           player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-")) %>%
    group_by(game_id, round_index) %>%
    # opponent's previous transition is previous row's prev.transition for one of the players, next row's prev.transition for the other
    mutate(opponent.prev.transition = ifelse(is.na(lag(player.prev.transition, 1)), lead(player.prev.transition, 1), lag(player.prev.transition, 1)), # opponent's previous transition
           player.transition.opponent.prev.transition = paste(player.transition, opponent.prev.transition, sep = sep)) %>%
    group_by(player_id) %>%
    count(player.transition.opponent.prev.transition) %>%
    group_by(player_id, player.transition.opponent.prev.transition) %>%
    mutate(player.transition = strsplit(player.transition.opponent.prev.transition, sep)[[1]][1], # add transition back in because we lose it in the count() call above
           opponent.prev.transition = strsplit(player.transition.opponent.prev.transition, sep)[[1]][2]) %>% # add opponent prev. transition back in because we lose it in the count() call above
    group_by(player_id, opponent.prev.transition) %>%
    mutate(row.totals = sum(n),
           # probability of this player transition, conditioned on previous transition
           p.transition.opponent.prev.transition = n / row.totals)
}


#' Function to get conditional distribution of each player's transition (+/-/0), 
#' given their *opponent's* previous *two transitions*
get.player.transition.opponent.prev.2transition.cond = function(data) {
  player.transition.opponent.prev.2transition.df = data.frame(player_id = character(), player.transition = character(), opponent.prev.transition = character(), opponent.prev.2transition = character(),
                                                              n = numeric(), row.totals = numeric())
  # TODO can we do this without a nested loop....
  # Bayesian smoothing, put count of 1 in each combination before adding true counts
  for (player.trans in TRANSITION_SET) {
    for (opponent.prev.trans in TRANSITION_SET) {
      for (opponent.prev.trans2 in TRANSITION_SET) {
        player.transition.opponent.prev.2transition.df = rbind(player.transition.opponent.prev.2transition.df, 
                                                               data.frame(player_id = PLAYER_SET,
                                                                          player.transition = player.trans,
                                                                          opponent.prev.transition = opponent.prev.trans,
                                                                          opponent.prev.2transition = opponent.prev.trans2,
                                                                          n = 1, row.totals = length(TRANSITION_SET)))
        
      }
    }
  }
  sep = "_"
  tmp = data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2),
           prev.move3 = lag(player_move, 3)) %>%
    filter(!is.na(prev.move), !is.na(prev.move2), !is.na(prev.move3), # lag call above sets NA for lag on first three moves: ignore it here
           prev.move3 != "none", prev.move2 != "none", prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-"),
           player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-"),
           player.prev.2transition = case_when(prev.move3 == prev.move2 ~ "0",
                                               ((prev.move3 == "rock" & prev.move2 == "paper") |
                                                  (prev.move3 == "paper" & prev.move2 == "scissors") |
                                                  (prev.move3 == "scissors" & prev.move2 == "rock")) ~ "+",
                                               ((prev.move3 == "rock" & prev.move2 == "scissors") |
                                                  (prev.move3 == "paper" & prev.move2 == "rock") |
                                                  (prev.move3 == "scissors" & prev.move2 == "paper")) ~ "-")) %>%
    group_by(game_id, round_index) %>%
    # opponent's previous transition is previous row's prev.transition for one of the players, next row's prev.transition for the other
    mutate(opponent.prev.transition = ifelse(is.na(lag(player.prev.transition, 1)), lead(player.prev.transition, 1), lag(player.prev.transition, 1)), # opponent's previous transition
           opponent.prev.2transition = ifelse(is.na(lag(player.prev.2transition, 1)), lead(player.prev.2transition, 1), lag(player.prev.2transition, 1)), # opponent's previous transition 2
           player.transition.opponent.prev.2transition = paste(player.transition, opponent.prev.transition, opponent.prev.2transition, sep = sep)) %>%
    group_by(player_id) %>%
    count(player.transition.opponent.prev.2transition) %>%
    group_by(player_id, player.transition.opponent.prev.2transition) %>%
    mutate(player.transition = strsplit(player.transition.opponent.prev.2transition, sep)[[1]][1], # add transition back in because we lose it in the count() call above
           opponent.prev.transition = strsplit(player.transition.opponent.prev.2transition, sep)[[1]][2], # add opponent prev. transition back in because we lose it in the count() call above
           opponent.prev.2transition = strsplit(player.transition.opponent.prev.2transition, sep)[[1]][3]) %>% # add opponent prev. transition 2 back in because we lose it in the count() call above
    group_by(player_id, opponent.prev.transition, opponent.prev.2transition) %>%
    mutate(row.totals = sum(n),
           # probability of this player transition, conditioned on previous transition
           p.transition.opponent.prev.2transition = n / row.totals)
    
  # return initial counts set to 1 in smoothing df plus counts calculated in tmp above
  left_join(player.transition.opponent.prev.2transition.df, tmp, by = c("player_id", "player.transition", "opponent.prev.transition", "opponent.prev.2transition")) %>%
    mutate(n.agg = ifelse(is.na(n.y), n.x, n.x + n.y),
           row.totals.agg = ifelse(is.na(row.totals.y), row.totals.x, row.totals.x + row.totals.y),
           p.transition.opponent.prev.2transition = n.agg / row.totals.agg) %>%
    select(player_id, player.transition, opponent.prev.transition, opponent.prev.2transition, 
           n.agg, row.totals.agg, p.transition.opponent.prev.2transition)
}


#' Function to get conditional distribution of each player's transition (+/-/0),
#' given the combination of *their previous transition and their previous outcome*
get.player.transition.prev.transition.prev.outcome.cond = function(data) {
  player.transition.prev.transition.prev.outcome.df = data.frame(player_id = character(), player.transition = character(), player.prev.transition = character(), prev.outcome = character(),
                                                                 n = numeric(), row.totals = numeric())
  # TODO can we do this without a nested loop....
  # Bayesian smoothing, put count of 1 in each combination before adding true counts
  for (player.trans in TRANSITION_SET) {
    for (prev.trans in TRANSITION_SET) {
      for (prev.outcome in OUTCOME_SET) {
        player.transition.prev.transition.prev.outcome.df = rbind(player.transition.prev.transition.prev.outcome.df, 
                                                                  data.frame(player_id = PLAYER_SET,
                                                                             player.transition = player.trans,
                                                                             player.prev.transition = prev.trans,
                                                                             prev.outcome = prev.outcome,
                                                                             n = 1, row.totals = length(TRANSITION_SET)))
        
      }
    }
  }
  sep = "_"
  tmp = data %>%
    group_by(player_id) %>%
    mutate(prev.outcome = lag(player_outcome, 1),
           prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2)) %>%
    filter(!is.na(prev.outcome), # lag call above sets NA for lag on first outcome: ignore it here
           !is.na(prev.move), !is.na(prev.move2), # lag call above sets NA for lag on first two moves: ignore it here
           prev.move2 != "none", prev.move != "none", player_move != "none") %>% 
    # TODO move to a model where we add all these cols once at the beginning then just summarize in each analysis
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-"),
           player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-"),
           player.transition.prev.transition.prev.outcome = paste(player.transition, player.prev.transition, prev.outcome, sep = sep)) %>%
    count(player.transition.prev.transition.prev.outcome) %>%
    group_by(player_id, player.transition.prev.transition.prev.outcome) %>%
    mutate(player.transition = strsplit(player.transition.prev.transition.prev.outcome, sep)[[1]][1], # add transition back in because we lose it in the count() call above
           player.prev.transition = strsplit(player.transition.prev.transition.prev.outcome, sep)[[1]][2], # add prev. transition back in because we lose it in the count() call above
           prev.outcome = strsplit(player.transition.prev.transition.prev.outcome, sep)[[1]][3]) %>% # add prev. outcome back in because we lose it in the count() call above
    group_by(player_id, player.prev.transition, prev.outcome) %>%
    mutate(row.totals = sum(n))
  
  # return initial counts set to 1 in smoothing df plus counts calculated in tmp above
  left_join(player.transition.prev.transition.prev.outcome.df, tmp, by = c("player_id", "player.transition", "player.prev.transition", "prev.outcome")) %>%
    mutate(n.agg = ifelse(is.na(n.y), n.x, n.x + n.y),
           row.totals.agg = ifelse(is.na(row.totals.y), row.totals.x, row.totals.x + row.totals.y),
           p.transition.prev.transition.prev.outcome = n.agg / row.totals.agg) %>%
    select(player_id, player.transition, player.prev.transition, prev.outcome, 
           n.agg, row.totals.agg, p.transition.prev.transition.prev.outcome)
}


#' Function to get conditional distribution of each player's transition (+/-/0),
#' given the combination of *their opponent's previous transition and the player's previous outcome*
get.player.transition.opponent.prev.transition.prev.outcome.cond = function(data) {
  player.transition.opponent.prev.transition.prev.outcome.df = data.frame(player_id = character(), player.transition = character(), opponent.prev.transition = character(), prev.outcome = character(),
                                                                          n = numeric(), row.totals = numeric())
  # TODO can we do this without a nested loop....
  # Bayesian smoothing, put count of 1 in each combination before adding true counts
  for (player.trans in TRANSITION_SET) {
    for (opponent.prev.trans in TRANSITION_SET) {
      for (prev.outcome in OUTCOME_SET) {
        player.transition.opponent.prev.transition.prev.outcome.df = rbind(player.transition.opponent.prev.transition.prev.outcome.df, 
                                                                           data.frame(player_id = PLAYER_SET,
                                                                                      player.transition = player.trans,
                                                                                      opponent.prev.transition = opponent.prev.trans,
                                                                                      prev.outcome = prev.outcome,
                                                                                      n = 1, row.totals = length(TRANSITION_SET)))
        
      }
    }
  }
  sep = "_"
  tmp = data %>%
    group_by(player_id) %>%
    mutate(prev.outcome = lag(player_outcome, 1),
           prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2)) %>%
    filter(!is.na(prev.outcome), # lag call above sets NA for lag on first outcome: ignore it here
           !is.na(prev.move), !is.na(prev.move2), # lag call above sets NA for lag on first two moves: ignore it here
           prev.move2 != "none", prev.move != "none", player_move != "none") %>% 
    # TODO move to a model where we add all these cols once at the beginning then just summarize in each analysis
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-"),
           player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-")) %>%
    group_by(game_id, round_index) %>%
    # opponent's previous transition is previous row's prev.transition for one of the players, next row's prev.transition for the other
    mutate(opponent.prev.transition = ifelse(is.na(lag(player.prev.transition, 1)), lead(player.prev.transition, 1), lag(player.prev.transition, 1)), # opponent's previous transition
           player.transition.opponent.prev.transition.prev.outcome = paste(player.transition, opponent.prev.transition, prev.outcome, sep = sep)) %>%
    group_by(player_id) %>%
    count(player.transition.opponent.prev.transition.prev.outcome) %>% 
    group_by(player_id, player.transition.opponent.prev.transition.prev.outcome) %>%
    mutate(player.transition = strsplit(player.transition.opponent.prev.transition.prev.outcome, sep)[[1]][1], # add transition back in because we lose it in the count() call above
           opponent.prev.transition = strsplit(player.transition.opponent.prev.transition.prev.outcome, sep)[[1]][2], # add opponent prev. transition back in because we lose it in the count() call above
           prev.outcome = strsplit(player.transition.opponent.prev.transition.prev.outcome, sep)[[1]][3]) %>% # add player previous outcome back in because we lose it in the count() call above
    group_by(player_id, opponent.prev.transition, prev.outcome) %>%
    mutate(row.totals = sum(n),
           # probability of this player transition, conditioned on opponent's previous transition and player's previous outcome
           p.transition.opponent.prev.transition.player.prev.outcome = n / row.totals)
  
  # return initial counts set to 1 in smoothing df plus counts calculated in tmp above
  left_join(player.transition.opponent.prev.transition.prev.outcome.df, tmp, by = c("player_id", "player.transition", "opponent.prev.transition", "prev.outcome")) %>%
    mutate(n.agg = ifelse(is.na(n.y), n.x, n.x + n.y),
           row.totals.agg = ifelse(is.na(row.totals.y), row.totals.x, row.totals.x + row.totals.y),
           p.transition.opponent.prev.transition.prev.outcome = n.agg / row.totals.agg) %>%
    select(player_id, player.transition, opponent.prev.transition, prev.outcome, 
           n.agg, row.totals.agg, p.transition.opponent.prev.transition.prev.outcome)
}



################
### ANALYSIS ###
################


### First order analysis: p(transition = x) -> 3 cells ###
# get overall probability of each transition (for each player)
player.transition.summary = get.player.transition.dist(data)

# calculate entropy over the distribution of each move (for each player)
player.transition.entropy = player.transition.summary %>%
  group_by(player_id) %>%
  summarize(transition.entropy.0 = -sum(p.transition * log2(p.transition))) # get entropy for distribution of each transition



### Second order analysis 1: p(transition = x | prev. outcome = y) -> 9 cells ###
# get marginal probability of each participant's previous outcome
player.prev.outcome.prob = get.player.prev.outcome.dist(data)

# get probability of each transition, given previous outcome
player.transition.prev.outcome = get.player.transition.outcome.cond(data) 


# calculate entropy based on probability above, normalizing by marginal probability of each previous outcome
player.transition.outcome.entropy = player.transition.prev.outcome %>%
  group_by(player_id, prev.outcome) %>%
  # get entropy for distribution of each transition, for each prev. outcome
  summarize(prev.outcome.entropy = -sum(p.transition.outcome * log2(p.transition.outcome))) %>%
  inner_join(player.prev.outcome.prob, by = "player_id") %>%
  # normalize entropy of each transition given each prev. outcome by the probability of that prev. outcome
  mutate(prev.outcome.entropy.norm = prev.outcome.entropy * p.outcome) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above
  filter(prev.outcome.x == prev.outcome.y) %>%
  # sum normalized entropy for distribution of each transition given each prev. outcome
  summarize(transition.entropy.1.player.outcome = sum(prev.outcome.entropy.norm))



### Second order analysis 2: p(transition = x | prev. transition = y) -> 9 cells ###

# get marginal probability of each participant's previous transition
player.prev.transition.prob = get.player.prev.transition.dist(data)

# get probability of each transition, given previous transition
player.transition.prev.transition = get.player.transition.prev.transition.cond(data) 

# calculate entropy based on probability above, normalizing by marginal probability of each previous transition
player.transition.prev.transition.entropy = player.transition.prev.transition %>%
  group_by(player_id, player.prev.transition) %>%
  # get entropy for distribution of each transition, for each prev. outcome
  summarize(prev.transition.entropy = -sum(p.transition.prev.transition * log2(p.transition.prev.transition))) %>%
  inner_join(player.prev.transition.prob, by = "player_id") %>%
  # normalize entropy of each transition given each prev. outcome by the probability of that prev. outcome
  mutate(prev.transition.entropy.norm = prev.transition.entropy * p.prev.transition) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above
  filter(player.prev.transition.x == player.prev.transition.y) %>%
  # sum normalized entropy for distribution of each transition given each prev. outcome
  summarize(transition.entropy.1.player.prev.transition = sum(prev.transition.entropy.norm))


### Second order analysis 3: p(transition = x | opponent prev. transition = y) -> 9 cells ###

# get marginal probability of each participant's *opponent's* previous transition
opponent.prev.transition.prob = get.opponent.prev.transition.dist(data)

# get probability of each transition, given opponent's previous transition
player.transition.opponent.prev.transition = get.player.transition.opponent.prev.transition.cond(data) 

# calculate entropy based on probability above, normalizing by marginal probability of each opponent's previous transition
player.transition.opponent.prev.transition.entropy = player.transition.opponent.prev.transition %>%
  group_by(player_id, opponent.prev.transition) %>%
  # get entropy for distribution of each transition, for each prev. outcome
  summarize(opponent.prev.transition.entropy = -sum(p.transition.opponent.prev.transition * log2(p.transition.opponent.prev.transition))) %>%
  inner_join(opponent.prev.transition.prob, by = "player_id") %>%
  # normalize entropy of each transition given each prev. outcome by the probability of that prev. outcome
  mutate(opponent.prev.transition.entropy.norm = opponent.prev.transition.entropy * p.opponent.prev.transition) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above
  filter(opponent.prev.transition.x == opponent.prev.transition.y) %>%
  # sum normalized entropy for distribution of each transition given each prev. outcome
  summarize(transition.entropy.1.opponent.prev.transition = sum(opponent.prev.transition.entropy.norm))


### Third order analysis 1: p(transition = x | prev. transition = y, prev. outcome = z) -> 27 cells ###

# get marginal probability of each participant's previous transition and previous outcome
player.prev.transition.prev.outcome.prob = get.player.prev.transition.prev.outcome.dist(data)

# get probability of each transition, given participant's previous transition and previous outcome
player.transition.prev.transition.prev.outcome = get.player.transition.prev.transition.prev.outcome.cond(data) 

# calculate entropy based on probability above, normalizing by marginal probability of each player's previous transition, outcome combination
sep = "_"
player.transition.prev.transition.prev.outcome.entropy = player.transition.prev.transition.prev.outcome %>%
  group_by(player_id, player.prev.transition, prev.outcome) %>%
  # get entropy for distribution of each transition, for each prev. transition, outcome combination
  summarize(player.prev.transition.prev.outcome.entropy = -sum(p.transition.prev.transition.prev.outcome * log2(p.transition.prev.transition.prev.outcome))) %>%
  # add column for coalescing previous transition, previous outcome
  mutate(player.prev.transition.prev.outcome = paste(player.prev.transition, prev.outcome, sep = sep)) %>%
  inner_join(player.prev.transition.prev.outcome.prob, by = "player_id") %>%
  # normalize entropy of each transition given each prev. transition, prev. outcome by the probability of that prev. transition, prev. outcome
  mutate(player.prev.transition.prev.outcome.entropy.norm = player.prev.transition.prev.outcome.entropy * p.transition.outcome) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above
  filter(player.prev.transition.prev.outcome.x == player.prev.transition.prev.outcome.y) %>%
  # sum normalized entropy for distribution of each transition given each prev. outcome
  summarize(transition.entropy.2.player.prev.transition.prev.outcome = sum(player.prev.transition.prev.outcome.entropy.norm))



### Third order analysis 2: p(transition = x | opponent prev. transition = y, prev. outcome = z) -> 27 cells ###

# get marginal probability of each participant's previous outcome and *opponent's* previous transition
opponent.prev.transition.prev.outcome.prob = get.opponent.prev.transition.prev.outcome.dist(data)

# get probability of each transition, given previous outcome and *opponent's* previous transition
player.transition.opponent.prev.transition.prev.outcome = get.player.transition.opponent.prev.transition.prev.outcome.cond(data) 

# calculate entropy based on probability above, normalizing by marginal probability of each player's opponent's previous transition, player's outcome combination
sep = "_"
player.transition.opponent.prev.transition.prev.outcome.entropy = player.transition.opponent.prev.transition.prev.outcome %>%
  group_by(player_id, opponent.prev.transition, prev.outcome) %>%
  # get entropy for distribution of each transition, for each opponent prev. transition, outcome combination
  summarize(opponent.prev.transition.prev.outcome.entropy = -sum(p.transition.opponent.prev.transition.prev.outcome * log2(p.transition.opponent.prev.transition.prev.outcome))) %>%
  # add column for coalescing previous transition, previous outcome
  mutate(opponent.prev.transition.player.prev.outcome = paste(opponent.prev.transition, prev.outcome, sep = sep)) %>%
  inner_join(opponent.prev.transition.prev.outcome.prob, by = "player_id") %>%
  # normalize entropy of each transition given each prev. transition, prev. outcome by the probability of that prev. transition, prev. outcome
  mutate(opponent.prev.transition.prev.outcome.entropy.norm = opponent.prev.transition.prev.outcome.entropy * p.opponent.prev.transition.player.prev.outcome) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above
  filter(opponent.prev.transition.player.prev.outcome.x == opponent.prev.transition.player.prev.outcome.y) %>%
  # sum normalized entropy for distribution of each transition given each prev. outcome
  summarize(transition.entropy.2.opponent.prev.transition.prev.outcome = sum(opponent.prev.transition.prev.outcome.entropy.norm))



### Third order analysis 3: p(transition = x | prev. transition = y, prev. transition 2 = z) -> 27 cells ###

# get marginal probability of each participant's previous two transitions
player.prev.2transition.prob = get.player.prev.2transition.dist(data)

# get probability of each transition, given previous two transitions
player.prev.2transition = get.player.transition.prev.2transition.cond(data) 

# calculate entropy based on probability above, normalizing by marginal probability of each player's previous two transitions
sep = "_"
player.transition.prev.2transition.entropy = player.prev.2transition %>%
  group_by(player_id, player.prev.transition, player.prev.2transition) %>%
  # get entropy for distribution of each transition, for each opponent prev. two transition combination
  summarize(player.prev.2transition.entropy = -sum(p.transition.prev.2transition * log2(p.transition.prev.2transition))) %>%
  # add column for coalescing previous two transitions
  mutate(player.prev.transition.prev.transition2 = paste(player.prev.transition, player.prev.2transition, sep = sep)) %>%
  inner_join(player.prev.2transition.prob, by = "player_id") %>%
  # normalize entropy of each transition given each prev. transition, prev. outcome by the probability of that prev. transition, prev. outcome
  mutate(player.prev.2transition.entropy.norm = player.prev.2transition.entropy * p.prev.2transition) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above
  filter(player.prev.transition.prev.transition2.x == player.prev.transition.prev.transition2.y) %>%
  # sum normalized entropy for distribution of each transition given each prev. outcome
  summarize(transition.entropy.2.player.prev.2transition = sum(player.prev.2transition.entropy.norm))



### Third order analysis 4: p(transition = x | opponent prev. transition = y, opponent prev. transition 2 = z) -> 27 cells ###

# get marginal probability of each participant's *opponent's* previous two transitions
opponent.prev.2transition.prob = get.opponent.prev.2transition.dist(data)

# get probability of each transition, given opponent's previous two transitions
opponent.prev.2transition = get.player.transition.opponent.prev.2transition.cond(data)

# calculate entropy based on probability above, normalizing by marginal probability of each player's *opponent's* previous two transitions
sep = "_"
player.transition.opponent.prev.2transition.entropy = opponent.prev.2transition %>%
  group_by(player_id, opponent.prev.transition, opponent.prev.2transition) %>%
  # get entropy for distribution of each transition, for each opponent prev. two transition combination
  summarize(opponent.prev.2transition.entropy = -sum(p.transition.opponent.prev.2transition * log2(p.transition.opponent.prev.2transition))) %>%
  # add column for coalescing previous two transitions
  mutate(opponent.prev.transition.prev.transition2 = paste(opponent.prev.transition, opponent.prev.2transition, sep = sep)) %>%
  inner_join(opponent.prev.2transition.prob, by = "player_id") %>%
  # normalize entropy of each transition given each prev. transition, prev. outcome by the probability of that prev. transition, prev. outcome
  mutate(opponent.prev.2transition.entropy.norm = opponent.prev.2transition.entropy * p.opponent.prev.2transition) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above
  filter(opponent.prev.transition.prev.transition2.x == opponent.prev.transition.prev.transition2.y) %>%
  # sum normalized entropy for distribution of each transition given each prev. outcome
  summarize(transition.entropy.2.opponent.prev.2transition = sum(opponent.prev.2transition.entropy.norm))



###############
### SUMMARY ###
###############


ENTROPY_SUMMARY = player.transition.entropy %>%
  left_join(player.transition.outcome.entropy, by = "player_id") %>%
  left_join(player.transition.prev.transition.entropy, by = "player_id") %>%
  left_join(player.transition.opponent.prev.transition.entropy, by = "player_id") %>%
  left_join(player.transition.prev.transition.prev.outcome.entropy, by = "player_id") %>%
  left_join(player.transition.opponent.prev.transition.prev.outcome.entropy, by = "player_id") %>%
  left_join(player.transition.prev.2transition.entropy, by = "player_id") %>%
  left_join(player.transition.opponent.prev.2transition.entropy, by = "player_id") %>%
  gather(entropy.type, entropy.val, transition.entropy.0:transition.entropy.2.opponent.prev.2transition)


graph.labels = c("transition.entropy.0" = "Distribution of transitions (+/-/0)",
                 "transition.entropy.1.player.outcome" = "Distribution of transitions given player's previous outcome (win/tie/loss)",
                 "transition.entropy.1.player.prev.transition" = "Distribution of transitions given player's previous transition",
                 "transition.entropy.1.opponent.prev.transition" = "Distribution of transitions given opponent's previous transition",
                 "transition.entropy.2.player.prev.transition.prev.outcome" = "Distribution of transitions given player's previous transition and previous outcome",
                 "transition.entropy.2.opponent.prev.transition.prev.outcome" = "Distribution of transitions given opponent's previous transition, player's previous outcome",
                 "transition.entropy.2.player.prev.2transition" = "Distribution of transitions given player's previous two transitions",
                 "transition.entropy.2.opponent.prev.2transition" = "Distribution of transitions given opponent's previous two transitions")

ENTROPY_SUMMARY %>%
  ggplot() +
  geom_violin(aes(x = entropy.type, y = entropy.val, color = entropy.type)) +
  geom_jitter(aes(x = entropy.type, y = entropy.val, color = entropy.type), width = 0.25, alpha = 0.5, size = 2) +
  labs(x = "Participant transition distributions", y = "Shannon Entropy (S)") +
  #ggtitle("Entropy values across participant transition distributions") +
  scale_color_viridis(discrete = TRUE,
                      name = element_blank(),
                      labels = graph.labels,
                      guide = guide_legend(nrow = length(unique(ENTROPY_SUMMARY$entropy.type)))) +
  individ_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.text.x = element_blank())


ENTROPY_SUMMARY %>%
  group_by(entropy.type) %>%
  summarize(mean.entropy = mean(entropy.val),
            se.entropy = sd(entropy.val) / sqrt(length(unique(player_id)))) %>%
  ggplot(aes(x = entropy.type, color = entropy.type)) +
  geom_point(aes(y = mean.entropy), size = 3) +
  geom_errorbar(aes(ymin = mean.entropy - se.entropy, ymax = mean.entropy + se.entropy),
                width = 0.5) +
  labs(x = "Participant move distributions", y = "Shannon Entropy (S)") +
  #ggtitle("Entropy summary across participant move distributions") +
  scale_color_viridis(discrete = TRUE,
                      name = element_blank(),
                      labels = graph.labels,
                      guide = guide_legend(nrow = length(unique(ENTROPY_SUMMARY$entropy.type)))) +
  individ_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 20)) +
  theme(axis.text.x = element_blank())




### Graph for data blitz and other display purposes (keeps only relevant analyses from those above)

ENTROPY_SUMMARY_FINAL = player.entropy %>%
  # move distributions
  left_join(player.prev.move.entropy, by = "player_id") %>%
  left_join(opponent.prev.move.entropy, by = "player_id") %>%
  left_join(player.prev.2move.entropy, by = "player_id") %>%
  # transition distributions
  left_join(player.transition.entropy, by = "player_id") %>%
  left_join(player.transition.outcome.entropy, by = "player_id") %>%
  gather(entropy.type, entropy.val, entropy.0:transition.entropy.1.player.outcome)


legend.width = 45
graph.labels.final = c("entropy.0" = str_wrap("Overall distribution of moves ('rock', 'paper', 'scissors')", legend.width), 
                       "entropy.1.player" = str_wrap("Distribution of moves given player's previous move", legend.width),
                       "entropy.1.opponent" = str_wrap("Distribution of moves given opponent's previous move", legend.width),
                       "entropy.2.player" = str_wrap("Distribution of moves given player's previous two moves", legend.width),
                       "transition.entropy.0" = str_wrap("Distribution of transitions ('+', '-', '0')", legend.width),
                       "transition.entropy.1.player.outcome" = str_wrap("Distribution of transitions given player's previous outcome ('win', 'tie', 'loss')", legend.width))

xlab.final = ""
ylab.final = "Shannon Entropy (S)"

ENTROPY_SUMMARY_FINAL %>%
  group_by(entropy.type) %>%
  summarize(mean.entropy = mean(entropy.val),
            se.entropy = sd(entropy.val) / sqrt(length(unique(player_id)))) %>%
  ggplot(aes(x = fct_reorder(entropy.type, mean.entropy, .desc = TRUE), 
             color = fct_reorder(entropy.type, mean.entropy, .desc = TRUE))) +
  geom_point(aes(y = mean.entropy), size = 5) +
  geom_errorbar(aes(ymin = mean.entropy - se.entropy, ymax = mean.entropy + se.entropy),
                width = 0.25) +
  labs(x = xlab.final, y = ylab.final) +
  scale_color_viridis(discrete = TRUE,
                      name = element_blank(),
                      labels = graph.labels.final,
                      guide = guide_legend(nrow = length(unique(ENTROPY_SUMMARY_FINAL$entropy.type)))) +
  #scale_y_continuous(limits = c(1.4, 1.6)) +
  individ_plot_theme +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20, face = "bold", angle = 45),
        axis.title.y = element_text(size = 28, face = "bold"),
        # legend.position = "none",
        legend.position = "right",
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.key = element_rect(fill = NA))




