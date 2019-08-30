#'
#' This is the data analysis script for RPS data
#' This script should *not* be used for general data overview, cleaning etc.
#' Do that in `data_processing.R` and use this script for more complex analyses.


# TODO make this a summary RMD once we have all data

rm(list = ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")

source('data_processing.R') # script used for data processing/cleanup
glimpse(data)


##########################
### ANALYSIS FUNCTIONS ###
##########################


# Function to summarize probability of each move for each participant
get.move.probs = function(data) {
  data %>%
    group_by(player_id) %>%
    count(player_move) %>%
    mutate(total = sum(n),
           pmove = n / total)
}

# Function to summarize probability of each move for each participant, 
# conditioned on their previous move
get.prior.move.probs = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move)) %>% # lag call above sets NA for lag on very first move: ignore it here
    group_by(player_id, player_move, prev.move) %>%
    mutate(move_prev.move = paste0(player_move, "-", prev.move)) %>% # category of move given previous move, e.g. "rock-paper"
    group_by(player_id) %>%
    count(move_prev.move) %>%
    mutate(total.moves = sum(n),
           pmove_prev.move = n / total.moves) %>% # probability of this category of move, e.g. probability of "rock-paper"
    group_by(player_id, move_prev.move) %>%
    mutate(player_move = strsplit(move_prev.move, "-")[[1]][1], # add prev.move back in because we lose it in the count() call above
      prev.move = strsplit(move_prev.move, "-")[[1]][2]) # add prev.move back in because we lose it in the count() call above
}


################
### ANALYSIS ###
################


### First order analysis: p(move = x) ###
player.summary = get.move.probs(data)

player.entropy = player.summary %>%
  group_by(player_id) %>%
  summarize(overall.entropy = -sum(pmove * log10(pmove))) # get entropy for distribution of each move



### Second order analysis: p(move = x | prev. move = y) ###
player.prev.move.summary = get.prior.move.probs(data)

player.prev.move.entropy = player.prev.move.summary %>%
  group_by(player_id, prev.move) %>%
  summarize(prev.move.entropy = -sum(pmove_prev.move * log10(pmove_prev.move))) %>% # get entropy for distribution of each move, for each prev. move
  inner_join(player.summary, by = "player_id") %>%
  mutate(prev.move.entropy.norm = prev.move.entropy * pmove) %>% # normalize entropy of each move given each prev. move by the probability of that prev. move
  group_by(player_id, prev.move) %>%
  filter(prev.move == player_move) %>%
  group_by(player_id) %>%
  summarize(overall.entropy.pmove = sum(prev.move.entropy.norm)) # sum normalized entropy for distribution of each move given each prev. move
  

