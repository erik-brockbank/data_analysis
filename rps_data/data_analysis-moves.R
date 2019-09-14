#'
#' This is the first level data analysis script for RPS data,
#' looking at dependencies in participant *move distributions* {R, P, S}
#' based on previous combinations of participant and opponent moves (R, P, S).
#' 
#' This script should *not* be used for general data overview, cleaning etc. 
#' (that happens in `data_processing.R`) and should not be used for analyses that extend
#' beyond move distribution dependencies.
#' 


rm(list = ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")

source('data_processing.R') # script used for data processing/cleanup
glimpse(data)


PLAYER_SET = unique(data$player_id)
MOVE_SET = c("rock", "paper", "scissors")



##########################
### ANALYSIS FUNCTIONS ###
##########################


#' Function to get marginal probability of each move for each participant
get.player.move.dist = function(data) {
  data %>%
    filter(player_move != "none") %>% # ignore "none" moves for this aggregation
    group_by(player_id) %>%
    count(player_move) %>%
    mutate(total = sum(n),
           pmove = n / total)
}


#' Function to get marginal probability of each previous move for each participant
get.player.prev.move.dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move), # lag call above sets NA for lag on first move: ignore it here
           prev.move != "none") %>% # ignore "none" moves for this aggregation
    count(prev.move) %>%
    mutate(total.moves = sum(n),
           p.prev.move = n / total.moves)
}


#' Function to get marginal probability of each previous move for each participant's *opponent*
get.opponent.prev.move.dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move)) %>% # lag call above sets NA for lag on first move: ignore it here
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1))) %>% # opponent's one move back (previous move)
    filter(opponent.prev.move != "none") %>% # ignore "none" moves for this aggregation
    group_by(player_id) %>%
    count(opponent.prev.move) %>%
    mutate(total.moves = sum(n),
           p.opponent.prev.move = n / total.moves)
}


#' Function to get marginal probability of each set of previous two move combinations for each participant
get.player.2move.dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1), # one move back (previous move)
           prev.move2 = lag(player_move, 2), # two moves back
           prev.2moves = paste(prev.move, prev.move2, sep = "-")) %>% # category of previous two moves, e.g. "rock-paper"
    filter(!is.na(prev.move), !is.na(prev.move2), # lag calls above set NA for lag on first move and second moves: ignore it here
           prev.move != "none", prev.move2 != "none") %>% # ignore "none" moves for this aggregation
    count(prev.2moves) %>%
    mutate(total.moves = sum(n),
           p.2prev.moves = n / total.moves)
}


#' Function to get marginal probability of each set of previous two moves for each participant's *opponent*
get.opponent.2move.dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1), # one move back (previous move)
           prev.move2 = lag(player_move, 2)) %>% # two moves back
    filter(!is.na(prev.move), !is.na(prev.move2)) %>% # lag call above sets NA for lag on first two moves: ignore it here
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1)), # opponent's one move back (previous move)
           opponent.prev.move2 = ifelse(is.na(lag(player_move, 1)), lead(prev.move2, 1), lag(prev.move2, 1))) %>% # opponent's two moves back
    group_by(player_id) %>%
    filter(opponent.prev.move != "none", opponent.prev.move2 != "none") %>% # ignore "none" moves for this aggregation
    mutate(prev.opponent.2moves = paste(opponent.prev.move, opponent.prev.move2, sep = "-")) %>% # category of previous two moves, e.g. "rock-paper"
    count(prev.opponent.2moves) %>%
    mutate(total.moves = sum(n),
           p.2prev.moves = n / total.moves)
}


#' Function to get marginal probability of each combination of a player's previous move *and* their opponent's previous move
get.player.opponent.prev.move.dist = function(data) {
  data %>%
    # add each player's previous move, then use that when referencing opponent's previous move
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move)) %>% # lag call above sets NA for lag on very first move: ignore it here
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1)),
           # add category of move given previous move, opponent previous move (e.g. "rock-scissors-scissors")
           prev.move_opponent.prev.move = paste(prev.move, opponent.prev.move, sep = "-")) %>%
    filter(prev.move != "none", opponent.prev.move != "none") %>% # ignore "none" moves for this purpose
    group_by(player_id) %>%
    count(prev.move_opponent.prev.move) %>%
    mutate(total.moves = sum(n),
           p.prev.move_opponent.prev.move = n / total.moves)
}


#' Function to summarize probability of each move for each participant, 
#' conditioned on their *own* previous move
get.player.prev.move.cond.probs = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           # category of move given previous move, e.g. "rock-paper"
           move_prev.move = paste(player_move, prev.move, sep = "-")) %>%
    filter(!is.na(prev.move), # lag call above sets NA for lag on very first move: ignore it here
           player_move != "none", prev.move != "none") %>% # ignore "none" moves for this aggregation
    count(move_prev.move) %>%
    group_by(player_id, move_prev.move) %>%
    mutate(player_move = strsplit(move_prev.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           prev.move = strsplit(move_prev.move, "-")[[1]][2]) %>% # add prev.move back in because we lose it in the count() call above
    group_by(player_id, prev.move) %>%
    mutate(row.totals = sum(n),
           # probability of this player move, conditioned on previous move
           pmove_prev.move = n / row.totals)
}


#' Function to summarize probability of each move for each participant,
#' conditioned on their *own* previous *two* moves.
get.player.prev.2move.cond.probs = function(data) {
  prev.2move.df = data.frame(player_id = character(), player_move = character(), prev.move = character(), prev.move2 = character(),
                             n = numeric(), row.totals = numeric())
  # TODO can we do this without a nested loop....
  # Bayesian smoothing, put count of 1 in each combination before adding true counts
  for (player.move in MOVE_SET) {
    for (prev.move in MOVE_SET) {
      for (prev.move2 in MOVE_SET) {
        prev.2move.df = rbind(prev.2move.df, data.frame(player_id = PLAYER_SET,
                                                        player_move = player.move,
                                                        prev.move = prev.move,
                                                        prev.move2 = prev.move2,
                                                        n = 1, row.totals = length(MOVE_SET)))
        
      }
    }
  }
  tmp = data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1), # one move back (previous move)
           prev.move2 = lag(player_move, 2), # two moves back
           move_2prev.move = paste(player_move, prev.move, prev.move2, sep = "-")) %>% # category of move given previous two moves, e.g. "rock-paper-rock"
    filter(!is.na(prev.move), !is.na(prev.move2), # lag calls above set NA for lag on first move and second moves: ignore it here
           player_move != "none", prev.move != "none", prev.move2 != "none") %>% # ignore "none" moves for this aggregation
    count(move_2prev.move) %>%
    group_by(player_id, move_2prev.move) %>%
    mutate(player_move = strsplit(move_2prev.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           prev.move = strsplit(move_2prev.move, "-")[[1]][2], # add prev.move back in because we lose it in the count() call above
           prev.move2 = strsplit(move_2prev.move, "-")[[1]][3]) %>% # add prev.move2 back in because we lose it in the count() call above
    group_by(player_id, prev.move, prev.move2) %>%
    mutate(row.totals = sum(n))
  
  # return initial counts set to 1 in prev.2move.df plus counts calculated in tmp above
  left_join(prev.2move.df, tmp, by = c("player_id", "player_move", "prev.move", "prev.move2")) %>%
    mutate(n.agg = ifelse(is.na(n.y), n.x, n.x + n.y),
           row.totals.agg = ifelse(is.na(row.totals.y), row.totals.x, row.totals.x + row.totals.y),
           pmove_2prev.move = n.agg / row.totals.agg) %>%
    select(player_id, player_move, prev.move, prev.move2, 
           n.agg, row.totals.agg, pmove_2prev.move)
}


#' Function to summarize probability of each move for each participant,
#' conditioned on their *opponent's* previous move 
get.opponent.prev.move.cond.probs = function(data) {
  data %>%
    # add each player's previous move, then use that when referencing opponent's previous move
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move)) %>% # lag call above sets NA for lag on very first move: ignore it here
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1)),
           # category of move given opponent previous move, e.g. "rock-paper"
           move_opponent.prev.move = paste(player_move, opponent.prev.move, sep = "-")) %>%
    filter(player_move != "none", opponent.prev.move != "none") %>% # ignore "none" moves for this aggregation
    group_by(player_id) %>%
    count(move_opponent.prev.move) %>%
    group_by(player_id, move_opponent.prev.move) %>%
    mutate(player_move = strsplit(move_opponent.prev.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           opponent.prev.move = strsplit(move_opponent.prev.move, "-")[[1]][2]) %>% # add opponent.prev.move back in because we lose it in the count() call above
    group_by(player_id, opponent.prev.move) %>%
    mutate(row.totals = sum(n),
           # probability of this player move, conditioned on opponent previous move
           pmove_opponent.prev.move = n / row.totals)
}

#' Function to summarize probability of each move for each participant,
#' conditioned on their *opponent's* previous *two* moves
get.opponent.prev.2move.cond.probs = function(data) {
  prev.2move.df = data.frame(player_id = character(), player_move = character(), opponent.prev.move = character(), opponent.prev.move2 = character(),
                            n = numeric(), row.totals = numeric())
  # TODO can we do this without a nested loop....
  # Bayesian smoothing, put count of 1 in each combination before adding true counts
  for (player.move in MOVE_SET) {
    for (prev.move in MOVE_SET) {
      for (prev.move2 in MOVE_SET) {
        prev.2move.df = rbind(prev.2move.df, data.frame(player_id = PLAYER_SET,
                                                      player_move = player.move,
                                                      opponent.prev.move = prev.move,
                                                      opponent.prev.move2 = prev.move2,
                                                      n = 1, row.totals = length(MOVE_SET)))
        
      }
    }
  }
  tmp = data %>%
    # add each player's previous two moves, then use that when referencing opponent's previous moves
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1), # one move back (previous move)
           prev.move2 = lag(player_move, 2)) %>% # two moves back
    filter(!is.na(prev.move), !is.na(prev.move2)) %>% # lag call above sets NA for lag on first two moves: ignore it here
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1)), # opponent's one move back (previous move)
           opponent.prev.move2 = ifelse(is.na(lag(player_move, 1)), lead(prev.move2, 1), lag(prev.move2, 1)), # opponent's two moves back
           # category of move given opponent's two previous moves, e.g. "rock-paper-paper"
           move_2prev.opponent.move = paste(player_move, opponent.prev.move, opponent.prev.move2, sep = "-")) %>%
    filter(player_move != "none", opponent.prev.move != "none", opponent.prev.move2 != "none") %>% # ignore "none" moves for this aggregation
    group_by(player_id) %>%
    count(move_2prev.opponent.move) %>%
    group_by(player_id, move_2prev.opponent.move) %>%
    mutate(player_move = strsplit(move_2prev.opponent.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           opponent.prev.move = strsplit(move_2prev.opponent.move, "-")[[1]][2], # add opponent.prev.move back in because we lose it in the count() call above
           opponent.prev.move2 = strsplit(move_2prev.opponent.move, "-")[[1]][3]) %>% # add opponent.prev.move2 back in because we lose it in the count() call above
    group_by(player_id, opponent.prev.move, opponent.prev.move2) %>%
    mutate(row.totals = sum(n))
  
  # return initial counts set to 1 in prev.2move.df plus counts calculated in tmp above
  left_join(prev.2move.df, tmp, by = c("player_id", "player_move", "opponent.prev.move", "opponent.prev.move2")) %>%
    mutate(n.agg = ifelse(is.na(n.y), n.x, n.x + n.y),
           row.totals.agg = ifelse(is.na(row.totals.y), row.totals.x, row.totals.x + row.totals.y),
           pmove_opponent.2prev.move = n.agg / row.totals.agg) %>%
    select(player_id, player_move, opponent.prev.move, opponent.prev.move2, 
           n.agg, row.totals.agg, pmove_opponent.2prev.move)
}


#' Function to summarize probability of each move for each participant,
#' conditioned on the combination of their previous move *and* their opponent's previous move
get.player.opponent.prev.move.cond.probs = function(data) {
  prev.2move.df = data.frame(player_id = character(), player_move = character(), opponent.prev.move = character(), opponent.prev.move2 = character(),
                             n = numeric(), row.totals = numeric())
  # TODO can we do this without a nested loop....
  # Bayesian smoothing, put count of 1 in each combination before adding true counts
  for (player.move in MOVE_SET) {
    for (prev.move in MOVE_SET) {
      for (prev.move2 in MOVE_SET) {
        prev.2move.df = rbind(prev.2move.df, data.frame(player_id = PLAYER_SET,
                                                        player_move = player.move,
                                                        prev.move = prev.move,
                                                        opponent.prev.move = prev.move2,
                                                        n = 1, row.totals = length(MOVE_SET)))
        
      }
    }
  }
  
  tmp = data %>%
    # add each player's previous move, then use that when referencing opponent's previous move
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move)) %>% # lag call above sets NA for lag on very first move: ignore it here
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1)),
           # add category of move given previous move, opponent previous move (e.g. "rock-scissors-scissors")
           move_prev.move_opponent.prev.move = paste(player_move, prev.move, opponent.prev.move, sep = "-")) %>%
    filter(player_move != "none", prev.move != "none", opponent.prev.move != "none") %>% # ignore "none" moves for this purpose
    group_by(player_id) %>%
    count(move_prev.move_opponent.prev.move) %>%
    group_by(player_id, move_prev.move_opponent.prev.move) %>%
    mutate(player_move = strsplit(move_prev.move_opponent.prev.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           prev.move = strsplit(move_prev.move_opponent.prev.move, "-")[[1]][2], # add prev.move back in because we lose it in the count() call above
           opponent.prev.move = strsplit(move_prev.move_opponent.prev.move, "-")[[1]][3]) %>% # add opponent.prev.move back in because we lose it in the count() call above
    group_by(player_id, prev.move, opponent.prev.move) %>%
    mutate(row.totals = sum(n))
  
  # return initial counts set to 1 in prev.2move.df plus counts calculated in tmp above
  left_join(prev.2move.df, tmp, by = c("player_id", "player_move", "prev.move", "opponent.prev.move")) %>%
    mutate(n.agg = ifelse(is.na(n.y), n.x, n.x + n.y),
           row.totals.agg = ifelse(is.na(row.totals.y), row.totals.x, row.totals.x + row.totals.y),
           pmove_prev.move_opponent.prev.move = n.agg / row.totals.agg) %>%
    select(player_id, player_move, prev.move, opponent.prev.move, 
           n.agg, row.totals.agg, pmove_prev.move_opponent.prev.move)
    
}


################
### ANALYSIS ###
################


### First order analysis: p(move = x) -> 3 cells ###
# get overall probability of each move (for each player)
player.summary = get.player.move.dist(data)

# calculate entropy over the distribution of each move (for each player)
player.entropy = player.summary %>%
  group_by(player_id) %>%
  summarize(entropy.0 = -sum(pmove * log2(pmove))) # get entropy for distribution of each move


### Second order analysis 1: p(move = x | prev. move = y) -> 9 cells ###
# get probability of each move, given previous move
player.prev.move.summary = get.player.prev.move.cond.probs(data) 

# get marginal probability of each participant's previous moves
player.prev.move.marginal = get.player.prev.move.dist(data)

# calculate entropy based on probability above, including marginal probability of each previous move calculated in first order analysis above
player.prev.move.entropy = player.prev.move.summary %>%
  group_by(player_id, prev.move) %>%
  # get entropy for distribution of each move, for each prev. move
  summarize(prev.move.entropy = -sum(pmove_prev.move * log2(pmove_prev.move))) %>%
  inner_join(player.prev.move.marginal, by = "player_id") %>%
  # normalize entropy of each move given each prev. move by the probability of that prev. move
  mutate(prev.move.entropy.norm = prev.move.entropy * p.prev.move) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above (entropy values for a move scaled by the probability of that move)
  filter(prev.move.x == prev.move.y) %>%
  # sum normalized entropy for distribution of each move given each prev. move
  summarize(entropy.1.player = sum(prev.move.entropy.norm))
  

### Second order analysis 2: p(move = x | opponent prev. move = y) -> 9 cells ###
# get probability associated with a move, given each of the opponent's possible previous moves
opponent.prev.move.summary = get.opponent.prev.move.cond.probs(data)
# get marginal probability of each of the opponent's possible previous moves
opponent.prev.move.marginal = get.opponent.prev.move.dist(data)

# calculate entropy of the distribution of each move given opponent's previous move, scaled by opponent's marginal probability of each previous move
opponent.prev.move.entropy = opponent.prev.move.summary %>%
  group_by(player_id, opponent.prev.move) %>%
  summarize(opponent.prev.move.entropy = -sum(pmove_opponent.prev.move * log2(pmove_opponent.prev.move))) %>% # get entropy for distribution of each move, for each possible opponent's prev. move
  inner_join(opponent.prev.move.marginal, by = "player_id") %>%
  # normalize entropy of each move given each prev. move by the probability of that prev. move
  mutate(opponent.prev.move.entropy.norm = opponent.prev.move.entropy * p.opponent.prev.move) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above (entropy values for a move scaled by the probability of that move)
  filter(opponent.prev.move.x == opponent.prev.move.y) %>%
  # sum normalized entropy for distribution of each move given each prev. move
  summarize(entropy.1.opponent = sum(opponent.prev.move.entropy.norm))

 

### Third order analysis 1: p(move = x | prev. move = y, opponent prev. move = z) -> 27 cells ###
# get probability associated with a move, given combination of previous move, opponent's previous move
player.opponent.prev.move.summary = get.player.opponent.prev.move.cond.probs(data) 

# get marginal probability of each unique previous move, opponent's previous move combination
player.opponent.prev.move.marginal = get.player.opponent.prev.move.dist(data)

# calculate entropy of the distribution of each move given previous move and opponent's previous move
player.opponent.prev.move.entropy = player.opponent.prev.move.summary %>%
  group_by(player_id, prev.move, opponent.prev.move) %>%
  # get entropy for distribution of each move, for each possible opponent and player's prev. move
  summarize(player.opponent.prev.move.entropy = -sum(pmove_prev.move_opponent.prev.move * log2(pmove_prev.move_opponent.prev.move))) %>%
  # add column for coalescing previous move and opponent previous move
  mutate(prev.move_opponent.prev.move = paste(prev.move, opponent.prev.move, sep = "-")) %>%
  inner_join(player.opponent.prev.move.marginal, by = "player_id") %>%
  # normalize entropy of each move given each prev. move and opponent's prev. move by the probability of that prev. move combination
  mutate(player.opponent.prev.move.entropy.norm = player.opponent.prev.move.entropy * p.prev.move_opponent.prev.move) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above (entropy values for a move scaled by the probability of that move)
  filter(prev.move_opponent.prev.move.x == prev.move_opponent.prev.move.y) %>%
  # sum normalized entropy for distribution of each move given each prev. move, opponent's previous move
  summarize(entropy.2.player.opponent = sum(player.opponent.prev.move.entropy.norm))


### Third order analysis 2: p(move = x | prev. move 1 = y, prev. move 2 = z) -> 27 cells ###
# get probability of each move, given previous two moves
player.prev.2move.summary = get.player.prev.2move.cond.probs(data)

# get marginal probability of each unique set of previous two moves
player.prev.2move.marginal = get.player.2move.dist(data)

# calculate entropy based on probability above, including marginal probability of each set of previous two moves
player.prev.2move.entropy = player.prev.2move.summary %>%
  group_by(player_id, prev.move, prev.move2) %>%
  # get entropy for distribution of each move, for each set of prev. two moves
  summarize(prev.2move.entropy = -sum(pmove_2prev.move * log2(pmove_2prev.move))) %>%
  # add column for coalescing previous two moves
  mutate(prev.2moves = paste(prev.move, prev.move2, sep = "-")) %>%
  inner_join(player.prev.2move.marginal, by = "player_id") %>%
  # normalize entropy of each move given each set of previous two moves by the probability of those previous 2 moves
  mutate(prev.2move.entropy.norm = prev.2move.entropy * p.2prev.moves) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above (entropy values for a move scaled by the probability of that move)
  filter(prev.2moves.x == prev.2moves.y) %>%
  # sum normalized entropy for distribution of each move given each set of 2 prev. moves
  summarize(entropy.2.player = sum(prev.2move.entropy.norm))



### Third order analysis 3: p(move = x | opponent prev. move 1 = y, opponent prev. move 2 = z) -> 27 cells ###
# get probability of each move, given previous two moves by opponent
opponent.prev.2move.summary = get.opponent.prev.2move.cond.probs(data)

# get marginal probability of each unique set of previous two moves
opponent.prev.2move.marginal = get.opponent.2move.dist(data)

# calculate entropy based on probability above, including marginal probability of each set of opponent's previous two moves
opponent.prev.2move.entropy = opponent.prev.2move.summary %>%
  group_by(player_id, opponent.prev.move, opponent.prev.move2) %>%
  # get entropy for distribution of each move, for each set of opponent's prev. two moves
  summarize(opponent.prev.2move.entropy = -sum(pmove_opponent.2prev.move * log2(pmove_opponent.2prev.move))) %>%
  # add column for coalescing opponent's previous two moves
  mutate(prev.opponent.2moves = paste(opponent.prev.move, opponent.prev.move2, sep = "-")) %>%
  inner_join(opponent.prev.2move.marginal, by = "player_id") %>%
  # normalize entropy of each move given each set of opponent's previous two moves by the probability of those previous 2 moves
  mutate(opponent.prev.2move.entropy.norm = opponent.prev.2move.entropy * p.2prev.moves) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above (entropy values for a move scaled by the probability of that move)
  filter(prev.opponent.2moves.x == prev.opponent.2moves.y) %>%
  summarize(entropy.2.opponent = sum(opponent.prev.2move.entropy.norm)) # sum normalized entropy for distribution of each move given each set of opponent's 2 prev. moves



###############
### SUMMARY ###
###############


ENTROPY_SUMMARY = player.entropy %>%
  left_join(player.prev.move.entropy, by = "player_id") %>%
  left_join(opponent.prev.move.entropy, by = "player_id") %>%
  left_join(player.opponent.prev.move.entropy, by = "player_id") %>%
  left_join(player.prev.2move.entropy, by = "player_id") %>%
  left_join(opponent.prev.2move.entropy, by = "player_id") %>%
  gather(entropy.type, entropy.val, entropy.0:entropy.2.opponent)


graph.labels = c("entropy.0" = "Distribution of moves", 
                 "entropy.1.player" = "Distribution of moves given player's previous move",
                 "entropy.1.opponent" = "Distribution of moves given opponent's previous move",
                 "entropy.2.player.opponent" = "Distribution of moves given player's previous move, opponent's previous move",
                 "entropy.2.player" = "Distribution of moves given player's previous two moves",
                 "entropy.2.opponent" = "Distribution of moves given opponent's previous two moves")
xlab = "Participant move distributions"
ylab = "Shannon Entropy (S)"

ENTROPY_SUMMARY %>%
  ggplot() +
  geom_violin(aes(x = entropy.type, y = entropy.val, color = entropy.type)) +
  geom_jitter(aes(x = entropy.type, y = entropy.val, color = entropy.type), width = 0.25, alpha = 0.5, size = 2) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy values across participant move distributions") +
  scale_color_viridis(discrete = TRUE,
                      name = element_blank(),
                      labels = graph.labels,
                      guide = guide_legend(nrow = length(unique(ENTROPY_SUMMARY$entropy.type)))) +
  individ_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 20)) +
  theme(axis.text.x = element_blank())


ENTROPY_SUMMARY %>%
  group_by(entropy.type) %>%
  summarize(mean.entropy = mean(entropy.val),
            se.entropy = sd(entropy.val) / sqrt(length(unique(player_id)))) %>%
  ggplot(aes(x = entropy.type, color = entropy.type)) +
  geom_point(aes(y = mean.entropy), size = 3) +
  geom_errorbar(aes(ymin = mean.entropy - se.entropy, ymax = mean.entropy + se.entropy),
                width = 0.5) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy summary across participant move distributions") +
  scale_color_viridis(discrete = TRUE,
                      name = element_blank(),
                      labels = graph.labels,
                      guide = guide_legend(nrow = length(unique(ENTROPY_SUMMARY$entropy.type)))) +
  individ_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 20)) +
  theme(axis.text.x = element_blank())


#############
### NOTES ###
#############

#' The goal here is to be able to predict a person's next move based on some combination
#' of their previous moves and their opponent's previous moves due to dependencies in how people select moves.
#' First, we just test the idea that there is something predictive about all individuals generally.
#' Then, we dive into whether looking at an individual's dyad tells us even more (i.e. is there stable dyad behavior that varies across dyads)






