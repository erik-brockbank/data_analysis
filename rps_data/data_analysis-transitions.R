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



rm(list = ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")

source('data_processing.R') # script used for data processing/cleanup
glimpse(data)



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


#' Function to get marginal probability of each outcome (win, loss, tie) for each participant
get.player.outcome.dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.outcome = lag(player_outcome, 1)) %>%
    filter(!is.na(prev.outcome)) %>% # lag call above sets NA for lag on first outcome: ignore it here
    count(prev.outcome) %>%
    mutate(total.outcomes = sum(n),
           p.outcome = n / total.outcomes)
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



################
### ANALYSIS ###
################


### First order analysis: p(transition = x) -> 3 cells ###
# get overall probability of each transition (for each player)
player.transition.summary = get.player.transition.dist(data)

# calculate entropy over the distribution of each move (for each player)
player.transition.entropy = player.transition.summary %>%
  group_by(player_id) %>%
  summarize(entropy.0 = -sum(p.transition * log2(p.transition))) # get entropy for distribution of each transition



### Second order analysis 1: p(transition = x | prev. outcome = y) -> 9 cells ###
# get marginal probability of each participant's previous outcome
player.prev.outcome.prob = get.player.outcome.dist(data)

# get probability of each transition, given previous outcome
player.transition.prev.outcome = get.player.transition.outcome.cond(data) 



# calculate entropy based on probability above, including marginal probability of each previous move calculated in first order analysis above
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
  summarize(entropy.1.player.outcome = sum(prev.outcome.entropy.norm))





###############
### SUMMARY ###
###############


ENTROPY_SUMMARY = player.transition.entropy %>%
  left_join(player.transition.outcome.entropy, by = "player_id") %>%
  gather(entropy.type, entropy.val, entropy.0:entropy.1.player.outcome)


graph.labels = c("entropy.0" = "Distribution of transitions (+/-/0)",
                 "entropy.1.player.outcome" = "Distribution of transitions given player's previous outcome (win/tie/loss)")

ENTROPY_SUMMARY %>%
  ggplot() +
  geom_violin(aes(x = entropy.type, y = entropy.val, color = entropy.type)) +
  geom_jitter(aes(x = entropy.type, y = entropy.val, color = entropy.type), width = 0.25, alpha = 0.5, size = 2) +
  labs(x = "Participant transition distributions", y = "Shannon Entropy (S)") +
  ggtitle("Entropy values across participant transition distributions") +
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
  labs(x = "Participant move distributions", y = "Shannon Entropy (S)") +
  ggtitle("Entropy summary across participant move distributions") +
  scale_color_viridis(discrete = TRUE,
                      name = element_blank(),
                      labels = graph.labels,
                      guide = guide_legend(nrow = length(unique(ENTROPY_SUMMARY$entropy.type)))) +
  individ_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 20)) +
  theme(axis.text.x = element_blank())



