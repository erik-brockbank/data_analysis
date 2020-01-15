#' README
#' This script analyzes entropy of participant move choices *over time*
#' based on previous combinations of participant and opponent moves (R, P, S).
#' 
#' This script should *not* be used for general data overview, cleaning etc. 
#' (that happens in `data_processing.R`) and should not be used for analyses that extend
#' beyond move distribution dependencies.
#'


# SETUP ========================================================================

setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")
source('01_data_analysis-move_entropy.R') # may need 02 as well (instead)


DISCOUNT_FCTR = 0.9
# TODO confirm that our 1 was added to everything in other entropy analyses, not just 0 entries?
# TODO confirm that we just add SMOOTHING_FCTR at startup and not every round
SMOOTHING_FCTR = 0.1


# GRAPHING STYLE ===============================================================

plot_theme = theme(
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)

text_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 24),
  axis.title.y = element_text(face = "bold", size = 20),
  axis.title.x = element_text(face = "bold", size = 20),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 12),
  axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
  # legend text
  legend.text = element_text(size = 14),
  # facet text
  strip.text = element_text(size = 12)
)


# GRAPHING FUNCTIONS ===========================================================

plot_entropy_individ = function(data) {
  data %>%
    mutate(participant_abbrev = strsplit(player_id, "-")[[1]][5]) %>%
    ggplot(aes(x = round_index, y = entropy_moves)) +
    geom_line(color = "blue") +
    facet_wrap(~participant_abbrev, ncol = 5) +
    labs(x = "Round", y = "Entropy (S) of moves") +
    plot_theme
    #text_theme 
}


# ANALYSIS FUNCTIONS ===========================================================

# Count each person's cumulative total number of moves each round weighted by distance
get_weighted_move_counts = function(data) {
  # Create count and weighted count columns for each move
  newdat = data.frame()
  for (participant in unique(data$player_id)) {
    subj_dat = data %>%
      filter(player_id == participant) %>%
      select(game_id, player_id, round_index, player_move) %>%
      mutate(rock_bool = as.numeric(player_move == "rock"),
             paper_bool = as.numeric(player_move == "paper"),
             scissors_bool = as.numeric(player_move == "scissors"),
             rock_count_wt = rock_bool,
             paper_count_wt = paper_bool,
             scissors_count_wt = scissors_bool,
             rock_count_smooth = rock_count_wt + SMOOTHING_FCTR, # smoothing factor for 0 entries
             paper_count_smooth = paper_count_wt + SMOOTHING_FCTR, # smoothing factor for 0 entries
             scissors_count_smooth = scissors_count_wt + SMOOTHING_FCTR # smoothing factor for 0 entries
             )
    # Update weighted count columns for each round index
    # TODO can we avoid iterating here? ...
    for (round in seq(2, max(subj_dat$round_index))) {
      # xt = lambda * xt-1 + yt
      subj_dat$rock_count_wt[round] = DISCOUNT_FCTR * subj_dat$rock_count_wt[round - 1] + subj_dat$rock_bool[round]
      subj_dat$paper_count_wt[round] = DISCOUNT_FCTR * subj_dat$paper_count_wt[round - 1] + subj_dat$paper_bool[round]
      subj_dat$scissors_count_wt[round] = DISCOUNT_FCTR * subj_dat$scissors_count_wt[round - 1] + subj_dat$scissors_bool[round]
      # Add smoothing factor to weighted updates
      subj_dat$rock_count_smooth[round] = subj_dat$rock_count_wt[round] + SMOOTHING_FCTR
      subj_dat$paper_count_smooth[round] = subj_dat$paper_count_wt[round] + SMOOTHING_FCTR
      subj_dat$scissors_count_smooth[round] = subj_dat$scissors_count_wt[round] + SMOOTHING_FCTR
    }
    newdat = rbind(newdat, as.data.frame(subj_dat))
  }
  return(newdat)
}


# Takes in data frame with weighted count of moves from each round and calculates entropy
get_weighted_move_entropy = function(data) {
  data = data %>%
    group_by(player_id, round_index) %>%
    mutate(weighted_count_sum = sum(rock_count_smooth, paper_count_smooth, scissors_count_smooth),
           p_rock = rock_count_smooth / weighted_count_sum,
           p_paper = paper_count_smooth / weighted_count_sum,
           p_scissors = scissors_count_smooth / weighted_count_sum,
           entropy_moves = -sum(p_rock * log2(p_rock),
                                p_paper * log2(p_paper),
                                p_scissors * log2(p_scissors)))
  return(data)
}


# Count each person's cumulative total number of moves given each previous move
# and weighted by distance. 
# This fxn follows a simliar format to `get_weighted_move_counts`
get_weighted_move_counts_prev_move = function(data) {
  # Create count and weighted count columns for each move-previous move combination
  newdat = data.frame()
  for (participant in unique(data$player_id)) {
    subj_dat = data %>%
      filter(player_id == participant) %>%
      select(game_id, player_id, round_index, move_prev_move) %>%
      # TODO can we make this less manual?
      mutate(rock_rock_count = as.numeric(move_prev_move == "rock-rock"),
             rock_paper_count = as.numeric(move_prev_move == "rock-paper"),
             rock_scissors_count = as.numeric(move_prev_move == "rock-scissors"),
             paper_rock_count = as.numeric(move_prev_move == "paper-rock"),
             paper_paper_count = as.numeric(move_prev_move == "paper-paper"),
             paper_scissors_count = as.numeric(move_prev_move == "paper-scissors"),
             scissors_rock_count = as.numeric(move_prev_move == "scissors-rock"),
             scissors_paper_count = as.numeric(move_prev_move == "scissors-paper"),
             scissors_scissors_count = as.numeric(move_prev_move == "scissors-scissors"),
             rock_rock_count_wt = rock_rock_count + SMOOTHING_FCTR,  # smoothing factor for 0 entries
             rock_paper_count_wt = rock_paper_count + SMOOTHING_FCTR, # smoothing factor for 0 entries
             rock_scissors_count_wt = rock_scissors_count + SMOOTHING_FCTR, # smoothing factor for 0 entries
             paper_rock_count_wt = paper_rock_count + SMOOTHING_FCTR,  # smoothing factor for 0 entries
             paper_paper_count_wt = paper_paper_count + SMOOTHING_FCTR, # smoothing factor for 0 entries
             paper_scissors_count_wt = paper_scissors_count + SMOOTHING_FCTR, # smoothing factor for 0 entries
             scissors_rock_count_wt = scissors_rock_count + SMOOTHING_FCTR,  # smoothing factor for 0 entries
             scissors_paper_count_wt = scissors_paper_count + SMOOTHING_FCTR, # smoothing factor for 0 entries
             scissors_scissors_count_wt = scissors_scissors_count + SMOOTHING_FCTR) # smoothing factor for 0 entries
    
    # Update weighted count columns for each round
    # TODO can we avoid iterating here? ...
    for (round in seq(3, max(subj_dat$round_index))) {
      # xt = lambda * xt-1 + yt
      subj_dat$rock_rock_count_wt[round] = DISCOUNT_FCTR * subj_dat$rock_rock_count_wt[round - 1] + subj_dat$rock_rock_count[round] # TODO add smoothing factor each round here? 
      subj_dat$rock_paper_count_wt[round] = DISCOUNT_FCTR * subj_dat$rock_paper_count_wt[round - 1] + subj_dat$rock_paper_count[round]
      subj_dat$rock_scissors_count_wt[round] = DISCOUNT_FCTR * subj_dat$rock_scissors_count_wt[round - 1] + subj_dat$rock_scissors_count[round]
      subj_dat$paper_rock_count_wt[round] = DISCOUNT_FCTR * subj_dat$paper_rock_count_wt[round - 1] + subj_dat$paper_rock_count[round]
      subj_dat$paper_paper_count_wt[round] = DISCOUNT_FCTR * subj_dat$paper_paper_count_wt[round - 1] + subj_dat$paper_paper_count[round]
      subj_dat$paper_scissors_count_wt[round] = DISCOUNT_FCTR * subj_dat$paper_scissors_count_wt[round - 1] + subj_dat$paper_scissors_count[round]
      subj_dat$scissors_rock_count_wt[round] = DISCOUNT_FCTR * subj_dat$scissors_rock_count_wt[round - 1] + subj_dat$scissors_rock_count[round]
      subj_dat$scissors_paper_count_wt[round] = DISCOUNT_FCTR * subj_dat$scissors_paper_count_wt[round - 1] + subj_dat$scissors_paper_count[round]
      subj_dat$scissors_scissors_count_wt[round] = DISCOUNT_FCTR * subj_dat$scissors_scissors_count_wt[round - 1] + subj_dat$scissors_scissors_count[round]
    }
    
    newdat = rbind(newdat, as.data.frame(subj_dat))
  }
  return(newdat)
}


# Takes in data frame with weighted count of move-previous move combinations from each round
# and calculates entropy, normalizing over the probability in each round of each previous move
# This fxn follows a simliar format to `get_weighted_move_entropy`
get_weighted_move_entropy_prev_move = function(data) {
  data = data %>%
    filter(!is.na(prev_move)) %>%
    group_by(player_id, round_index) %>%
           # Total weighted count of previous move rock, paper, and scissors
    mutate(weighted_count_sum_rock = sum(rock_rock_count_wt, paper_rock_count_wt, scissors_rock_count_wt),
           weighted_count_sum_paper = sum(rock_paper_count_wt, paper_paper_count_wt, scissors_paper_count_wt),
           weighted_count_sum_scissors = sum(rock_scissors_count_wt, paper_scissors_count_wt, scissors_scissors_count_wt),
           # Conditional probability of each move-previous move combo (weighted count over weighted count of previous moves above)
           p_rock_rock = rock_rock_count_wt / weighted_count_sum_rock,
           p_rock_paper = rock_paper_count_wt / weighted_count_sum_paper,
           p_rock_scissors = rock_scissors_count_wt / weighted_count_sum_scissors,
           p_paper_rock = paper_rock_count_wt / weighted_count_sum_rock,
           p_paper_paper = paper_paper_count_wt / weighted_count_sum_paper,
           p_paper_scissors = paper_scissors_count_wt / weighted_count_sum_scissors,
           p_scissors_rock = scissors_rock_count_wt / weighted_count_sum_rock,
           p_scissors_paper = scissors_paper_count_wt / weighted_count_sum_paper,
           p_scissors_scissors = scissors_scissors_count_wt / weighted_count_sum_scissors,
           # Entropy of move distribution given previous move
           entropy_rock = -sum(p_rock_rock * log2(p_rock_rock),
                               p_paper_rock * log2(p_paper_rock),
                               p_scissors_rock * log2(p_scissors_rock)),
           entropy_paper = -sum(p_rock_paper * log2(p_rock_paper),
                               p_paper_paper * log2(p_paper_paper),
                               p_scissors_paper * log2(p_scissors_paper)),
           entropy_scissors = -sum(p_rock_scissors * log2(p_rock_scissors),
                               p_paper_scissors * log2(p_paper_scissors),
                               p_scissors_scissors * log2(p_scissors_scissors)))
  return(data)
}


# ANALYSIS =====================================================================

# 1. Entropy for moves
# -> get each participant's entropy per round for weighted probability of each move
newdat_moves = get_weighted_move_counts(data)
move_entropy = get_weighted_move_entropy(newdat_moves)
# TODO "none" moves create NA entropy values for all subsequent rounds. What to do about this?

# Graphs
# Entropy of each individual over each round
plot_entropy_individ(move_entropy)

# Get summary values for each individual
move_entropy_summary = move_entropy %>%
  group_by(player_id) %>%
  summarize(mean_ent = mean(entropy_moves, na.rm = T),
            vals = n(),
            se = sd(entropy_moves, na.rm = T) / sqrt(vals),
            ci.lower = mean_ent - se,
            ci.upper = mean_ent + se)

# Average entropy of each individual over all rounds compared to overall entropy over moves for that individual
move_entropy_summary %>%
  ggplot(aes(x = player_id, y = mean_ent)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper)) +
  geom_point(data = player.entropy, aes(x = player_id, y = entropy.0), color = "red") +
  labs(x = "Participant", y = "Entropy (S) over move distributions") +
  plot_theme +
  text_theme +
  theme(axis.text.x = element_blank())

move_entropy_high_level = move_entropy_summary %>%
  summarize(mean_entropy = mean(mean_ent),
            n = n(),
            se = sd(mean_ent) / sqrt(n),
            ci.lower = mean_entropy - se,
            ci.upper = mean_entropy + se)
move_entropy_overall_high_level = player.entropy %>%
  summarize(mean_entropy = mean(entropy.0),
            n = n(),
            se = sd(entropy.0) / sqrt(n),
            ci.lower = mean_entropy - se,
            ci.upper = mean_entropy + se)
  
move_entropy_high_level %>%
  ggplot(aes(y = mean_entropy, x = 1)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper), color = "blue") +
  geom_point(data = move_entropy_overall_high_level, aes(y = mean_entropy, x = 1), color = "red") +
  geom_errorbar(data = move_entropy_overall_high_level, aes(ymin = ci.lower, ymax = ci.upper), color = "red") +
  labs(x = "", y = "Entropy (S)") +
  plot_theme +
  text_theme +
  theme(axis.text.x = element_blank())


# 2. Entropy for moves given previous move
data = data %>%
  group_by(player_id) %>%
  mutate(prev_move = lag(player_move, 1),
         # category of move given previous move, e.g. "rock-paper"
         move_prev_move = paste(player_move, prev_move, sep = "-")) %>%
  filter(!is.na(prev_move)) %>%
  mutate(
    # marginal probability of each previous move, per round
    p_rock = cumsum(prev_move == "rock") / round_index,
    p_paper = cumsum(prev_move == "paper") / round_index,
    p_scissors = cumsum(prev_move == "scissors") / round_index)

newdat_prev_move = get_weighted_move_counts_prev_move(data)
prev_move_entropy = get_weighted_move_entropy_prev_move(newdat_prev_move)
# TODO normalize these entropy values by marginal probability of each previous move (at each index??)



