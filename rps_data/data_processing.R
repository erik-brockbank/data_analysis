#'
#' This is the data formatting and cleanup script for RPS data
#' This script should be used to view the data in summary and look for outliers or 
#' data cleanliness issues.
#' This script should *not* be used for more complex analyses. 
#' 

# TODO make this a summary RMD once we have all data


rm(list = ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")

library(tidyverse)
library(viridis)


DATA_FILE = "rps_raw.csv" # name of file containing full dataset for all rounds
SHORT_ROUND_PLAYERS = c("6ac3a837-c8cc-4bd0-9cd9-40d6dcd6c0c1", "960a15dd-c442-4693-a55d-0096fe8c14b3",
                        "c45e2174-9181-43a9-a834-c918c4201273", "dd051569-60c1-43c6-8691-f49a47008cd8")

#######################
### DATA PROCESSING ###
#######################

# Function to read in and structure data appropriately
read.data = function(filename) {
  # read aggregate data
  data = read_csv(filename)
  return(data)
}

# Function to calculate Shannon entropy for proportions of R, P, S
# requires that data come in with a "player_move" col and a "move_pct" col
# returns entropy value for that set of move proportions
get.entropy = function(proportions) {
  # S = - SUM_i { p_i * log(p_i) } for probability vector p of R, P, S
  -sum(proportions$move_pct * log2(proportions$move_pct))
}


######################
### GRAPHING STYLE ###
######################

# Graphing functions
my.log.breaks = function(lims){
  majors = seq(floor(log10(lims[1])), ceiling(log10(lims[2])), by = 1)
  minors = log10(unlist(lapply(majors[-1], function(x){seq(10 ^ (x - 1), 9 * 10 ^ (x - 1), by = 10 ^ (x - 1))})))
  return(list(majors, minors))
}

mylogx = function(lims){
  breaks = my.log.breaks(lims)
  scale_x_log10(limits = lims, 
                breaks = 10 ^ breaks[[1]], 
                minor_breaks = breaks[[2]])
}

mylogy = function(lims){
  breaks = my.log.breaks(lims)
  scale_y_log10(limits = lims, 
                breaks = 10 ^ breaks[[1]], 
                minor_breaks = breaks[[2]])
}

individ_plot_theme = theme(
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
  strip.text = element_text(size = 12),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)


##########################
### GRAPHING FUNCTIONS ###
##########################

# Check that each player's response times aren't so low that they may not be trying
plot.rt = function(data) {
  data %>%
    ggplot(aes(x = player_id, y = player_rt)) +
    geom_jitter(alpha = 0.5, size = 0.8, width = 0.25, color = "blue") +
    mylogy(c(1, 10000)) +
    labs(x = "Participant", y = "RT (ms)") +
    ggtitle("Response times by participant") +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}

# Check that each player's response times stay relatively consistent over the experiment
plot.rt.sequence = function(data) {
  quartile.data = data %>%
    group_by(player_id) %>%
    mutate(round_quartile = ntile(round_index, 4)) %>%
    #ungroup() %>%
    group_by(round_quartile) %>%
    summarize(quartile.mean = mean(player_rt),
              quartile.se = sd(player_rt) / sqrt(length(player_rt)))
  
  quartile.data %>%
    ggplot(aes(x = round_quartile, y = quartile.mean)) +
    geom_point() +
    geom_errorbar(aes(ymin = quartile.mean - quartile.se, ymax = quartile.mean + quartile.se)) +
    labs(x = "Trial quartile", y = "Mean RT") +
    ggtitle("Participant response times by quartile") +
    individ_plot_theme
}


# Check that each player is doing a reasonable distribution of moves
plot.moves = function(data) {
  move.summary = data %>%
    group_by(player_id) %>%
    count(player_move) %>%
    mutate(total = sum(n),
           move_pct = n / total,
           participant_abbrev = strsplit(player_id, "-")[[1]][5])
  
  move.summary %>%
    ggplot(aes(x = player_move, y = move_pct)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_hline(yintercept = 0.33, linetype = "dashed", color = "red") +
    labs(x = "", y = "") +
    ggtitle("Move percentage by participant") +
    individ_plot_theme +
    facet_wrap(~participant_abbrev, scales = "free_y", ncol = 5)
}

# Check how well-matched players were in each dyad
plot.outcomes = function(data) {
  
  outcome.summary = data %>%
    group_by(game_id, round_index) %>%
    #arrange(round_index) %>%
    filter(row_number()==1) %>%
    ungroup() %>%
    group_by(game_id) %>%
    count(player_outcome) %>%
    mutate(total = sum(n),
           outcome_pct = n / total,
           game_id_abbrev = strsplit(game_id, "-")[[1]][5])
  
  outcome.summary %>%
    ggplot(aes(x = player_outcome, y = outcome_pct)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_hline(yintercept = 0.33, linetype = "dashed", color = "red") +
    labs(x = "", y = "") +
    ggtitle("Outcome percentage by dyad") +
    individ_plot_theme +
    scale_x_discrete(name = element_blank(),
                     labels = c("win" = "win:loss", "loss" = "loss:win", "tie" = "tie:tie")) +
    facet_wrap(~game_id_abbrev, ncol = 5)
}


# Check that entropy of each player's moves stays relatively consistent
# -> If it goes down substantially, this is a sign they may have stopped trying
plot.move.sequence = function(data) {
  subsets = 10
  subset.data = data %>%
    group_by(player_id) %>%
    mutate(round_ntile = ntile(round_index, subsets)) %>%
    group_by(player_id, round_ntile) %>%
    count(player_move) %>%
    mutate(total = sum(n),
           move_pct = n / total,
           participant_abbrev = strsplit(player_id, "-")[[1]][5])
  
  # there has to be a way to do this without looping...
  entropy.seq = data.frame(participant_abbrev = character(), subset = numeric(), entropy = double())
  for (player in unique(subset.data$participant_abbrev)) {
    for (subset in seq(1:subsets)) {
      subj.ntile.data = subset.data %>% 
        filter(participant_abbrev == player, round_ntile == subset)
      entropy.seq = rbind(entropy.seq, data.frame(participant_abbrev = player, subset = subset,
                                                  entropy = get.entropy(subj.ntile.data)))
    }
  }

  entropy.seq %>%
    ggplot(aes(x = subset, y = entropy)) +
    geom_line() +
    labs(x = "", y = "Shannon entropy (S)") +
    ggtitle("Entropy of participant moves over trial segments") +
    individ_plot_theme +
    facet_wrap(~participant_abbrev, scales = "free_y", ncol = 5) +
    scale_x_continuous(name = element_blank(), breaks = seq(0, 10, 2))
}

# Check that no player did so terribly that we should be suspicious of whether they were trying
# (this should show up in the RT and move choices as well)
plot.scores = function(data) {
  score.summary = data %>%
    group_by(player_id) %>%
    filter(round_index == max(round_index)) %>%
    mutate(final_score = player_total + player_points) %>%
    select(player_id, final_score)
  
  print(score.summary)
  
  score.summary %>%
    ggplot(aes(x = final_score)) +
    geom_histogram(binwidth = 10) +
    labs(x = "Participant scores") +
    ggtitle("Distribution of participant final scores") +
    individ_plot_theme
}



################
### ANALYSIS ###
################

# Read in data
data = read.data(DATA_FILE)

# Overview of data
glimpse(data)
table(data$player_id)


### Response times ###
plot.rt(data) # 100 trial participants look a lot like 300 trial participants
sort(data$player_rt)[1:50] # we have some very low RTs: should we exclude all < 800ms?
plot.rt.sequence(data) # by quartile, people do seem to be going faster: are they caring less?


### Move choices ###
plot.moves(data) # some of these people don't appear to have a stable balance of R, P, S: did they stop trying?

chisq_tests = data.frame(player_id = character(), chisq = numeric(), p_val = numeric())
for (player in unique(data$player_id)) {
  subj.data = data %>%
    filter(player_id == player,
           player_move %in% c("rock", "paper", "scissors"))
  tst = chisq.test(table(subj.data$player_move))
  chisq_tests = rbind(chisq_tests, data.frame(player_id = player, chisq = tst$statistic, p_val = tst$p.value))
}
chisq_tests # several of these are highly significant: people are not choosing an even balance of R, P, S


### Outcomes ###
plot.outcomes(data) # No players appear wildly mismatched, though score differential does suggest there may have been a couple


### Scores ###
plot.scores(data) 
# for 100 rounds, max: 300; min: -100; expected: 66
# for 300 rounds, max: 900; min: -300; expected: 200
# Look at final point differentials by dyad to see what distribution of point differences looked like
data %>%
  group_by(game_id) %>%
  filter(round_index == max(round_index)) %>%
  mutate(final_score = player_total + player_points,
         point_diff = abs(final_score - lag(final_score, 1))) %>%
  filter(!is.na(point_diff)) %>%
  select(game_id, point_diff)


### Effort ###
# Are people trying throughout the experiment?
# Note this should also be reflected in the response times
plot.move.sequence(data) # entropy wobbles around by decile but doesnt appear to fall off a cliff for anybody




# TODO plot score as 1, -1, 0 then take the highest ones in each dyad to get a sense of whether anybody is reliably winning
# can also plot 1, -1, 0 for each person over time to see if somebody "figures their opponent out" (look for long sequences at 1 or -1)


# ANALYSIS: are the percentage of 3-move sequences what we would expect from a random player?
seq.counts3 = data %>%
  group_by(player_id) %>%
  mutate(prev.move = lag(player_move, 1), # one move back (previous move)
         prev.move2 = lag(player_move, 2), # two moves back
         prev.move3 = lag(player_move, 3)) %>% # three moves back
  filter(!is.na(prev.move), !is.na(prev.move2), !is.na(prev.move3), # lag calls above set NA for lag on first three moves: ignore it here
         player_move != "none", prev.move != "none", prev.move2 != "none", prev.move3 != "none") %>%
  group_by(player_id, round_index) %>%
  mutate(rock.count = sum(prev.move == "rock", prev.move2 == "rock", prev.move3 == "rock"),
         paper.count = sum(prev.move == "paper", prev.move2 == "paper", prev.move3 == "paper"),
         scissors.count = sum(prev.move == "scissors", prev.move2 == "scissors", prev.move3 == "scissors"),
         min.col = min(c(rock.count, paper.count, scissors.count)),
         mid.col = median(c(rock.count, paper.count, scissors.count)),
         max.col = max(c(rock.count, paper.count, scissors.count)),
         seq.type = paste(min.col, mid.col, max.col, sep = "-")) %>%
  ungroup() %>%
  count(seq.type) %>%
  mutate(pct = n / sum(n))

seq.counts3
chisq.test(seq.counts3$n, p = c((1/9), (6/9), (2/9)))



seq.counts1 = data %>%
  filter(player_move != "none") %>%
  count(player_move)

seq.counts1
chisq.test(x = seq.counts1$n, p = c(1/3, 1/3, 1/3))


seq.counts2 = data %>%
  group_by(player_id) %>%
  mutate(prev.move = lag(player_move, 1), # one move back (previous move)
         prev.move2 = lag(player_move, 2)) %>% # two moves back
  filter(!is.na(prev.move), !is.na(prev.move2), # lag calls above set NA for lag on first two moves: ignore it here
         player_move != "none", prev.move != "none", prev.move2 != "none") %>%
  group_by(player_id, round_index) %>%
  mutate(rock.count = sum(prev.move == "rock", prev.move2 == "rock"),
         paper.count = sum(prev.move == "paper", prev.move2 == "paper"),
         scissors.count = sum(prev.move == "scissors", prev.move2 == "scissors"),
         min.col = min(c(rock.count, paper.count, scissors.count)),
         mid.col = median(c(rock.count, paper.count, scissors.count)),
         max.col = max(c(rock.count, paper.count, scissors.count)),
         seq.type = paste(min.col, mid.col, max.col, sep = "-")) %>%
  #as.data.frame() %>%
  ungroup() %>%
  count(seq.type) %>%
  mutate(pct = n / sum(n))

seq.counts2
chisq.test(x = seq.counts2$n, p = c(1/3, 2/3))

