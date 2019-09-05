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
  axis.text.x = element_text(size = 12, angle = 90, hjust = 0, vjust = 0),
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
    labs(x = "Participant", y = "Log10 trial RT (ms, max 10000)") +
    ggtitle("Response times by participant") +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}

# Check that each player's response times stay relatively consistent over the experiment
plot.rt.sequence = function(data) {
  quartile.data = data %>%
    group_by(player_id) %>%
    mutate(round_quartile = ntile(round_index, 4)) %>%
    ungroup() %>%
    group_by(round_quartile) %>%
    summarize(quartile.mean = mean(player_rt),
              quartile.se = sd(player_rt) / sqrt(length(player_rt)))
  
  quartile.data %>%
    ggplot(aes(x = round_quartile, y = quartile.mean)) +
    geom_point() +
    geom_errorbar(aes(ymin = quartile.mean - quartile.se, ymax = quartile.mean + quartile.se)) +
    labs(x = "Trial quartile", y = "Participant response times") +
    ggtitle("Change in participant response times by quartile") +
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
    labs(x = "Move", y = "Percent") +
    ggtitle("Move percentage by participant") +
    individ_plot_theme +
    facet_wrap(~participant_abbrev, scales = "free_y", ncol = 4)
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
    labs(x = "ntile of trials", y = "Shannon entropy (S)") +
    ggtitle("Entropy of participant moves over trial ntile") +
    individ_plot_theme +
    facet_wrap(~participant_abbrev, scales = "free_y", ncol = 4)

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


### Scores ###
plot.scores(data) 
# for 100 rounds, max: 300; min: -100; expected: 66
# for 300 rounds, max: 900; min: -300; expected: 200


### Effort ###
# Are people trying throughout the experiment?
# Note this should also be reflected in the response times
plot.move.sequence(data) # entropy wobbles around by decile but doesnt appear to fall off a cliff for anybody



# TODO
# Distribution of outcomes for each player? 



