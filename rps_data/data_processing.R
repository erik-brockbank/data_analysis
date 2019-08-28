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


DATA_FILE = "rps_raw.csv" # name of file containing full dataset for all rounds


#######################
### DATA PROCESSING ###
#######################

# Function to read in and structure data appropriately
read.data = function(filename) {
  # read aggregate data
  data = read_csv(filename)
  
  # process data
  data$player_move = as.factor(data$player_move)
  data$player_outcome = as.factor(data$player_outcome)
  
  return(data)
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
  plot.title = element_text(face = "bold", size = 28),
  axis.title.y = element_text(face = "bold", size = 24),
  axis.title.x = element_text(face = "bold", size = 24),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14, angle = 90, hjust = 0, vjust = 0),
  # legend text
  legend.text = element_text(size = 14),
  # facet text
  strip.text = element_text(face = "bold", size = 20),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)


####################
### DATA SUMMARY ###
####################

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


# Check that each player is doing a reasonable distribution of moves
plot.moves = function(data) {
  move.summary = data %>%
    group_by(player_id) %>%
    count(player_move) %>%
    mutate(total = sum(n),
           move_pct = n / total)
  
  move.summary %>%
    ggplot(aes(x = player_move, y = move_pct)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_hline(yintercept = 0.33, linetype = "dashed", color = "red") +
    labs(x = "Move", y = "Percent") +
    ggtitle("Move percentage by participant") +
    individ_plot_theme +
    theme(strip.text = element_blank()) +
    facet_wrap(~player_id, scales = "free_y", ncol = 3)
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
    geom_histogram(binwidth = 20) +
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
# Plot RT distributions
plot.rt(data)
# Manually view the lowest RTs across participants
sort(data$player_rt)[1:50] # we have some very low RTs...


### Move choices ###
plot.moves(data)


### Scores ###
plot.scores(data) 
# for 100 rounds, max: 300; min: -100; expected: 66
# for 300 rounds, max: 900; min: -300; expected: 200


# TODO
# Longest sequence? 
# Distribution of outcomes for each player? 



