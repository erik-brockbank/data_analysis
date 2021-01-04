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
<<<<<<< HEAD
library(patchwork)
=======

>>>>>>> 90dd038de111de39b8cefa609f837c9ed98b4579


DATA_FILE = "rps_v1_data.csv" # name of file containing full dataset for all rounds
FREE_RESP_FILE = "rps_v1_data_freeResp.csv" # name of file containing free response data by participant
SLIDER_FILE = "rps_v1_data_sliderData.csv" # name of file containing slider Likert data by participant

<<<<<<< HEAD
NULL_SAMPLES = 10000

=======
>>>>>>> 90dd038de111de39b8cefa609f837c9ed98b4579
# Pilot info
# PILOT_DATA_FILE = "rps_pilot.csv" # name of file containing full dataset for all *pilot* rounds
# Participant IDs who played only 100 rounds (NB: this applies to pilot data only)
# SHORT_ROUND_PLAYERS = c("6ac3a837-c8cc-4bd0-9cd9-40d6dcd6c0c1", "960a15dd-c442-4693-a55d-0096fe8c14b3",
                        # "c45e2174-9181-43a9-a834-c918c4201273", "dd051569-60c1-43c6-8691-f49a47008cd8")


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


get_emp_score_differential = function(data) {
  score_diff = data %>%
    group_by(game_id) %>%
    filter(round_index == max(round_index)) %>%
    mutate(final_score = player_total + player_points,
           point_diff = abs(final_score - lag(final_score, 1))) %>%
    filter(!is.na(point_diff)) %>%
    select(game_id, point_diff)
  return(score_diff)
}


get_emp_win_differential = function(data) {
  win_diff = data %>%
    group_by(game_id, player_id) %>%
    count(win_count = player_outcome == "win") %>%
    filter(win_count == TRUE) %>%
    group_by(game_id) %>%
    mutate(opp_win_count = lag(n, 1)) %>%
    filter(!is.na(opp_win_count)) %>%
    summarize(win_diff = abs(n - opp_win_count))
  return(win_diff)
}

# Generate sample distribution for expected point differential
get_sample_score_differential = function(reps) {
  # Participant score differences go up by +/- 4 (+3 and -1) or 0 for win/loss or tie 
  score_diff_sample = data.frame(point_diff = replicate(reps, abs(sum(sample(c(-4, 0, 4), 300, replace = T)))))
  return(score_diff_sample)
}

get_sample_win_count_differential = function(reps) {
  win_diff_sample = data.frame(
<<<<<<< HEAD
    game_id = seq(1:reps),
=======
>>>>>>> 90dd038de111de39b8cefa609f837c9ed98b4579
    win_diff = replicate(reps, abs(sum(sample(c(-1, 0, 1), 300, replace = T))))
  )
  return(win_diff_sample)
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

# Investigate how much time each player spent viewing results of each round
plot.outcome.viewtime = function(data) {
  data %>%
    ggplot(aes(x = player_id, y = player_outcome_viewtime)) +
    geom_jitter(alpha = 0.5, size = 0.8, width = 0.25, color = "blue") +
    mylogy(c(1, 10000)) +
    labs(x = "Participant", y = "RT (ms)") +
    ggtitle("Outcome viewing times by participant") +
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
    geom_bar(stat = "identity", width = 0.5, color = "blue", fill = "blue", alpha = 0.5) +
    geom_hline(yintercept = 0.33, linetype = "dashed", color = "red") +
    labs(x = "", y = "") +
    ggtitle("Move percentage by participant") +
    individ_plot_theme +
    facet_wrap(~participant_abbrev, ncol = 5)
}

# Check how well-matched players were in each dyad
<<<<<<< HEAD
plot.outcomes = function(outcome_summary) {
  outcome_summary %>%
=======
plot.outcomes = function(data) {
  outcome.summary = data %>%
    group_by(game_id, round_index) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    group_by(game_id) %>%
    count(player_outcome) %>%
    mutate(total = sum(n),
           outcome_pct = n / total,
           game_id_abbrev = strsplit(game_id, "-")[[1]][5])
  
  outcome.summary %>%
>>>>>>> 90dd038de111de39b8cefa609f837c9ed98b4579
    ggplot(aes(x = player_outcome, y = outcome_pct)) +
    geom_bar(stat = "identity", width = 0.5, color = "blue", fill = "blue", alpha = 0.5) +
    geom_hline(yintercept = 0.33, linetype = "dashed", color = "red") +
    labs(x = "", y = "") +
    ggtitle("Outcome percentage by dyad") +
    individ_plot_theme +
    scale_x_discrete(name = element_blank(),
                     labels = c("win" = "win:loss", "loss" = "loss:win", "tie" = "tie:tie")) +
    facet_wrap(~game_id_abbrev, ncol = 5)
}

# Check final score differentials for each dyad
plot.score.differentials = function(score_diff, title, col) {
  score_diff %>%
    ggplot(aes(x = point_diff)) +
    geom_histogram(color = col, fill = col, alpha = 0.5, breaks = c(seq(0, 300, by = 50))) +
    labs(x = "Dyad score differentials", y = "Count") +
    ggtitle(title) +
    individ_plot_theme
}


# Plot win count differentials for each dyad
plot.win.differentials = function(win_diff, title, group) {
  win_diff %>%
    ggplot(aes(x = win_diff, color = group, fill = group)) +
    geom_histogram(alpha = 0.5, breaks = c(seq(0, 60, by = 10))) +
    labs(x = "Dyad win count differential", y = "Count") +
    # Explicit ylim values used for cog sci graphs to make them pretty
    #ylim(c(0, 6000)) +
    #ylim(c(0, 20)) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        #labels = element_blank(),
                        begin = 0.2,
                        end = 0.8) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       #labels = element_blank(),
                       begin = 0.2, 
                       end = 0.8) +
    ggtitle(title) +
    individ_plot_theme
    #theme(legend.position = "none")
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
    ggtitle("Move entropy by participant over game deciles") +
    individ_plot_theme +
    facet_wrap(~participant_abbrev, ncol = 5) +
    scale_x_continuous(name = element_blank(), breaks = seq(0, 10, 2))
}





################
### ANALYSIS ###
################

# Read in data
# data = read.data(PILOT_DATA_FILE)
data = read.data(DATA_FILE)
unique(data$game_id)

# Remove incomplete data
INCOMPLETE_DATA = data %>%
  group_by(player_id) %>%
  summarize(rounds = max(round_index)) %>%
  filter(rounds < 300) %>%
  select(player_id)

data = data %>%
  filter(!(player_id %in% INCOMPLETE_DATA$player_id))

glimpse(data)
unique(data$game_id)

# figure out which participants didn't write full data
# data %>%
#   group_by(game_id) %>%
#   filter(round_index == 300) %>%
#   mutate(completion = anytime(round_begin_ts))


### Response times ###
# Response time when choosing moves
plot.rt(data)

rt_summary = data %>%
  group_by(player_id) %>%
  summarize(mean_rt = mean(player_rt),
            nrounds = n())

mean(rt_summary$mean_rt)
sd(rt_summary$mean_rt)
  
# Response time when proceeding to next round after viewing results
plot.outcome.viewtime(data)

sort(data$player_rt)[1:50] # we have some very low RTs: should we exclude all < 800ms?
plot.rt.sequence(data) # by quartile, people do seem to be going faster: are they caring less?


### Move choices ###
plot.moves(data) # some of these people don't appear to have a stable balance of R, P, S: did they stop trying?


### Outcomes ###
<<<<<<< HEAD
outcome_summary = data %>%
  group_by(game_id, round_index) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  group_by(game_id) %>%
  count(player_outcome) %>%
  filter(!is.na(player_outcome)) %>% # TODO why do we have NAs in the data??
  mutate(total = sum(n),
         outcome_pct = n / total,
         game_id_abbrev = strsplit(game_id, "-")[[1]][5])

plot.outcomes(outcome_summary) # No players appear wildly mismatched, though score differential does suggest there may have been a couple
=======
plot.outcomes(data) # No players appear wildly mismatched, though score differential does suggest there may have been a couple
>>>>>>> 90dd038de111de39b8cefa609f837c9ed98b4579



### Scores ###
# Final score differentials for each dyad
score_diff_empirical = get_emp_score_differential(data)
plot.score.differentials(score_diff_empirical, "Empirical Data", "blue")
# Summary stats
mean(score_diff_empirical$point_diff)
sd(score_diff_empirical$point_diff) / sqrt(nrow(score_diff_empirical))


<<<<<<< HEAD
score_diff_null = get_sample_score_differential(NULL_SAMPLES)
=======
score_diff_null = get_sample_score_differential(10000)
>>>>>>> 90dd038de111de39b8cefa609f837c9ed98b4579
plot.score.differentials(score_diff_null, "Sampled Null Data", "red")
# Summary stats
mean(score_diff_null$point_diff)


<<<<<<< HEAD
### Win count differentials ###
# Empirical win count differentials
win_count_diff_empirical = get_emp_win_differential(data)
plot.win.differentials(win_count_diff_empirical, "Distribution of win count differentials", "Empirical data")
mean(win_count_diff_empirical$win_diff)
sd(win_count_diff_empirical$win_diff) / sqrt(nrow(win_count_diff_empirical))
max(win_count_diff_empirical$win_diff)

# Sampled null win count differentials
win_count_diff_null = get_sample_win_count_differential(NULL_SAMPLES)
plot.win.differentials(win_count_diff_null, "Distribution of win count differentials", "Sampled null data")
mean(win_count_diff_null$win_diff)
max(win_count_diff_null$win_diff)

# Chi-squared comparison of empirical and null counts
emp_win_count_bins = win_count_diff_empirical %>%
  group_by(bin = cut(win_diff, breaks = seq(0, 60, by = 10), include.lowest = TRUE)) %>% 
  summarise(n = n())

null_win_count_bins = win_count_diff_null %>%
  filter(win_diff <= max(win_count_diff_empirical$win_diff)) %>%
  group_by(bin = cut(win_diff, breaks = seq(0, 60, by = 10), include.lowest = TRUE)) %>% 
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

chisq.test(emp_win_count_bins$n, p = null_win_count_bins$prop)


# Confirm results when truncating the top percentiles
win_count_diff_empirical_top = win_count_diff_empirical %>%
  top_frac(win_diff, n = 0.1)
win_count_diff_empirical_truncated = win_count_diff_empirical %>%
  filter(!game_id %in% win_count_diff_empirical_top$game_id)
max(win_count_diff_empirical_truncated$win_diff)

emp_win_count_bins_truncated = win_count_diff_empirical_truncated %>%
  group_by(bin = cut(win_diff, breaks = seq(0, 60, by = 10), include.lowest = TRUE)) %>% 
  summarise(n = n())

null_win_count_bins = win_count_diff_null %>%
  filter(win_diff <= max(win_count_diff_empirical_truncated$win_diff)) %>%
  group_by(bin = cut(win_diff, breaks = seq(0, 60, by = 10), include.lowest = TRUE)) %>% 
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

chisq.test(emp_win_count_bins_truncated$n, p = null_win_count_bins$prop)


# Plot histograms overlaid for easier comparison
win_count_diff_empirical = win_count_diff_empirical %>%
  mutate(cat = "Empirical data")
win_count_diff_null = win_count_diff_null %>%
  mutate(cat = "Sampled null data")

empirical_n = win_count_diff_empirical %>% nrow()
# scale_factor = NULL_SAMPLES / empirical_n
scale_factor = empirical_n / NULL_SAMPLES

win_count_diff_empirical %>%
  ggplot(aes(x = win_diff, color = cat, fill = cat)) +
  geom_histogram(#aes(y = ..count.. * scale_factor),
                 alpha = 0.4,
                 breaks = c(seq(0, 60, by = 10)),
                 position = "identity") +
  geom_histogram(data = win_count_diff_null,
                 aes(y = ..count.. * scale_factor, x = win_diff, color = cat, fill = cat),
                 alpha = 0.6,
                 breaks = c(seq(0, 60, by = 10)),
                 position = "identity") +
  labs(x = "Dyad win count differential", y = "Count (equal scales)") +
  # Explicit ylim values used for cog sci graphs to make them pretty
  #ylim(c(0, 6000)) +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      begin = 0.2,
                      end = 0.8) +
  scale_fill_viridis(discrete = T,
                     name = element_blank(),
                     begin = 0.2, 
                     end = 0.8) +
  ggtitle("Distribution of win count differentials") +
  individ_plot_theme

# Analysis: Do people have disproportionate numbers of wins/losses overall?
# Chi-squared test of win/loss/tie counts for each dyad

# TODO look into why we have NA outcomes...
data %>% filter(is.na(player_outcome))

chisq_tests = data.frame(game_id = character(), 
                         chisq = numeric(), p_val = numeric(), 
                         loss = numeric(), tie = numeric(), win = numeric(),
                         stringsAsFactors = F)

outcome_summary = data %>%
  group_by(game_id, round_index) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  group_by(game_id) %>%
  count(player_outcome) %>%
  filter(!is.na(player_outcome)) %>% # TODO why do we have NAs in the data??
  filter(!is.na(player_outcome))

for (game in unique(outcome_summary$game_id)) {
  game_data = outcome_summary %>%
    filter(game_id == game)
  tst = chisq.test(game_data$n)
  vals =  game_data %>%
    spread(key = "player_outcome", value = "n")
  chisq_tests = rbind(chisq_tests, data.frame(game_id = as.character(game), 
                                              chisq = tst$statistic, p_val = tst$p.value,
                                              loss = vals$loss, tie = vals$tie, win = vals$win,
                                              stringsAsFactors = F))
}
chisq_tests

win_diff_comparison  = chisq_tests %>%
  inner_join(win_count_diff_empirical, by = "game_id")
win_diff_comparison

total_dyads = win_diff_comparison %>% nrow()
significant_dyads = win_diff_comparison %>%
  filter(p_val <= 0.05) %>%
  nrow()

win_diff_comparison %>%
  filter(p_val <= 0.05)

win_diff_comparison %>%
  filter(win_diff >= 30)




=======
### Win counts ###
win_diff_empirical = get_emp_win_differential(data)
plot.win.differentials(win_diff_empirical, "Distribution of win count differentials", "Empirical data")
# Summary stats
mean(win_diff_empirical$win_diff)
sd(win_diff_empirical$win_diff) / sqrt(nrow(win_diff_empirical))

win_diff_null = get_sample_win_count_differential(10000)
plot.win.differentials(win_diff_null, "Distribution of win count differentials", "Sampled null data")
# Summary stats
mean(win_diff_null$win_diff)
>>>>>>> 90dd038de111de39b8cefa609f837c9ed98b4579


### Effort ###
# Are people trying throughout the experiment?
# entropy wobbles around by decile but doesnt appear to fall off a cliff for anybody (min is 1.2)
plot.move.sequence(data)


# Completion time
completion_summary = data %>%
  group_by(player_id) %>%
  summarize(completion_time = round_begin_ts[round_index == 300],
            start_time =  round_begin_ts[round_index == 1],
            total_secs = (completion_time - start_time) / 1000)

mean(completion_summary$total_secs)
sd(completion_summary$total_secs)

###################
### SURVEY DATA ###
###################

# TODO remove incomplete data participants for any actual data presentation

fr_data = read.data(FREE_RESP_FILE)
slider_data = read.data(SLIDER_FILE)

# Remove incomplete data participants
slider_data = slider_data %>%
  filter(!(player_id %in% INCOMPLETE_DATA$player_id))
glimpse(slider_data)

unique(slider_data$player_id)
unique(fr_data$player_id)

### Free response answers ###
fr_data$free_resp_answer # any notable responses here?


### Likert slider data ###
slider_summary = slider_data %>%
  group_by(statement) %>%
  summarize(mean_resp = mean(resp),
            n = n(),
            se = sd(resp) / sqrt(n),
            ci.lower = mean_resp - se,
            ci.upper = mean_resp + se)

slider_data %>%
  ggplot(aes(x = statement, y = resp, color = str_wrap(statement, width = 45))) +
  geom_jitter(width = 0.25, alpha = 0.75) +
  geom_point(data = slider_summary, aes(x = statement, y = mean_resp, color = str_wrap(statement, width = 45)), size = 5) +
  geom_errorbar(data = slider_summary, aes(x = statement, y = mean_resp, ymin = ci.lower, ymax = ci.upper, color = str_wrap(statement, width = 45))) +
  labs(x = "Slider statement", y = "Response (1-7; Strongly Disagree-Strongly Agree)") +
  individ_plot_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 11, face = "italic"),
        legend.title = element_blank()) +
  scale_color_viridis(discrete = T)

# TODO make mean+SEM plot of the above






