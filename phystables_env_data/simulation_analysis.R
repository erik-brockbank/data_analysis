#' Script for analyzing simulation data across the scenarios used in the task
#' Questions:
#' 1. Do simulations in the low complexity scenarios look like people's response time
#' 2. Do simulations in the high complexity scenarios frequently fail to terminate?



rm(list=ls())
setwd("~/web/vullab/data_analysis/phystables_env_data")


library(tidyverse)
library(viridis)

SIM_DATA_FILEPATH = "containment_sims.csv"
TERMINATION_DATA_FILEPATH = "containment_sims_v2.csv"


##########################
### ANALYSIS FUNCTIONS ###
##########################

read.data = function(filepath) {
  data = read_csv(filepath)
  
  # add scenario, complexity, containment rotation
  scenario.split = with(data, strsplit(trialname, "_"))
  data$scenario = unlist(lapply(scenario.split, "[", 2))
  data$containment = unlist(lapply(scenario.split, "[", 4))
  data$complexity = unlist(lapply(scenario.split, "[", 6))
  data$rotation = unlist(lapply(scenario.split, "[", 7))
  data$rotation[is.na(data$rotation)] = "NONE" # avoid NAs in this column
  
  # convert trial sims to milliseconds, add log trial sims
  # data$tsims.ms = data$tsims * 1000
  # data$log.tsims.ms = log10(data$tsims.ms)
  data$sim_time.ms = data$simulation_time * 1000
  data$log.sim_time.ms = log10(data$sim_time.ms)
  
  return(data)
}


# Note these are copied over from `data_analysis_kitchen_sink.R`, may be
# worth putting them in a general purpose library to source

CONTAINMENT_LABELS = c(
  l1 = "low containment",
  l2 = "medium containment",
  l3 = "high containment"
)

COMPLEXITY_LABELS = c(
  l1 = "none",
  l2 = "low",
  l3 = "medium",
  l4 = "high"
)

SCENARIO_LABELS = c(
  sc1 = "scenario 1",
  sc2 = "scenario 2",
  sc3 = "scenario 3",
  sc4 = "scenario 4"
)

default.theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 32),
  axis.title.y = element_text(face = "bold", size = 36),
  axis.title.x = element_text(face = "bold", size = 36),
  # axis text
  axis.text.x = element_text(size = 24),
  axis.text.y = element_text(size = 20),
  # facet text
  strip.text = element_text(face = "bold", size = 28),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line(color = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent")
)

containment.complexity.plot = function(mean.data, title = "", xlab = "", ylab = "") {
  mean.data %>%
    ggplot(aes(x = complexity, y = means)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
    scale_x_discrete(labels = COMPLEXITY_LABELS) +
    facet_wrap(. ~ containment,
               # scales = "free",
               labeller = labeller(containment = CONTAINMENT_LABELS)) +
    labs(x = xlab, y = ylab) +
    ggtitle(title) +
    default.theme
}

scenario.plot = function(mean.data, title = "", xlab = "", ylab = "") {
  mean.data %>%
    ggplot(aes(x = complexity, y = means, color = scenario)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.5) +
    scale_x_discrete(labels = COMPLEXITY_LABELS) +
    guides(color = FALSE) +
    facet_grid(scenario ~ containment,
               scales = "free",
               labeller = labeller(containment = CONTAINMENT_LABELS,
                                   scenario = SCENARIO_LABELS)) +
    labs(x = xlab, y = ylab) +
    ggtitle(title) +
    default.theme +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
}



########################
### SUMMARY ANALYSIS ###
########################

# data = read.data(SIM_DATA_FILEPATH)
data = read.data(TERMINATION_DATA_FILEPATH)

# Remove non-terminated trials
sim.data_5s = data %>%
  filter(simulation_time <= 5)
sim.data_10s = data %>%
  filter(simulation_time <= 10)
sim.data_30s = data %>%
  filter(simulation_time <= 30)
sim.data_60s = data %>%
  filter(simulation_time <= 60)



#### MEAN SIMTIME ###
title = "Simulation times across complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean simulation time (ms)"

# Calculate means, CIs
# simtime.means = sim.data_5s %>%
# simtime.means = sim.data_10s %>%
# simtime.means = sim.data_30s %>%
# simtime.means = sim.data_60s %>%
simetime.means = get.containment.complexity.se(sim.data_30s)

simtime.means = data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(sim_time.ms),
            trials = n(),
            se.lower = means - sqrt(var(sim_time.ms) / length(sim_time.ms)),
            se.upper = means + sqrt(var(sim_time.ms) / length(sim_time.ms))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
containment.complexity.plot(simtime.means, title, xlab, ylab)


### LOG SIMTIME ### 
title = "Log simulation times across complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean simulation time (log10 ms)"

# Calculate means, CIs
log.simtime.means = sim.data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(log.tsims.ms),
            trials = n(),
            se.lower = means - sqrt(var(log.tsims.ms) / length(log.tsims.ms)),
            se.upper = means + sqrt(var(log.tsims.ms) / length(log.tsims.ms))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
containment.complexity.plot(log.simtime.means, title, xlab, ylab)

# View proportion of time-up (non terminating) simulations
data %>%
  group_by(containment, complexity) %>%
  summarize(trials = n(),
            goal = sum(outcome == "REDGOAL" | outcome == "GREENGOAL"),
            timeup = sum(outcome == "TIMEUP"),
            timeup.prop = timeup / trials)



### MEAN BOUNCES ###
title = "Number of bounces across complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean bounces"

# Calculate means, CIs
bounce.means = sim.data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(bounces),
            trials = n(),
            se.lower = means - sqrt(var(bounces) / length(bounces)),
            se.upper = means + sqrt(var(bounces) / length(bounces))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
containment.complexity.plot(bounce.means, title, xlab, ylab)



### SIMTIME PER SCENARIO ###
title = "Simulation times across scenarios by complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean simulation time"

# Calculate means, CIs
simtime.scenario.means = sim.data %>%
  group_by(scenario, containment, complexity) %>%
  summarize(means = mean(tsims.ms),
            trials = n(),
            se.lower = means - sqrt(var(tsims.ms) / length(tsims.ms)),
            se.upper = means + sqrt(var(tsims.ms) / length(tsims.ms))) %>%
  select(scenario, containment, complexity, means, trials, se.lower, se.upper)
# Graph data
scenario.plot(simtime.scenario.means, title, xlab, ylab)


### BOUNCES PER SCENARIO ###
title = "Bounces across scenarios by complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean bounces"

# Calculate means, CIs
bounce.scenario.means = sim.data %>%
  group_by(scenario, containment, complexity) %>%
  summarize(means = mean(bounces),
            trials = n(),
            se.lower = means - sqrt(var(bounces) / length(bounces)),
            se.upper = means + sqrt(var(bounces) / length(bounces))) %>%
  select(scenario, containment, complexity, means, trials, se.lower, se.upper)
# Graph data
scenario.plot(bounce.scenario.means, title, xlab, ylab)



############################
### TERMINATION ANALYSES ###
############################

containment_complexity_labels = c(
  "l1-l1" = "low containment, no complexity",
  "l1-l2" = "low containment, low complexity",
  "l1-l3" = "low containment, medium complexity",
  "l1-l4" = "low containment, high complexity",
  "l2-l1" = "medium containment, no complexity",
  "l2-l2" = "medium containment, low complexity",
  "l2-l3" = "medium containment, medium complexity",
  "l2-l4" = "medium containment, high complexity",
  "l3-l1" = "high containment, no complexity",
  "l3-l2" = "high containment, low complexity",
  "l3-l3" = "high containment, medium complexity",
  "l3-l4" = "high containment, high complexity"
)

complexity_containment_labels = c(
  "l1-l1" = "no complexity, low containment",
  "l1-l2" = "no complexity, medium containment",
  "l1-l3" = "no complexity, high containment",
  "l2-l1" = "low complexity, low containment",
  "l2-l2" = "low complexity, medium containment",
  "l2-l3" = "low complexity, high containment",
  "l3-l1" = "medium complexity, low containment",
  "l3-l2" = "medium complexity, medium containment",
  "l3-l3" = "medium complexity, high containment",
  "l4-l1" = "high complexity, low containment",
  "l4-l2" = "high complexity, medium containment",
  "l4-l3" = "high complexity, high containment"
)

complexity_labels_verbose = c(
  l1 = "no complexity",
  l2 = "low complexity",
  l3 = "medium complexity",
  l4 = "high complexity"
)

termination.theme = theme(
  # titles
  axis.title.y = element_text(face = "bold", size = 24),
  axis.title.x = element_text(face = "bold", size = 24),
  # axis text
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
  # facet text
  strip.text = element_text(face = "bold", size = 18),
  legend.text = element_text(size = 14),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line(color = "black"),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),
  legend.position = "bottom"
)

term_data = read.data(TERMINATION_DATA_FILEPATH)

summary_data = data.frame(containment = character(),
                          complexity = character(),
                          containment_complexity = character(),
                          complexity_containment = character(),
                          threshold = numeric(),
                          ntrials = numeric(),
                          termination_trials = numeric(),
                          termination_percent = numeric())

for (threshold in seq(1:60)) {
  summary_data = rbind(summary_data,
                       term_data %>%
                         group_by(containment, complexity) %>%
                         summarize(
                           containment_complexity = paste(unique(containment), unique(complexity), sep = "-"),
                           complexity_containment = paste(unique(complexity), unique(containment), sep = "-"),
                           threshold = threshold,
                           ntrials = n(),
                           termination_trials = sum(simulation_time <= threshold),
                           termination_percent = termination_trials / ntrials) %>%
                         as.data.frame()
  )
}


summary_data %>%
  ggplot(aes(x = threshold, y = termination_percent, color = containment_complexity)) +
  geom_line() +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = containment_complexity_labels) +
  guides(color = guide_legend(ncol = 3)) +
  labs(x = "Termination threshold (sec)", y = "Percent of trials") +
  termination.theme

summary_data %>%
  ggplot(aes(x = threshold, y = termination_percent, color = containment_complexity)) +
  geom_line() +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = containment_complexity_labels) +
  labs(x = "Termination threshold (sec)", y = "Percent of trials") +
  facet_wrap(. ~ containment,
             scales = "free",
             labeller = labeller(containment = CONTAINMENT_LABELS)) +
  guides(color = guide_legend(ncol = 3)) +
  termination.theme

summary_data %>%
  ggplot(aes(x = threshold, y = termination_percent, color = complexity_containment)) +
  geom_line() +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = complexity_containment_labels) +
  labs(x = "Termination threshold (sec)", y = "Percent of trials") +
  facet_wrap(. ~ complexity,
             scales = "free",
             labeller = labeller(complexity = complexity_labels_verbose)) +
  guides(color = guide_legend(ncol = 4)) +
  termination.theme





