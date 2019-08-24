# Script for reading in phystables_env data and processing/analyzing
rm(list=ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/phystables_env/")

library(tidyverse)

data = read_csv("phystables_env_raw.csv")
glimpse(data)
length(levels(as.factor(data$subjID)))


########################################
### SANITY CHECKS: DATA COMPLETENESS ###
########################################

# Handle response time for "no response" rows by setting to 10s
data.no.resp = data %>%
  filter(responsetime == -1)
table(data.no.resp$subjID)

data$responsetime[data$responsetime == -1] = 10 * 1000

# Handle 0s responsetime trials by setting to 1s
data.immediate.resp = data %>%
  filter(responsetime == 0)
table(data.immediate.resp$subjID)

data$responsetime[data$responsetime == 0] = 1


# Do we have 256 observations for each participant?
# Do we have equal sc1, sc2, sc3, and sc4 for each participant?
# Do we have equal containment l1, l2, l3 for each participant?
# Do we have equal complexity l1, l2, l3, l4 for each participant?




##################
### PROCESSING ###
##################

# Add columns for scenario, complexity level, containment level, and rotation (incl. "distractor")
scenario.split = with(data, strsplit(trialname, "_"))
data$scenario = unlist(lapply(scenario.split, "[", 2))
data$containment = unlist(lapply(scenario.split, "[", 4))
data$complexity = unlist(lapply(scenario.split, "[", 6))
data$rotation = unlist(lapply(scenario.split, "[", 7))
data$rotation[is.na(data$rotation)] = "NONE" # avoid NAs in this column


# Add column for (log) response time
data = data %>%
  group_by(scenario, subjID) %>%
  mutate(log.responsetime = log10(responsetime)) # log of unique response time - the subject, scenario mean

# Add column for whether selection was correct
data = data %>%
  mutate(correct = trialtarget == usertarget)


glimpse(data)



###################################
### SANITY CHECKS: DATA QUALITY ### 
###################################


### KEYBOARD GUESSES ###
# barplot of each participant's "r" and "g" guess count
# -> should not reflect highly disproportionate "r" or "g" guesses by any person
data %>%
  ggplot(aes(x = subjID, fill = usertarget)) +
  geom_bar(position = "fill", width = 0.75) +
  theme(legend.position = "bottom", axis.text.x = element_blank()) +
  labs(x = "Participant", y = "Response proportion (count 256)", fill = "Trial guess")


### ACCURACY ###
# participant count of correct guesses
# -> should not be so low that it reflects a lack of understanding of the task
# NB: this will also show up in the jitter plot of scores above
data %>%
  ggplot(aes(x = subjID, fill = correct)) +
  geom_bar(position = "fill", width = 0.75) +
  theme(legend.position = "bottom", axis.text.x = element_blank()) +
  labs(x = "Participant", y = "Response proportion (count 256)", fill = "Trial correct")

# TODO should we eliminate the participant that has < 0.5 accuracy?
low.accuracy = data %>%
  group_by(subjID) %>%
  summarize(correct.count = sum(correct),
            total.count = length(correct),
            correct.pct = correct.count / total.count) %>%
  filter(correct.pct < 0.5)
low.accuracy

### SCORES ###
# participant trial scores
# -> should be reasonably clustered 
# -> should NOT have a high number of 0s (no guess)
data %>%
  ggplot(aes(x = subjID, y = score)) +
  theme(axis.text.x = element_blank()) +
  geom_jitter(alpha = 0.5, size = 0.8, width = 0.25, color = "blue") +
  labs(x = "Participant", y = "Trial scores (max 120)")
# TODO is there a better way to display this?


### RESPONSE TIMES ###

# distribution of participant log response times
# -> should be roughly normal, not have too many outliers at low or high end
data %>%
  ggplot(aes(x = log.responsetime)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = "Response time (log10 ms)", y = "Trial Count")

# distribution of participant raw response times
data %>%
  ggplot(aes(x = responsetime)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Response time", y = "Trial Count")

# boxplot for each participant's log responsetime
# -> should be > 0
data %>%
  ggplot(aes(x = subjID, y = log.responsetime)) +
  geom_jitter(width = 0.25, alpha = 0.5, color = "blue") +
  # geom_boxplot(width = 0.5, alpha = 0.5) +
  theme(axis.text.x = element_blank()) +
  labs(x = "Participant", y = "Response time (log10 ms)")

# participant log response time by trial index
# -> should have no lines that are close to 0 across a large number of trials
data %>%
  ggplot(aes(x = trialindex, y = log.responsetime, color = subjID)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(x = "Trial Number (1 - 256)", y = "Response time (log10 ms)", color = "Participant")

# TODO are people learning??
data %>%
  ggplot(aes(x = trialindex, y = log.responsetime, color = subjID)) +
  geom_smooth(method = 'lm', se = FALSE) +
  theme(legend.position = "none") +
  labs(x = "Trial Number (1 - 256)", y = "Response time (log10 ms)", color = "Participant")



##########################
### ANALYSIS FUNCTIONS ###
##########################

containment_labels = c(
  l1 = "low containment",
  l2 = "medium containment",
  l3 = "high containment"
)

complexity_labels = c(
  l1 = "none",
  l2 = "low",
  l3 = "medium",
  l4 = "high"
)

scenario_labels = c(
  sc1 = "scenario 1",
  sc2 = "scenario 2",
  sc3 = "scenario 3",
  sc4 = "scenario 4"
)

make.canonical.bargraph = function(df.means, title, xlab, ylab) {
  df.means %>%
    ggplot(aes(x = complexity, y = means)) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_x_discrete(labels = complexity_labels) +
    geom_errorbar(mapping = aes(ymin = se.lower, ymax = se.upper), width = 0.2) +
    facet_wrap(. ~ containment,
               scales = "free_x",
               labeller = labeller(containment = containment_labels)) +
    labs(x = xlab, y = ylab) +
    ggtitle(title)
}

make.canonical.bargraph.scenario = function(df.means, title, xlab, ylab) {
  df.means %>%
    ggplot(aes(x = complexity, y = means, color = scenario)) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_x_discrete(labels = complexity_labels) +
    guides(color = FALSE) +
    geom_errorbar(mapping = aes(ymin = se.lower, ymax = se.upper), width = 0.2) +
    facet_grid(scenario ~ containment,
               scales = "free_x",
               labeller = labeller(containment = containment_labels,
                                   scenario = scenario_labels)) +
    labs(x = xlab, y = ylab) +
    ggtitle(title)
}

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
  axis.line = element_line(color = "black")
)


#################################
### ANALYSIS: SIMULATION TIME ###
#################################
### Mean time for the ball to execute rest of path after participant answer ###
title = "Ball movement time across complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean movement time (ms)"

# Calculate means, CIs
simulation.time.means = data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(simtime),
            trials = n(),
            se.lower = means - sqrt(var(simtime) / length(simtime)),
            se.upper = means + sqrt(var(simtime) / length(simtime))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
make.canonical.bargraph(simulation.time.means, title, xlab, ylab)

# Graph as dot with errors
simulation.time.means %>%
  ggplot(aes(x = complexity, y = means)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  scale_x_discrete(labels = complexity_labels) +
  facet_wrap(. ~ containment,
             scales = "free",
             labeller = labeller(containment = containment_labels)) +
  #scale_y_continuous(limits = c(250, 750), breaks = seq(250, 750, by = 250)) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  default.theme



###############################
### ANALYSIS: RESPONSE TIME ###
###############################

# Add columns for response time scaled by mean response time for that participant, scenario
data = data %>%
  filter(rotation %in% c("NONE", "LEFT", "RIGHT", "TWICE")) %>%
  group_by(scenario, subjID) %>%
  mutate(trials = n(),
         mean.scenario.subj.responsetime = mean(responsetime),
         log.mean.scenario.subj.responsetime = mean(log.responsetime),
         scaled.responsetime = responsetime - mean.scenario.subj.responsetime,
         log.scaled.responsetime = log.responsetime - log.mean.scenario.subj.responsetime)

# Sanity check scaled response time: this should be 0
data %>%
  group_by(scenario, subjID) %>%
  summarize(sum.scaled.responsetime = sum(scaled.responsetime),
            sum.log.scaled.responsetime = sum(log.scaled.responsetime)) %>%
  summarize(scaled.responsetime.check = max(sum.scaled.responsetime),
            log.scaled.responsetime.check = max(sum.scaled.responsetime))



### MEAN RESPONSETIME ###
title = "Response time across complexity, containment levels"
xlab = "Simulation complexity"
ylab = "Mean response time (ms)"

# Calculate means, CIs
responsetime.means = data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(responsetime),
            trials = n(),
            se.lower = means - sqrt(var(responsetime) / length(responsetime)),
            se.upper = means + sqrt(var(responsetime) / length(responsetime))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
make.canonical.bargraph(responsetime.means, title, xlab, ylab)

# Graph as dot with errors
responsetime.means %>%
  ggplot(aes(x = complexity, y = means)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  scale_x_discrete(labels = complexity_labels) +
  facet_wrap(. ~ containment,
             scales = "free",
             labeller = labeller(containment = containment_labels)) +
  scale_y_continuous(limits = c(250, 750), breaks = seq(250, 750, by = 250)) +
  labs(x = xlab, y = ylab) +
  #ggtitle(title) +
  default.theme


### MEAN LOG RESPONSETIME ###
title = "Log response time across complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean response time"

# Calculate means, CIs
log.responsetime.means = data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(log.responsetime),
            trials = n(),
            se.lower = means - sqrt(var(log.responsetime) / length(log.responsetime)),
            se.upper = means + sqrt(var(log.responsetime) / length(log.responsetime))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
# response time dot plot (no canonical function for this because the scale_y is custom)
log.responsetime.means %>%
  ggplot(aes(x = complexity, y = means, ymin = se.lower, ymax = se.upper)) +
  geom_pointrange(size = 1) +
  scale_x_discrete(labels = complexity_labels) +
  facet_wrap(. ~ containment,
             scales = "free",
             labeller = labeller(containment = containment_labels)) +
  scale_y_continuous(limits = c(2.3, 2.6), breaks = seq(2.3, 2.6, by = 0.1)) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  default.theme



#### MEAN SCALED RESPONSETIME ###
title = "Response time scaled by scenario mean across complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean scaled response time (ms)"

# Calculate means, CIs
scaled.responsetime.means = data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(scaled.responsetime),
            trials = n(),
            se.lower = means - sqrt(var(scaled.responsetime) / length(scaled.responsetime)),
            se.upper = means + sqrt(var(scaled.responsetime) / length(scaled.responsetime))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
make.canonical.bargraph(scaled.responsetime.means, title, xlab, ylab)


#### MEAN SCALED LOG RESPONSETIME ###
title = "Log response time scaled by scenario mean across complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean scaled log response time (log10 ms)"

# Calculate means, CIs
log.scaled.responsetime.means = data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(log.scaled.responsetime),
            trials = n(),
            se.lower = means - sqrt(var(log.scaled.responsetime) / length(log.scaled.responsetime)),
            se.upper = means + sqrt(var(log.scaled.responsetime) / length(log.scaled.responsetime))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
make.canonical.bargraph(log.scaled.responsetime.means, title, xlab, ylab)


##########################################
### ANALYSIS: RESPONSE TIME / SCENARIO ###
##########################################

### MEAN RESPONSETIME BY SCENARIO ###
title = "Response time across scenarios by complexity, containment levels"
xlab = "Simulation complexity"
ylab = "Mean response time (ms)"

# Calculate means, CIs
responsetime.scenario.means = data %>%
  group_by(scenario, containment, complexity) %>%
  summarize(means = mean(responsetime),
            trials = n(),
            se.lower = means - sqrt(var(responsetime) / length(responsetime)),
            se.upper = means + sqrt(var(responsetime) / length(responsetime))) %>%
  select(scenario, containment, complexity, means, trials, se.lower, se.upper)
# Graph data
make.canonical.bargraph.scenario(responsetime.scenario.means, title, xlab, ylab)

# Graph as points with errorbar
scen.theme = default.theme
scen.theme$panel.border = element_rect(colour = "black", fill = NA, size = 1)

responsetime.scenario.means %>%
  ggplot(aes(x = complexity, y = means, color = scenario)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.5) +
  scale_x_discrete(labels = complexity_labels) +
  guides(color = FALSE) +
  scale_y_continuous(limits = c(300, 900), breaks = seq(300, 900, by = 200)) +
  facet_grid(scenario ~ containment,
             scales = "free",
             labeller = labeller(containment = containment_labels,
                                 scenario = scenario_labels)) +
  labs(x = xlab, y = ylab) +
  #ggtitle(title) +
  scen.theme


### LOG RESPONSETIME BY SCENARIO ###
title = "Log response time across scenarios"
xlab = "Complexity level"
ylab = "Mean response time"

scen.theme = default.theme
scen.theme$panel.border = element_rect(colour = "black", fill = NA, size = 1)
# Calculate means, CIs
log.responsetime.scenario.means = data %>%
  group_by(scenario, containment, complexity) %>%
  summarize(means = mean(log.responsetime),
            trials = n(),
            se.lower = means - sqrt(var(log.responsetime) / length(log.responsetime)),
            se.upper = means + sqrt(var(log.responsetime) / length(log.responsetime))) %>%
  select(scenario, containment, complexity, means, trials, se.lower, se.upper)
# Graph data
# dot plot of log response time by scenario
# no canonical function because scale_y is custom
log.responsetime.scenario.means %>%
  ggplot(aes(x = complexity, y = means, ymin = se.lower, ymax = se.upper, color = scenario)) +
  geom_pointrange() +
  scale_x_discrete(labels = complexity_labels) +
  guides(color = FALSE) +
  scale_y_continuous(limits = c(2.3, 2.7), breaks = seq(2.3, 2.7, by = 0.1)) +
  facet_grid(scenario ~ containment,
             scales = "free",
             labeller = labeller(containment = containment_labels,
                                 scenario = scenario_labels)) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  scen.theme




#######################
### ANALYSIS: SCORE ###
#######################

#### MEAN SCORE ###
title = "Trial scores across complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean score"

# Calculate means, CIs
score.means = data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(score),
            trials = n(),
            se.lower = means - sqrt(var(score) / length(score)),
            se.upper = means + sqrt(var(score) / length(score))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
make.canonical.bargraph(score.means, title, xlab, ylab)


### MEAN SCORE BY SCENARIO ###
title = "Trial scores across scenarios by complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean trial score"

# Calculate means, CIs
score.scenario.means = data %>%
  group_by(scenario, containment, complexity) %>%
  summarize(means = mean(score),
            trials = n(),
            se.lower = means - sqrt(var(score) / length(score)),
            se.upper = means + sqrt(var(score) / length(score))) %>%
  select(scenario, containment, complexity, means, trials, se.lower, se.upper)
# Graph data
make.canonical.bargraph.scenario(score.scenario.means, title, xlab, ylab)


##########################
### ANALYSIS: ACCURACY ###
##########################

### MEAN ACCURACY ###
title = "Trial accuracy across complexity, containment levels"
xlab = "Simulation complexity"
ylab = "Mean proportion correct"

# Calculate participant accuracy for each containment/complexity level
participant.accuracy.means = data %>%
  group_by(subjID, containment, complexity) %>%
  summarize(right.answers = sum(correct),
            total.answers = n(),
            accuracy = right.answers / total.answers)

# Calculate means, CIs
accuracy.means = participant.accuracy.means %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(accuracy),
            trials = n(),
            se.lower = means - sqrt(var(accuracy) / length(accuracy)),
            se.upper = means + sqrt(var(accuracy) / length(accuracy)),
            ci.lower = t.test(accuracy)$conf.int[1],
            ci.upper = t.test(accuracy)$conf.int[2]) %>%
  select(containment, complexity, means, trials, se.lower, se.upper, ci.lower, ci.upper)
# Graph data
# accuracy dotplot (no general expression for this because scale_y is unique)
accuracy.means %>%
  ggplot(aes(x = complexity, y = means, ymin = se.lower, ymax = se.upper)) +
  geom_pointrange(size = 1) +
  scale_x_discrete(labels = complexity_labels) +
  facet_wrap(. ~ containment,
             scales = "free",
             labeller = labeller(containment = containment_labels)) +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  default.theme

# Graph with standard error bars
accuracy.means %>%
  ggplot(aes(x = complexity, y = means)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  scale_x_discrete(labels = complexity_labels) +
  geom_hline(aes(yintercept = 0.5), linetype = "dashed") +
  facet_wrap(. ~ containment,
             scales = "free",
             labeller = labeller(containment = containment_labels)) +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = xlab, y = ylab) +
  #ggtitle(title) +
  default.theme


### MEAN ACCURACY BY SCENARIO ###
title = "Trial accuracy across scenarios by complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean proportion correct"

# Calculate participant accuracy in each scenario by containment, complexity level
participant.scenario.accuracy.means = data %>%
  group_by(subjID, scenario, containment, complexity) %>%
  summarize(right.answers = sum(correct),
            total.answers = n(),
            accuracy = right.answers / total.answers)

# Calculate means, CIs
scenario.accuracy.means = participant.scenario.accuracy.means %>%
  group_by(scenario, containment, complexity) %>%
  summarize(means = mean(accuracy),
            trials = n(),
            se.lower = means - sqrt(var(accuracy) / length(accuracy)),
            se.upper = means + sqrt(var(accuracy) / length(accuracy))) %>%
  select(scenario, containment, complexity, means, trials, se.lower, se.upper)
# Graph data
make.canonical.bargraph.scenario(scenario.accuracy.means, title, xlab, ylab)


#########################
### ANALYSIS: BOUNCES ###
#########################

title = "Number of bounces across complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean number of bounces"

bounces.means = data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(numbounces),
            trials = n(),
            se.lower = means - sqrt(var(numbounces) / length(numbounces)),
            se.upper = means + sqrt(var(numbounces) / length(numbounces))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
make.canonical.bargraph(bounces.means, "Bounces", xlab, "Number of Bounces")

bounces.means %>%
  ggplot(aes(x = complexity, y = means)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  scale_x_discrete(labels = complexity_labels) +
  facet_wrap(. ~ containment,
             scales = "free",
             labeller = labeller(containment = containment_labels)) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  default.theme


###########################
### STATISTICS!!!!!!!!! ###
###########################

data = data %>%
  mutate(log.simtime = log10(simtime))

data %>%
  ggplot(aes(x = log.simtime, y = log.responsetime)) +
  geom_point()

data %>%
  ggplot(aes(x = log.simtime, y = log.scaled.responsetime)) +
  geom_point()

cor.test(data$log.simtime, data$log.responsetime)
cor.test(data$log.simtime, data$log.scaled.responsetime)

glimpse(data)



### ANOVAS ###

# This takes ~ 2s
model.scenario = with(data, aov(log.responsetime ~ scenario + Error(subjID / scenario)))
summary(model.scenario)
#' Summary: 
#' scenario accounts for significant variance, F(3,234) = 64.89, p < 2e-16

# This takes ~ 10s
model.containment.complexity = with(data, aov(log.responsetime ~ containment * complexity + Error(subjID / (containment * complexity))))
summary(model.containment.complexity)
#' Summary: 
#' containment accounts for significant variance, F(2,156) = 55.63, p < 2e-16
#' complexity accounts for significant variance, F(3,234) = 8.87, p = 1.37e-05
#' interaction is not significant, F(6,468) = 0.487, p = 0.818

# This takes 3-4 mins
model.containment.complexity.rotation = with(data, aov(log.responsetime ~ containment * complexity * rotation + Error(subjID / (containment * complexity * rotation))))
summary(model.containment.complexity.rotation)
#' Summary:
#' containment accounts for significant variance, F(2,156) = 55.63, p < 2e-16
#' complexity accounts for significant variance, F(3,234) = 8.87, p = 1.37e-05
#' rotation accounts for significant variance, F(3,234) = 6.995, p = 0.000159
#' None of the interactions are significant (containment:complexity, containment:rotation, complexity:rotation, containment:complexity:rotation)

# This takes 3-4 mins
model.containment.complexity.scenario = with(data, aov(log.responsetime ~ scenario * containment * complexity + Error(subjID / (scenario * containment * complexity))))
summary(model.containment.complexity.scenario)
#' Summary:
#' containment accounts for significant variance, F(2,156) = 55.63, p < 2e-16
#' complexity accounts for significant variance, F(3,234) = 8.87, p = 1.37e-05
#' scenario accounts for significant variance, F(3,234) = 64.89, p < 2e-16
#' containment:complexity interaction is not significant, F(6,468) = 0.487, p = 0.818
#' containment:scenario interaction is highly significant, F(6,468) = 6.935, p = 4.49e-07
#' complexity:scenario interaction is highly significant, F(9,702) = 4.662, p = 5.11e-06
#' containment:complexity:scenario interaction is significant, F(18,1404) = 1.626, p = 0.0467


# This takes 4-5 mins
model.full.additive = with(data, aov(log.responsetime ~ containment * complexity * scenario + rotation + Error(subjID / (containment * complexity * scenario + rotation))))
summary(model.full.additive)
# Same as above


# Make sure there's nothing going on with containment/complexity and rotation
model.containment.complexity.rotation = with(data, aov(log.responsetime ~ containment * complexity * rotation + Error(subjID / (containment * complexity * rotation))))
summary(model.containment.complexity.rotation)
#' Summary: there isn't (interaction terms not significant)


# Make sure there's nothing going on with scenario:rotation
# This takes < 1 min
model.scenario.rotation = with(data, aov(log.responsetime ~ scenario * rotation + Error(subjID / (scenario * rotation))))
summary(model.scenario.rotation)
#' scenario and rotation are highly significant (see above)
#' scenario:rotation interaction is significant, F(9,702) = 2.939, p = 0.00197



### ACCURACY ###

accuracy.containment.complexity = with(participant.accuracy.means, aov(accuracy ~ containment * complexity + Error(subjID / (containment * complexity))))
summary(accuracy.containment.complexity)
#' Summary: 
#' containment accounts for significant variance, F(2,156) = 829.5, p < 2e-16
#' complexity accounts for significant variance, F(3,234) = 15.7, p = 2.46e-09
#' interaction is not significant, F(6,468) = 61.79, p <2e-16



# For some reason this breaks everything, too slow
model.basic = with(data, aov(log.responsetime ~ scenario * containment * complexity * rotation + 
                 Error(subjID / (scenario * containment * complexity * rotation))))







#######################
### SIMULATION DATA ###
#######################


# TODO turn up the noise parameter on the ball's velocity in the simulator?


sim.data = read_csv("containment_sims.csv")
glimpse(sim.data)

table(sim.data$trialname)

# add scenario, complexity, containment rotation
scenario.split = with(sim.data, strsplit(trialname, "_"))
sim.data$scenario = unlist(lapply(scenario.split, "[", 2))
sim.data$containment = unlist(lapply(scenario.split, "[", 4))
sim.data$complexity = unlist(lapply(scenario.split, "[", 6))
sim.data$rotation = unlist(lapply(scenario.split, "[", 7))
sim.data$rotation[is.na(sim.data$rotation)] = "NONE" # avoid NAs in this column

# check new columns
glimpse(sim.data)
table(sim.data$rotation)

# convert trial sims to milliseconds, add log trial sims
sim.data$tsims.ms = sim.data$tsims * 1000
sim.data$log.tsims.ms = log10(sim.data$tsims.ms)

dim(sim.data)
# Remove non-terminated trials
sim.data = sim.data %>%
  filter(outcome %in% c("REDGOAL", "GREENGOAL"))
dim(sim.data) # removes about 900 rows w/ 50s max, 12k rows with 10s max


#### MEAN SIMTIME ###
title = "Simulation times across complexity, containment levels"
xlab = "Complexity level"
ylab = "Mean simulation time (ms)"

# Calculate means, CIs
simtime.means = sim.data %>%
  group_by(containment, complexity) %>%
  summarize(means = mean(tsims.ms),
            trials = n(),
            se.lower = means - sqrt(var(tsims.ms) / length(tsims.ms)),
            se.upper = means + sqrt(var(tsims.ms) / length(tsims.ms))) %>%
  select(containment, complexity, means, trials, se.lower, se.upper)
# Graph data
make.canonical.bargraph(simtime.means, title, xlab, ylab)

simtime.means %>%
  ggplot(aes(x = complexity, y = means)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  scale_x_discrete(labels = complexity_labels) +
  facet_wrap(. ~ containment,
             scales = "free",
             labeller = labeller(containment = containment_labels)) +
  #scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  default.theme

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
log.simtime.means %>%
  ggplot(aes(x = complexity, y = means, ymin = se.lower, ymax = se.upper)) +
  geom_pointrange() +
  scale_x_discrete(labels = complexity_labels) +
  facet_wrap(. ~ containment,
             scales = "free_x",
             labeller = labeller(containment = containment_labels)) +
  labs(x = xlab, y = ylab) +
  ggtitle(title)


sim.data %>%
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
make.canonical.bargraph(bounce.means, title, xlab, ylab)



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
make.canonical.bargraph.scenario(simtime.scenario.means, title, xlab, ylab)


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
make.canonical.bargraph.scenario(bounce.scenario.means, title, xlab, ylab)



#################################
### ANALYSIS: STRATEGY CHANGE ###
#################################

#' First, we look at responsetime and accuracy by quartile across *all* containment/complexity levels
#' 1. Response time drops precipitously in the first quartile and stays low
#' 2. Accuracy stays very consistent across all quartiles so participants were at best more *efficient* overall


### RESPONSETIME ###

# Responsetime by quartile, all trials
trial.order.qtrs = data %>%
  mutate(trial.quartile = ceiling(trialindex / 64)) %>%
  group_by(trial.quartile) %>%
  summarize(means = mean(responsetime),
            trials = n(),
            se.lower = means - sqrt(var(responsetime) / length(responsetime)),
            se.upper = means + sqrt(var(responsetime) / length(responsetime))) %>%
  select(trial.quartile, means, trials, se.lower, se.upper)

title = "Response time by trial order quartile"
xlab = "Quartile of trial order"
ylab = "Mean response time (ms)"

trial.order.qtrs %>%
  ggplot(aes(x = trial.quartile, y = means)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  default.theme


# Responsetime by trial index, all trials
trial.order.means = data %>%
  group_by(trialindex) %>%
  summarize(means = mean(responsetime),
            trials = n(),
            se.lower = means - sqrt(var(responsetime) / length(responsetime)),
            se.upper = means + sqrt(var(responsetime) / length(responsetime))) %>%
  select(trialindex, means, trials, se.lower, se.upper)

title = "Response time by trial index"
xlab = "Trial index"
ylab = "Mean response time (ms)"

trial.order.means %>%
  ggplot(aes(x = trialindex, y = means)) +
  geom_point() +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  default.theme


### ACCURACY ###

# Accuracy by subject x quartile, all trials
trialindex.accuracy.qtrs = data %>%
  mutate(trial.quartile = ceiling(trialindex / 64)) %>%
  group_by(trial.quartile, subjID) %>%
  summarize(right.answers = sum(correct),
            total.answers = n(),
            accuracy = right.answers / total.answers)

trialindex.accuracy.summary.qtrs = trialindex.accuracy.qtrs %>%
  group_by(trial.quartile) %>%
  summarize(means = mean(accuracy),
            trials = n(),
            se.lower = means - sqrt(var(accuracy) / length(accuracy)),
            se.upper = means + sqrt(var(accuracy) / length(accuracy))) %>%
  select(trial.quartile, means, trials, se.lower, se.upper)

title = "Participant accuracy by trial order quartile"
xlab = "Quartile of trial order"
ylab = "Percent correct"

trialindex.accuracy.summary.qtrs %>%
  ggplot(aes(x = trial.quartile, y = means)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  scale_y_continuous(limits = c(0.5, 1.0)) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  default.theme


# Accuracy by trialindex, all trials
trialindex.accuracy.means = data %>%
  group_by(trialindex) %>%
  summarize(right.answers = sum(correct),
            total.answers = n(),
            accuracy = right.answers / total.answers)

title = "Accuracy by trial index"
xlab = "Trial index"
ylab = "Percent correct"

trialindex.accuracy.means %>%
  ggplot(aes(x = trialindex, y = accuracy)) +
  geom_point() +
  labs(x = xlab, y = ylab) +
  scale_y_continuous(limits = c(0.5, 1.0)) +
  ggtitle(title) +
  default.theme


#' Next, we look at responsetime and accuracy by quartile for *only* high containment, high complexity trials
#' 1. Unlike across all trials, response time drops more gradually, looking very linear by quartiles
#' 2. Accuracy decreases slightly from 0.575 to 0.525
#' 



### PROCESS DATA ###
# Check data
glimpse(data)
levels(as.factor(data$complexity))
levels(as.factor(data$containment))

# Select just high complexity high containmnet trials
complex.trials = data %>%
  filter(complexity == "l4", containment == "l3")
glimpse(complex.trials) # drops down to 1264 observations
table(complex.trials$subjID) # 16 high complexity/high containment trials per subject (4 scenario x 4 rotation)

# Split into ordinal quartiles
complex.trials = complex.trials %>%
  group_by(subjID) %>%
  mutate(trial.order = rank(trialindex), # get ordinal rank of each high comp. high cont. trial
         trial.quartile = ceiling(trial.order / 4)) # get quartile for each trial

# Check that it works
complex.trials %>%
  filter(subjID == "user_1548457510128") %>% # "user_1548179758602"
  select(trialname, trialindex, trial.order, trial.quartile)


### RESPONSETIME ### 

# Mean responsetime by quartile
title = "High containment, high complexity response time"
xlab = "Quartile of trial order"
ylab = "Mean response time (ms)"

complex.responsetime.qtrs = complex.trials %>%
  group_by(trial.quartile) %>%
  summarize(means = mean(responsetime),
            trials = n(),
            se.lower = means - sqrt(var(responsetime) / length(responsetime)),
            se.upper = means + sqrt(var(responsetime) / length(responsetime))) %>%
  select(trial.quartile, means, trials, se.lower, se.upper)

complex.responsetime.qtrs %>%
  ggplot(aes(x = trial.quartile, y = means)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  default.theme


# Mean responsetime by trial order (more continuous than quartile)
xlab = "Trial order"

complex.responsetime.means = complex.trials %>%
  group_by(trial.order) %>%
  summarize(means = mean(responsetime),
            trials = n(),
            se.lower = means - sqrt(var(responsetime) / length(responsetime)),
            se.upper = means + sqrt(var(responsetime) / length(responsetime))) %>%
  select(trial.order, means, trials, se.lower, se.upper)

complex.responsetime.means %>%
  ggplot(aes(x = trial.order, y = means)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  labs(x = xlab, y = ylab) +
  ggtitle(title) +
  default.theme


### ACCURACY ### 

# Accuracy by subject x quartile
complex.accuracy.qtrs = complex.trials %>%
  group_by(trial.quartile, subjID) %>%
  summarize(right.answers = sum(correct),
            total.answers = n(),
            accuracy = right.answers / total.answers)

complex.accuracy.summary.qtrs = complex.accuracy.qtrs %>%
  group_by(trial.quartile) %>%
  summarize(means = mean(accuracy),
            trials = n(),
            se.lower = means - sqrt(var(accuracy) / length(accuracy)),
            se.upper = means + sqrt(var(accuracy) / length(accuracy))) %>%
  select(trial.quartile, means, trials, se.lower, se.upper)


title = "High containment, high complexity accuracy"
xlab = "Quartile of trial order"
ylab = "Mean proportion correct"

complex.accuracy.summary.qtrs %>%
  ggplot(aes(x = trial.quartile, y = means)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = se.lower, ymax = se.upper), width = 0.25) +
  geom_hline(aes(yintercept = 0.5), linetype = "dashed") +
  scale_y_continuous(limits = c(0.4, 0.6)) +
  labs(x = xlab, y = ylab) +
  #ggtitle(title) +
  default.theme


# did anybody figure it out?
complex.accuracy.qtrs %>%
  filter(accuracy == 1)

complex.accuray.qtrs[complex.accuracy.qtrs$accuracy[complex.accuracy.qtrs$trial.quartile == 4] == 1,]

complex.accuracy.qtrs %>%
  filter(accuracy == 1) %>%
  ggplot(aes(x = trial.quartile, y = accuracy, color = subjID)) +
  geom_line(alpha = 0.25) +
  theme(legend.position = "none")


### ANOVAS ###
subset.data.q1 = complex.trials$correct[complex.trials$trial.quartile == 1]
subset.data.q4 = complex.trials$correct[complex.trials$trial.quartile == 4]

binom.test(x = sum(subset.data.q1),
              n = length(subset.data.q1),
              p = 0.5)

binom.test(x = sum(subset.data.q4),
           n = length(subset.data.q4),
           p = 0.5)




model.quartile.resp = with(complex.trials, aov(log.responsetime ~ trial.quartile + Error(subjID / (trial.quartile))))
summary(model.quartile.resp) # trial.quartile explains significant variance in response time

model.quartile.acc = with(complex.accuracy.qtrs, aov(accuracy ~ trial.quartile + Error(subjID / (trial.quartile))))
summary(model.quartile.acc)


### SCRATCHPAD ### 

data %>%
  ggplot(aes(x = responsetime, y = score)) +
  geom_point()


