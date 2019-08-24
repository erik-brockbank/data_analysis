# Script for checking whether participant responses are well explained by easy heuristics
rm(list=ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/phystables_env/")

library(tidyverse)
library(jsonlite)
library(lme4)


# Read in and check data
data = read_csv("phystables_env_raw.csv")


#####################
### DATA CLEANING ###
#####################

# Handle response time for "no response" rows by setting to 10s (max available time)
data$responsetime[data$responsetime == -1] = 10 * 1000

# Handle 0s responsetime trials by setting to 1s
data$responsetime[data$responsetime == 0] = 1


#######################
### DATA PROCESSING ###
#######################

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


# Coerce JSON columns to be json readable
data$goaldistances = chartr("\'\"","\"\'", data$goaldistances)
data$ballstartpos = chartr("\'\"","\"\'", data$ballstartpos)
data$ballwaitpos = chartr("\'\"","\"\'", data$ballwaitpos)

# Parse JSON columns
data = data %>% # NB: this can take ~10s
  mutate(goaldistances = map(goaldistances, ~ fromJSON(.)),
         ballstartpos = map(ballstartpos, ~ fromJSON(.)),
         ballwaitpos = map(ballwaitpos, ~ fromJSON(.)))

#####################
### DATA ANALYSIS ###
#####################

# Strategy below:
# add columns for usertarget == heuristic_x_target, run binomial tests on each of these columns
# Note this doesn't factor in repeated measures structure in the data


# Based on analysis of accuracy in data_processing, focus on high containment, high complexity trials
data = data %>%
  filter(complexity == "l4", containment == "l3")


# 1. Closest target border to point where ball paused
# Note if e.g. ball is directly below a target, distance used here is distance to bottom of target, not Euc. distance to center
data = data %>%
  mutate(guess_closest_Dist = usertarget == goaldistances[[1]]$name[goaldistances[[1]]$goaldist == min(goaldistances[[1]]$goaldist)])

length(data$guess_closest_Dist)
sum(data$guess_closest_Dist) # looks to be about 50%
binom.test(sum(data$guess_closest_Dist), length(data$guess_closest_Dist), p = 0.5)


# 1.5. Check that results for Euclidean dist. to center of target are the same as above
for (i in seq(1, length(data$goaldistances))) {
  data$goaldistances[[i]] = data$goaldistances[[i]] %>%
    mutate(center.x = goal$left + (goal$right - goal$left) / 2,
           center.y = goal$top + (goal$bottom - goal$top) / 2,
           ball.start.Euc = sqrt((data$ballstartpos[[i]]$x - center.x)^2 + (data$ballstartpos[[i]]$y - center.y)^2),
           ball.wait.Euc = sqrt((data$ballwaitpos[[i]]$x - center.x)^2 + (data$ballwaitpos[[i]]$y - center.y)^2),
           ball.dir.Euc = ball.wait.Euc - ball.start.Euc)
}

data = data %>%
  mutate(guess_closest_Euc = usertarget == goaldistances[[1]]$name[goaldistances[[1]]$ball.wait.Euc == min(goaldistances[[1]]$ball.wait.Euc)])

length(data$guess_closest_Euc)
sum(data$guess_closest_Euc) # Same numbers as above
binom.test(sum(data$guess_closest_Euc), length(data$guess_closest_Euc), p = 0.5)


# 2. Target in direction of ball movement
data = data %>%
  mutate(guess_ball_dir = usertarget == goaldistances[[1]]$name[goaldistances[[1]]$ball.dir.Euc == min(goaldistances[[1]]$ball.dir.Euc)])

length(data$guess_ball_dir)
sum(data$guess_ball_dir)
binom.test(sum(data$guess_ball_dir), length(data$guess_ball_dir), p = 0.5) # this is significant but 700 doesn't seem *that* high



# TODO how to include repeated measures aspect?
# linear mixed effects model with guess on each trial, closest target on each trial, ball direction target on each trial

data = data %>% 
  mutate(picks_red = usertarget == "red",
         red_closest_Euc = goaldistances[[1]]$name[goaldistances[[1]]$ball.wait.Euc == min(goaldistances[[1]]$ball.wait.Euc)] == "red",
         red_ball_dir = goaldistances[[1]]$name[goaldistances[[1]]$ball.dir.Euc == min(goaldistances[[1]]$ball.dir.Euc)] == "red")

# TODO singular fit a problem here?
model.basic = glmer(data = data,
                    family = binomial(),
                    picks_red ~ 1 +
                     (1 | subjID) +
                     (1 | scenario))
                      #(1 | subjID:scenario))

model.closest.target = glmer(data = data,
                             family = binomial(),
                             picks_red ~ red_closest_Euc +
                               (1 | subjID) +
                               (1 | scenario))
model.ball.dir = glmer(data = data,
                       family = binomial(),
                       picks_red ~ red_ball_dir +
                         (1 | subjID) +
                         (1 | scenario))


anova(model.closest.target, model.basic)
anova(model.ball.dir, model.basic)


