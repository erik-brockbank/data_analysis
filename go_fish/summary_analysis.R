#' 
#' Analysis script for Go Fish web version
#' 


rm(list = ls())
setwd("~/web/vullab/data_analysis/go_fish")

library(tidyverse)
library(viridis)



# GLOBALS ======================================================================

SUMMARY_DATA = "01_go_fish_meta.csv"
TRIAL_DATA = "02_go_fish_trials.csv"
GENERATION_RESP_DATA = "03_go_fish_generation_free_resp.csv"
GENERATION_JUDG_DATA = "04_go_fish_generation_judgment.csv"
EVAL_DATA = "05_go_fish_evaluation.csv"
MEMORY_DATA = "06_go_fish_memory.csv"

CONDITION_LABELS = c("TRUE" = "describe", "FALSE" = "explain")
RULE_EVAL_LABELS = c("TRUE" = "target rule", "FALSE" = "all other rules")



# ANALYSIS FUNCTIONS ===========================================================

read_data = function(filepath) {
  data = read_csv(filepath)
  return(data)
}


get_prediction_summary = function(prediction_data) {
  prediction_summary = prediction_data %>%
    mutate(input_correct = 
             (input_prediction_catches_fish == prediction_catches_fish)) %>%
    group_by(is_control, trial_index) %>%
    summarize(accuracy = sum(input_correct) / n())
  return(prediction_summary)
}


get_generation_judg_summary = function(generation_judg) {
  generation_judg_summary = generation_judg %>%
    group_by(is_control) %>%
    summarize(mean_accuracy = mean(subj_accuracy),
              subjects = n(),
              se_accuracy = sd(subj_accuracy) / sqrt(n()),
              ci_lower = mean_accuracy - se_accuracy,
              ci_upper = mean_accuracy + se_accuracy)
  return(generation_judg_summary)
}


get_eval_summary = function(eval_data) {
  eval_summary_target_rule = eval_data %>%
    filter(is_target_rule == TRUE) %>% # for target rule, summarize across participants
    group_by(is_target_rule, is_control) %>%
    summarize(mean_rating = mean(input_rule_rating),
              subjects = n(),
              se_rating = sd(input_rule_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
  
  eval_summary_other_rules = eval_data %>%
    filter(is_target_rule == FALSE) %>% # for target rule, summarize across participants
    group_by(is_target_rule, is_control, subjID) %>%
    summarize(mean_subj_rating = mean(input_rule_rating),
              rules = n()) %>%
    group_by(is_target_rule, is_control) %>%
    summarize(mean_rating = mean(mean_subj_rating),
              subjects = n(),
              se_rating = sd(mean_subj_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
    
  eval_summary = rbind(eval_summary_target_rule, eval_summary_other_rules)
  return(eval_summary)
}


get_time_summary = function(time_data) {
  time_summary = time_data %>%
    group_by(is_control) %>%
    summarize(mean_task_time = mean(experiment_completion_time),
              subjects = n(),
              se_task_time = sd(experiment_completion_time) / sqrt(n()),
              ci_lower = mean_task_time - se_task_time,
              ci_upper = mean_task_time + se_task_time)
  return(time_summary)
}

get_memory_subj_accuracy = function(memory_data) {
  memory_subj_accuracy = memory_data %>%
    mutate(memory_correct = 
             (memory_shape_in_expt == input_shape_in_expt)) %>%
    group_by(is_control, subjID) %>%
    summarize(subj_accuracy = sum(memory_correct) / n())
  return(memory_subj_accuracy)
}

get_memory_summary = function(memory_subj_accuracy) {
  memory_summary = memory_subj_accuracy %>%
    group_by(is_control) %>%
    summarize(mean_memory_accuracy = mean(subj_accuracy),
              subjects = n(),
              se_memory_accuracy = sd(subj_accuracy) / sqrt(n()),
              ci_lower = mean_memory_accuracy - se_memory_accuracy,
              ci_upper = mean_memory_accuracy + se_memory_accuracy)
  return(memory_summary)
}


# GRAPHING FUNCTIONS ===========================================================

plot_prediction_summary = function(prediction_summary) {
  prediction_summary %>%
    # filter(trial_index > 4) %>%
    ggplot(aes(x = trial_index, y = accuracy, color = is_control)) +
    geom_line() +
    labs(x = "Trial index", y = "Accuracy") +
    ggtitle("Prediction accuracy in each round") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        labels = CONDITION_LABELS) +
    individ_plot_theme
}


plot_generation_judgments = function(generation_judg_summary) {
  generation_judg_summary %>%
    ggplot(aes(x = is_control, y = mean_accuracy, 
               color = is_control, fill = is_control)) +
    geom_bar(stat = "identity", alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25, color = "black") +
    geom_hline(yintercept = 0.5, color = "red", size = 2, linetype = "dashed") +
    labs(x = "", y = "Mean accuracy") +
    ggtitle("Accuracy on generation judgment task") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        labels = CONDITION_LABELS) +
    scale_fill_viridis(discrete = T,
                        name = element_blank(),
                        labels = CONDITION_LABELS) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}


plot_eval_results = function(eval_data) {
  eval_data %>%
    ggplot(aes(x = is_target_rule, y = mean_rating, 
               color = is_control, fill = is_control)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(), width = 0.5, color = "black") +
    labs(y = "Mean rating (1: 'Not good' - 7: 'Good')") +
    ggtitle("Evaluation across conditions") +
    scale_x_discrete(name = element_blank(),
                   labels = RULE_EVAL_LABELS) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        labels = CONDITION_LABELS) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       labels = CONDITION_LABELS) +
    individ_plot_theme
}


plot_time_data = function(time_summary) {
  time_summary %>%
    ggplot(aes(x = is_control, y = mean_task_time, 
               color = is_control, fill = is_control)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25, color = "black") +
    labs(x = "", y = "Mean time on experiment (secs)") +
    ggtitle("Time on task across conditions") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        labels = CONDITION_LABELS) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       labels = CONDITION_LABELS) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}


plot_memory_data = function(memory_summary) {
  memory_summary %>%
    ggplot(aes(x = is_control, y = mean_memory_accuracy, 
               color = is_control, fill = is_control)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25, color = "black") +
    labs(x = "", y = "Mean accuracy") +
    ggtitle("Memory probe accuracy across conditions") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        labels = CONDITION_LABELS) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       labels = CONDITION_LABELS) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}


plot_generation_data = function(generation_data) {
  generation_data %>%
    group_by(is_control) %>%
    summarize(subjects = n(),
              correct_generation_pct = sum(free_response_coded) / n()) %>%
    ggplot(aes(x = is_control, y = correct_generation_pct, 
               color = is_control, fill = is_control)) +
      geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
      labs(x = "", y = "Percent correct rule generation") +
      ggtitle("Rule generation across conditions") +
      scale_color_viridis(discrete = T,
                          name = element_blank(),
                          labels = CONDITION_LABELS) +
      scale_fill_viridis(discrete = T,
                         name = element_blank(),
                         labels = CONDITION_LABELS) +
      individ_plot_theme +
      theme(axis.text.x = element_blank())
}


individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 24),
  axis.title.y = element_text(face = "bold", size = 20),
  axis.title.x = element_text(face = "bold", size = 20),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14),
  # legend text
  legend.text = element_text(size = 16),
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



# MAIN EFFECT ANALYSES =========================================================

# Summary
summary_data = read_data(SUMMARY_DATA)
summary_data = summary_data %>%
  mutate(Condition = ifelse(is_control == TRUE, "Describe", "Explain"))
tab = table(summary_data$Condition)

summary_data$browser = "chrome"
# 9am segments
summary_data$browser[summary_data$expt_end_ts == 1579195111824] = "firefox"
summary_data$browser[summary_data$expt_end_ts == 1579195689111] = "firefox"
summary_data$browser[summary_data$expt_end_ts == 1579195265899] = "firefox"
summary_data$browser[summary_data$expt_end_ts == 1579195158508] = "firefox"
summary_data$browser[summary_data$expt_end_ts == 1579196044620] = "firefox"
summary_data$browser[summary_data$expt_end_ts == 1579195407183] = "firefox"
summary_data$browser[summary_data$expt_end_ts == 1579195200442] = "firefox"

# 10am segments
summary_data$browser[summary_data$expt_end_ts == 1579198926883] = "firefox"

summary_data$browser[summary_data$expt_end_ts == 1579198381814] = "firefox?" # either this or below (10:13)
summary_data$browser[summary_data$expt_end_ts == 1579198394262] = "firefox?" # either this or below (10:13)

summary_data$browser[summary_data$expt_end_ts == 1579198526636] = "firefox?" # either this or below (10:15)
summary_data$browser[summary_data$expt_end_ts == 1579198525617] = "firefox?" # either this or above (10:15)


# Analysis: prediction accuracy by condition, trial
prediction_data = read_data(TRIAL_DATA)
prediction_summary = get_prediction_summary(prediction_data)
plot_prediction_summary(prediction_summary)


# Analysis: generation judgment task accuracy by condition
generation_judg = read_data(GENERATION_JUDG_DATA)
generation_judg_subj = generation_judg %>%
  mutate(input_correct = 
           (input_judgment == judgment_catches_fish)) %>%
  # NB: we only need subjID here but including it makes later group_by easier
  group_by(is_control, subjID) %>%
  summarize(subj_accuracy = sum(input_correct) / n())
generation_judg_summary = get_generation_judg_summary(generation_judg_subj)

# Plot results
plot_generation_judgments(generation_judg_summary)

# Statistical test
t_gen = t.test(generation_judg_subj$subj_accuracy[generation_judg_subj$is_control == FALSE],
               generation_judg_subj$subj_accuracy[generation_judg_subj$is_control == TRUE])
t_gen


# Analysis: evaluation ratings by condition
eval_data = read_data(EVAL_DATA)
eval_summary = get_eval_summary(eval_data)
plot_eval_results(eval_summary)
# Note all other rules here is average of subject average ratings (for X non-target rules), 
# whereas target rule is mean across subjects (since only one target rule)

# Statistical test: compare evaluation of target rule
t_eval = t.test(eval_data$input_rule_rating[eval_data$is_control == TRUE & eval_data$is_target_rule == TRUE],
                eval_data$input_rule_rating[eval_data$is_control == FALSE & eval_data$is_target_rule == TRUE])
t_eval


# COVARIATE ANALYSES ===========================================================

# Sanity check: experiment time
# TODO supplement this with trial completion time rather than whole experiment
time_data = read_data(SUMMARY_DATA)
time_data = time_data %>%
  mutate(experiment_completion_time = (expt_end_ts - expt_start_ts) / 1000)

# Statistical test
t_time = t.test(time_data$experiment_completion_time[time_data$is_control == FALSE],
           time_data$experiment_completion_time[time_data$is_control == TRUE])
t_time

# Plot time on experiment
time_summary = get_time_summary(time_data)
plot_time_data(time_summary)


# TODO look at time on describe/explain trials not just experiment overall


# Sanity check: memory performance
memory_data = read_data(MEMORY_DATA)
memory_subj_accuracy = get_memory_subj_accuracy(memory_data)
memory_summary = get_memory_summary(memory_subj_accuracy)

# Statistical test
t_mem = t.test(memory_subj_accuracy$subj_accuracy[memory_subj_accuracy$is_control == TRUE],
               memory_subj_accuracy$subj_accuracy[memory_subj_accuracy$is_control == FALSE])
t_mem

# Plot memory performance
plot_memory_data(memory_summary)



# GENERATION FREE RESPONSES ANALYSIS ===========================================

# TODO code free responses

generation_data = read_data(GENERATION_RESP_DATA)
generation_data = generation_data %>%
  mutate(free_response_coded = 0)

# Score generation data: only correct if it's very clear cut
generation_data$free_response
generation_data$free_response_coded[1] = 1 # TODO fill this and subsequent rows in with 0/1


plot_generation_data(generation_data)



