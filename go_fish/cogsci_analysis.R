#' Analysis for Cog Sci 2020 submission
#' Note there may be overlap between content here and what is in `cogsci_analysis.Rmd` and `summary_analysis.R`
#' However, *this script should be the final source of truth for any cog sci submission materials*.
#' 


rm(list = ls())
setwd("~/web/vullab/data_analysis/go_fish")

library(tidyverse)
library(viridis)


# GLOBALS ======================================================================
# Data files
SUMMARY_DATA = "01_go_fish_meta.csv"
TRIAL_DATA = "02_go_fish_trials.csv"
GENERATION_RESP_DATA = "03_go_fish_generation_free_resp.csv"
GENERATION_JUDG_DATA = "04_go_fish_generation_judgment.csv"
EVAL_DATA = "05_go_fish_evaluation.csv"
MEMORY_DATA = "06_go_fish_memory.csv"

PILOT_SUMMARY_DATA = "pilot/01_go_fish_pilot_meta.csv"
PILOT_TRIAL_DATA = "pilot/02_go_fish_pilot_trials.csv"
PILOT_GENERATION_RESP_DATA = "pilot/03_go_fish_pilot_generation_free_resp.csv"
PILOT_GENERATION_JUDG_DATA = "pilot/04_go_fish_pilot_generation_judgment.csv"
PILOT_EVAL_DATA = "pilot/05_go_fish_pilot_evaluation.csv"
PILOT_MEMORY_DATA = "pilot/06_go_fish_pilot_memory.csv"

# Data labels
RULE_EVAL_LABELS = c("TRUE" = "Target rule", "FALSE" = "All other rules")
DISTRACTOR_LABELS = c("TRUE" = "Target rule", "FALSE" = "Distractor rule")

DISTRACTOR_RULE = "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish."




# ANALYSIS FUNCTIONS ===========================================================

# Read in and process summary data file
read_summary_data = function(filepath, is_pilot) {
  # Read in data at filepath and add/modify columns as needed
  summary_data = read_csv(filepath)
  summary_data = summary_data %>%
    mutate(Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
           Pilot = is_pilot,
           experiment_completion_time = (expt_end_ts - expt_start_ts) / 1000)
  return(summary_data)
}


# Read in and process data from explain/describe trials
read_trial_data = function(filepath, is_pilot) {
  trial_data = read_csv(filepath)
  trial_data = trial_data %>%
    mutate(
      Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
      Pilot = is_pilot,
      input_correct =
        (input_prediction_catches_fish == prediction_catches_fish))
  return(trial_data)
}


# Read in and process data from generation "judgment" (binary response) task
read_generation_judgment_data = function(filepath, is_pilot) {
  generation_judgment_data = read_csv(filepath)
  generation_judgment_data = generation_judgment_data %>%
    mutate(
      Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
      Pilot = is_pilot,
      input_correct = 
        (input_judgment == judgment_catches_fish))
  return(generation_judgment_data)
}


# Read in and process data from generation free response task
# NB: coding of free response answers is handled separately
read_generation_free_resp_data = function(filepath, is_pilot) {
  generation_free_resp_data = read_csv(filepath)
  generation_free_resp_data = generation_free_resp_data %>%
    mutate(
      Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
      Pilot = is_pilot)
  
  generation_free_resp_data$free_response_str = str_replace_all(
    generation_free_resp_data$free_response, "\n" , "[newline]")
  
  return(generation_free_resp_data)
}


# Read in and process data from rule evaluation task
read_evaluation_data = function(filepath, is_pilot) {
  evaluation_data = read_csv(filepath)
  evaluation_data = evaluation_data %>%
    mutate(Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
           Pilot = is_pilot)
  return(evaluation_data)
}


# Read in and process data from memory task at end of experiment
read_memory_data = function(filepath, is_pilot) {
  memory_data = read_csv(filepath)
  memory_data = memory_data %>%
    mutate(Condition = ifelse(is_control == TRUE, "Describe", "Explain"),
           Pilot = is_pilot,
           memory_correct = 
             (memory_shape_in_expt == input_shape_in_expt))
  return(memory_data)
}


# Read in coded free response data (note this requires writing free response data to csv in initialization code)
read_coded_free_resp_data = function(filename, summary_data) {
  generation_free_resp_coded = read_csv(filename)
  generation_free_resp_coded = generation_free_resp_coded %>%
    select(subjID, free_response_str, Revision) %>%
    inner_join(., summary_data, by = "subjID") %>%
    select(subjID, Condition, free_response_str, Revision)
  
  return(generation_free_resp_coded)
}


# Summarize prediction data for graphing
get_prediction_summary = function(trial_data) {
  prediction_summary = trial_data %>%
    group_by(Condition, trial_index) %>%
    summarize(accuracy = sum(input_correct) / n())
  return(prediction_summary)
}


# Summarize generation judgment data by participant
get_generation_judgment_subj_summary = function(generation_judgment_data) {
  generation_subject_summary = generation_judgment_data %>%
    group_by(Condition, subjID) %>%
    summarize(subj_accuracy = sum(input_correct) / n())
  return(generation_subject_summary)
}


# Summarize generation judgment data across participants
get_generation_judgment_summary = function(generation_judgment_subject_summary) {
  generation_judg_summary = generation_judgment_subject_summary %>%
    group_by(Condition) %>%
    summarize(mean_accuracy = mean(subj_accuracy),
              subjects = n(),
              se_accuracy = sd(subj_accuracy) / sqrt(n()),
              ci_lower = mean_accuracy - se_accuracy,
              ci_upper = mean_accuracy + se_accuracy)
  return(generation_judg_summary)
}


# Summarize generation free response data across participants
get_generation_free_response_summary = function(generation_free_response_coded) {
  generation_free_response_summary = generation_free_response_coded %>%
    group_by(Condition) %>%
    summarize(subjects = n(),
              correct_generation_pct = sum(Revision) / n())
  return(generation_free_response_summary)
}


# Summarize evaluation data across participants and conditions for target and non-target rules
get_evaluation_summary = function(evaluation_data) {
  # Average rating across participants on target rule
  eval_summary_target_rule = evaluation_data %>%
    filter(is_target_rule == TRUE) %>% # for target rule, summarize across participants
    group_by(Condition) %>%
    summarize(ruleset = "Target",
              mean_rating = mean(input_rule_rating),
              subjects = n(),
              se_rating = sd(input_rule_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
  
  # Average rating across participants on distractor rule
  eval_summary_distractor_rule = evaluation_data %>%
    filter(rule_text == DISTRACTOR_RULE) %>% # for distractor rule, summarize across participants
    group_by(Condition) %>%
    summarize(ruleset = "Distractor",
              mean_rating = mean(input_rule_rating),
              subjects = n(),
              se_rating = sd(input_rule_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
  
  # Average of each participant's average across non-target rules
  eval_summary_other_rules = evaluation_data %>%
    filter(is_target_rule == FALSE &
             rule_text != DISTRACTOR_RULE) %>% # for all other rules, summarize average of participant avgs
    group_by(Condition, subjID) %>%
    summarize(mean_subj_rating = mean(input_rule_rating),
              rules = n()) %>%
    group_by(Condition) %>%
    summarize(ruleset = "All other rules",
              mean_rating = mean(mean_subj_rating),
              subjects = n(),
              se_rating = sd(mean_subj_rating) / sqrt(n()),
              ci_lower = mean_rating - se_rating,
              ci_upper = mean_rating + se_rating)
  
  eval_summary = rbind(eval_summary_target_rule, eval_summary_distractor_rule, eval_summary_other_rules)
  return(eval_summary)
}


# Summarize experiment completion time data
get_time_summary = function(time_data) {
  time_summary = time_data %>%
    group_by(Condition) %>%
    summarize(mean_task_time = mean(experiment_completion_time),
              subjects = n(),
              se_task_time = sd(experiment_completion_time) / sqrt(subjects),
              ci_lower = mean_task_time - se_task_time,
              ci_upper = mean_task_time + se_task_time)
  return(time_summary)
}


# Summarize average trial completion time by participant
get_trial_time_subj_summary = function(trial_data) {
  trial_subject_summary = trial_data %>%
    mutate(trial_completion_time = (trial_n_end_ts - trial_n_start_ts) / 1000) %>%
    group_by(Pilot, Condition, subjID) %>%
    summarize(mean_trial_completion = mean(trial_completion_time),
              trials = n(),
              se_trial_completion = sd(trial_completion_time) / sqrt(trials),
              ci_lower = mean_trial_completion - se_trial_completion,
              ci_upper = mean_trial_completion + se_trial_completion)
  return(trial_subject_summary)
}


# Get summary of time spent on trials in each condition across participants
get_trial_time_summary = function(trial_time_subj_summary) {
  trial_time_summary = trial_time_subj_summary %>%
    # trial time completion not available in pilot data
    filter(Pilot == FALSE) %>%
    group_by(Condition) %>%
    # These column names kept the same as those in `get_time_summary` for easier graphing
    summarize(mean_task_time = mean(mean_trial_completion),
              subjects = n(),
              se_trial_time = sd(mean_trial_completion) / sqrt(subjects),
              ci_lower = mean_task_time - se_trial_time,
              ci_upper = mean_task_time + se_trial_time)
  return(trial_time_summary)
}


# Summarize memory performance data by participant
get_memory_subj_summary = function(memory_data) {
  memory_subj_accuracy = memory_data %>%
    group_by(Condition, subjID) %>%
    summarize(subj_accuracy = sum(memory_correct) / n())
  return(memory_subj_accuracy)
}


# Summarize memory performance across participants by condition
get_memory_summary = function(memory_subject_summary) {
  memory_summary = memory_subject_summary %>%
    group_by(Condition) %>%
    summarize(mean_memory_accuracy = mean(subj_accuracy),
              subjects = n(),
              se_memory_accuracy = sd(subj_accuracy) / sqrt(n()),
              ci_lower = mean_memory_accuracy - se_memory_accuracy,
              ci_upper = mean_memory_accuracy + se_memory_accuracy)
  return(memory_summary)
}


# Hand-code participant responses on free response rule generation data
code_generation_free_resp_data = function(generation_free_resp_data) {
  # Add column for coding of free response (1 = definitely accurate rule, 0 otherwise)
  generation_free_resp_coded = generation_free_resp_data %>%
    mutate(free_response_coded = 0)
  
  # Code free response data
  # # Comments:
  # # Unsure on 9, 14 (similar to 9), 27, 35 (similar to 9, 14), 72 (also references previous success)
  # # Unsure on 75 (misses triangle...)
  # generation_free_resp_coded$free_response_coded[2] = 1
  # generation_free_resp_coded$free_response_coded[6] = 1
  # generation_free_resp_coded$free_response_coded[7] = 1
  # generation_free_resp_coded$free_response_coded[8] = 1
  # generation_free_resp_coded$free_response_coded[10] = 1
  # generation_free_resp_coded$free_response_coded[11] = 1
  # generation_free_resp_coded$free_response_coded[19] = 1
  # generation_free_resp_coded$free_response_coded[20] = 1
  # generation_free_resp_coded$free_response_coded[23] = 1
  # generation_free_resp_coded$free_response_coded[24] = 1
  # generation_free_resp_coded$free_response_coded[25] = 1
  # generation_free_resp_coded$free_response_coded[26] = 1
  # generation_free_resp_coded$free_response_coded[31] = 1
  # generation_free_resp_coded$free_response_coded[34] = 1
  # generation_free_resp_coded$free_response_coded[36] = 1
  # generation_free_resp_coded$free_response_coded[37] = 1
  # generation_free_resp_coded$free_response_coded[40] = 1
  # generation_free_resp_coded$free_response_coded[43] = 1
  # generation_free_resp_coded$free_response_coded[44] = 1
  # generation_free_resp_coded$free_response_coded[45] = 1
  # generation_free_resp_coded$free_response_coded[47] = 1
  # generation_free_resp_coded$free_response_coded[48] = 1
  # generation_free_resp_coded$free_response_coded[55] = 1
  # generation_free_resp_coded$free_response_coded[58] = 1
  # generation_free_resp_coded$free_response_coded[63] = 1
  # # Pilot coding
  # generation_free_resp_coded$free_response_coded[70] = 1
  # generation_free_resp_coded$free_response_coded[71] = 1
  # generation_free_resp_coded$free_response_coded[74] = 1
  # generation_free_resp_coded$free_response_coded[79] = 1
  # generation_free_resp_coded$free_response_coded[80] = 1
  # generation_free_resp_coded$free_response_coded[82] = 1
  
  return(generation_free_resp_coded)
}





# PLOTTING FUNCTIONS ===========================================================

# Generic plot theme for use in most graphs
individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 24),
  axis.title.y = element_text(face = "bold", size = 24),
  axis.title.x = element_text(face = "bold", size = 20),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 16, face = "bold"),
  axis.text.x = element_text(size = 16, face = "bold"),
  # legend text
  legend.text = element_text(size = 18, face = "bold"),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom",
  legend.key = element_rect(colour = "transparent", fill = "transparent")
)


# Line plot of prediction accuracy by condition for each trial
# (percent of participants in each condition who were correct in each trial)
plot_prediction_summary = function(prediction_summary) {
  prediction_summary %>%
    filter(trial_index > 4) %>%
    ggplot(aes(x = trial_index, y = accuracy, color = Condition)) +
    geom_line() +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    labs(x = "Trial index", y = "Accuracy") +
    # ggtitle("Prediction accuracy in each round") +
    scale_color_viridis(discrete = T,
                        name = element_blank()) +
    individ_plot_theme
}


# Bar chart of classification accuracy on binary generation judgment task by condition
plot_generation_judgments = function(generation_judgment_summary) {
  generation_judgment_summary %>%
    ggplot(aes(x = Condition, y = mean_accuracy, 
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", alpha = 0.5, width = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25) +
    geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
    labs(x = "", y = "Mean classification accuracy") +
    ylim(c(0, 1)) +
    # ggtitle("Accuracy on generation judgment task") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}


# Bar chart of rule generation accuracy for coded free response answers
plot_generation_free_responses = function(generation_free_resp_summary) {
  generation_free_resp_summary %>%
    ggplot(aes(x = Condition, y = correct_generation_pct, 
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    labs(x = "", y = "Percent Correct") +
    ggtitle("Rule generation across conditions") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}


# Bar chart of average evaluation ratings across conditions on rule evaluation task
plot_evaluation_results = function(evaluation_summary, comparison_set) {
  evaluation_summary %>%
    ggplot(aes(x = ruleset, y = mean_rating, 
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.5, alpha = 0.5) +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper), 
      position = position_dodge(width = 0.5, preserve = "single"), 
      width = 0.2) +
    labs(y = "Mean evaluation rating") +
    # ggtitle("Evaluation across conditions") +
    scale_x_discrete(name = element_blank()) +
                     #labels = comparison_set) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    scale_y_continuous(breaks = seq(1, 7)) +
    individ_plot_theme
}


# Bar chart of experiment completion time
plot_time_data = function(time_summary, ylab, ymax, title) {
  time_summary %>%
    ggplot(aes(x = Condition, y = mean_task_time, 
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25) +
    ylim(0, ymax) +
    labs(x = "", y = ylab) +
    ggtitle(title) +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme +
    theme(axis.text.x = element_blank(),
          plot.title = element_text(size = 32, face = "bold"))
}


# Bar chart of average memory accuracy across conditions 
plot_memory_data = function(memory_summary) {
  memory_summary %>%
    ggplot(aes(x = Condition, y = mean_memory_accuracy, 
               color = Condition, fill = Condition)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25) +
    geom_hline(yintercept = 0.5, linetype = "dashed", size = 1) +
    ylim(c(0, 1)) +
    labs(x = "", y = "Mean memory accuracy") +
    # ggtitle("Memory probe accuracy across conditions") +
    scale_color_viridis(discrete = T,
                        name = element_blank(),
                        # Change defaults to be blue/green instead of yellow/purple
                        begin = 0.25,
                        end = 0.75) +
    scale_fill_viridis(discrete = T,
                       name = element_blank(),
                       # Change defaults to be blue/green instead of yellow/purple
                       begin = 0.25,
                       end = 0.75) +
    individ_plot_theme +
    theme(axis.text.x = element_blank())
}






# DATA INITIALIZATION ==========================================================

# Read in data
summary_data = bind_rows(read_summary_data(SUMMARY_DATA, FALSE), 
                         read_summary_data(PILOT_SUMMARY_DATA, TRUE))
trial_data = bind_rows(read_trial_data(TRIAL_DATA, FALSE), 
                       read_trial_data(PILOT_TRIAL_DATA, TRUE))
generation_judgment_data = bind_rows(read_generation_judgment_data(GENERATION_JUDG_DATA, FALSE),
                                     read_generation_judgment_data(PILOT_GENERATION_JUDG_DATA, TRUE))
evaluation_data = bind_rows(read_evaluation_data(EVAL_DATA, FALSE),
                            read_evaluation_data(PILOT_EVAL_DATA, TRUE))
memory_data = bind_rows(read_memory_data(MEMORY_DATA, FALSE),
                        read_memory_data(PILOT_MEMORY_DATA, TRUE))

# Summarize data
prediction_summary = get_prediction_summary(trial_data)
generation_judgment_subject_summary = get_generation_judgment_subj_summary(generation_judgment_data)
generation_judgment_summary = get_generation_judgment_summary(generation_judgment_subject_summary)
evaluation_summary = get_evaluation_summary(evaluation_data)

completion_time_summary = get_time_summary(summary_data)
trial_time_subject_summary = get_trial_time_subj_summary(trial_data)
trial_time_summary = get_trial_time_summary(trial_time_subject_summary)
memory_subject_summary = get_memory_subj_summary(memory_data)
memory_summary = get_memory_summary(memory_subject_summary)


# Read in and code free response data
# generation_free_resp_data = bind_rows(read_generation_free_resp_data(GENERATION_RESP_DATA, FALSE),
#                                       read_generation_free_resp_data(PILOT_GENERATION_RESP_DATA, TRUE))
# generation_free_resp_coded = code_generation_free_resp_data(generation_free_resp_data)
# generation_free_resp_summary = get_generation_free_response_summary(generation_free_resp_coded)


# generation_free_resp_alternate_coder = generation_free_resp_coded %>%
#   select(subjID, free_response_str) %>%
#   mutate(free_response_coder2 = 0)
# write.csv(generation_free_resp_alternate_coder, "free_response_alternate.csv")

GENERATION_RESP_DATA_CODED = "free_response_combined.csv"
generation_free_resp_coded = read_coded_free_resp_data(GENERATION_RESP_DATA_CODED, summary_data)
generation_free_resp_summary = get_generation_free_response_summary(generation_free_resp_coded)


# See how our coding compared to accuracy in the classification task
generation_task_comparison = generation_free_resp_coded %>%
  inner_join(generation_judgment_subject_summary, by = c("subjID", "Condition"))

# How many people did we code as getting the rule who were < 100% on the classification?
generation_task_comparison %>%
  filter(Revision == 1 & subj_accuracy < 1)

# How many people got 100% on the classification but were coded as not getting the rule?
generation_task_comparison %>%
  filter(Revision == 0 & subj_accuracy == 1)

# Chi-sq test to make sure that raw count of people who got 100% on classification is significant
count_data = table(generation_task_comparison$Condition, generation_task_comparison$subj_accuracy == 1)
chisq_counts = chisq.test(count_data)
chisq_counts



# DATA ANALYSIS: GENERATION ====================================================

# Generation judgment task
plot_generation_judgments(generation_judgment_summary)

t_gen = t.test(
  generation_judgment_subject_summary$subj_accuracy[generation_judgment_subject_summary$Condition == "Describe"],
  generation_judgment_subject_summary$subj_accuracy[generation_judgment_subject_summary$Condition == "Explain"],
  var.equal = T
)
t_gen
round(t_gen$estimate[1], 2)
round(t_gen$estimate[2], 2)
round(t_gen$statistic, 2)
t_gen$parameter
round(t_gen$p.value, 3)


# Generation free response task
# TODO include this graph? Doesn't seem necessary, Bonawitz/Griffiths don't...

plot_generation_free_responses(generation_free_resp_summary)
generation_props = generation_free_resp_coded %>%
  group_by(Condition) %>%
  summarize(success = sum(Revision),
            total = n())
chisq_gen = prop.test(c(generation_props$success), c(generation_props$total))
round(chisq_gen$estimate[1], 3)
round(chisq_gen$estimate[2], 3)
chisq_gen$parameter
round(chisq_gen$statistic, 2)
round(chisq_gen$p.value, 3)





# DATA ANALYSIS: EVALUATION ====================================================

plot_evaluation_results(evaluation_summary, RULE_EVAL_LABELS)
t_eval = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe" & evaluation_data$is_target_rule == TRUE],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain" & evaluation_data$is_target_rule == TRUE],
  var.equal = T
)
t_eval
round(t_eval$estimate[1], 2)
round(t_eval$estimate[2], 2)
round(t_eval$statistic, 2)
t_eval$parameter
round(t_eval$p.value, 3)


# Look at counts of people making each rating (instead of treating ratings as continuous)
  # all participants
table(evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain" & evaluation_data$is_target_rule == T])
table(evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe" & evaluation_data$is_target_rule == T])




# Wilcoxon signed-rank test showing that target rule is different from all other rules across both groups
eval_summary_other_rules = evaluation_data %>%
  filter(is_target_rule == FALSE) %>% # for target rule, summarize across participants
  group_by(is_target_rule, Condition, subjID) %>%
  summarize(mean_subj_rating = mean(input_rule_rating))

eval_difference = evaluation_data %>%
  group_by(subjID, Condition) %>%
  filter(is_target_rule == TRUE) %>%
  inner_join(., eval_summary_other_rules, by = "subjID") %>%
  mutate(diff = input_rule_rating - mean_subj_rating) %>%
  select(subjID, Condition.x, diff)

# TODO is this right? Is the conversion from V to z okay?
wil_exp = wilcox.test(eval_difference$diff[eval_difference$Condition.x == "Explain"], exact = F)
wil_des = wilcox.test(eval_difference$diff[eval_difference$Condition.x == "Describe"], exact = F)

wil_exp
wil_des
qnorm(wil_exp$p.value / 2)
qnorm(wil_des$p.value / 2)


eval_summary_target_rule = evaluation_data %>%
  filter(is_target_rule == TRUE) %>% # for target rule, summarize across participants
  group_by(is_target_rule, Condition) %>%
  summarize(ruleset = "Target",
            mean_rating = mean(input_rule_rating),
            subjects = n(),
            se_rating = sd(input_rule_rating) / sqrt(n()),
            ci_lower = mean_rating - se_rating,
            ci_upper = mean_rating + se_rating)

eval_summary_distractor_rule = evaluation_data %>%
  filter(rule_text == DISTRACTOR_RULE) %>%
  group_by(is_target_rule, Condition) %>%
  summarize(ruleset = "Distractor",
            mean_rating = mean(input_rule_rating),
            subjects = n(),
            se_rating = sd(input_rule_rating) / sqrt(n()),
            ci_lower = mean_rating - se_rating,
            ci_upper = mean_rating + se_rating)

eval_summary_comparison = bind_rows(eval_summary_target_rule, eval_summary_distractor_rule)
plot_evaluation_results(eval_summary_comparison, DISTRACTOR_LABELS)
t_eval_comp = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe"
                                    & evaluation_data$rule_text == DISTRACTOR_RULE],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain" 
                                    & evaluation_data$rule_text == DISTRACTOR_RULE],
  var.equal = T
)
t_eval_comp
round(t_eval_comp$estimate[1], 2)
round(t_eval_comp$estimate[2], 2)
round(t_eval_comp$statistic, 2)
t_eval$parameter
round(t_eval_comp$p.value, 3)



# Did people rate the distractor lower than the target rule in each condition?
t_eval_target_dist_exp = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain"
                                    & evaluation_data$rule_text == "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish."],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Explain" 
                                    & evaluation_data$is_target_rule == TRUE],
  var.equal = T
)

t_eval_target_dist_desc = t.test(
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe"
                                    & evaluation_data$rule_text == "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish."],
  evaluation_data$input_rule_rating[evaluation_data$Condition == "Describe" 
                                    & evaluation_data$is_target_rule == TRUE],
  var.equal = T
)

# Explainers
t_eval_target_dist_exp
round(t_eval_target_dist_exp$statistic, 2)
round(t_eval_target_dist_exp$parameter, 1)
t_eval_target_dist_exp$p.value
# Describers
t_eval_target_dist_desc
round(t_eval_target_dist_desc$statistic, 2)
round(t_eval_target_dist_desc$parameter, 1)
t_eval_target_dist_desc$p.value


# Look at evaluation just among people who did/didn't generate correctly
#   (see Bonawitz/Griffiths)
subset_participants = generation_free_resp_coded %>%
  select(subjID, Condition, Revision) %>%
  # filter(Revision == 0) %>%
  # filter(Revision == 1) %>%
  inner_join(., evaluation_data, on = subjID)


# Sanity check the join above
table(subset_participants$subjID)
unique(subset_participants$subjID) # 56 or 30
sum(generation_free_resp_coded$Revision) # 30: this is equal to 86 (total) - number of unique participants above or equal to number of unique part. above

# Plot data
eval_summary_subset = get_evaluation_summary(subset_participants)
plot_evaluation_results(eval_summary_subset, RULE_EVAL_LABELS)

t_subset = t.test(
  subset_participants$input_rule_rating[subset_participants$Condition == "Describe" 
                                           & subset_participants$is_target_rule == TRUE],
  subset_participants$input_rule_rating[subset_participants$Condition == "Explain" 
                                           & subset_participants$is_target_rule == TRUE],
  var.equal = T
)

t_subset
round(t_subset$estimate[1], 2)
round(t_subset$estimate[2], 2)
round(t_subset$statistic, 2)
round(t_subset$parameter, 1)
round(t_subset$p.value, 3)

# look at counts of people who made each rating rather than treating it as continuous
table(subset_participants$input_rule_rating[subset_participants$Condition == "Explain" & subset_participants$is_target_rule == T])
table(subset_participants$input_rule_rating[subset_participants$Condition == "Describe" & subset_participants$is_target_rule == T])

subset_summary = subset_participants %>%
  filter(is_target_rule == T) %>%
  group_by(Condition, as.factor(Revision)) %>%
  summarize(mean_rating = mean(input_rule_rating),
            n = n(),
            se = sd(input_rule_rating) / sqrt(n),
            ci_lower = mean_rating - se,
            ci_upper = mean_rating + se)

subset_participants %>%
  filter(is_target_rule == T) %>%
  # filter(rule_text == DISTRACTOR_RULE) %>%
  ggplot(aes(x = Condition, y = input_rule_rating, color = as.factor(Revision))) +
  geom_jitter(width = 0.25, alpha = 0.5, height = 0.25, size = 4) +
  geom_point(data = subset_summary, 
             aes(x = Condition, y = mean_rating, color = `as.factor(Revision)`),
             size = 6) +
  geom_errorbar(data = subset_summary, 
                aes(x = Condition, y = mean_rating, color = `as.factor(Revision)`, ymin = ci_lower, ymax = ci_upper),
                width = 0.25,
                size = 1) +
  scale_color_viridis(discrete = T,
                      labels = c("0" = "Incorrect hypothesis", "1" = "Correct hypothesis"),
                      name = element_blank(),
                      # Change defaults to be blue/green instead of yellow/purple
                      begin = 0.25,
                      end = 0.75) +
  scale_y_discrete(breaks = c(2, 3, 4, 5, 6, 7),
                   labels = c("2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7"),
                   limits = c(2, 3, 4, 5, 6, 7)) +
  labs(y = "Rating (1-7)") +
  ggtitle("Target rule evaluation") +
  #ylim(0.5, 7.5) +
  individ_plot_theme


t_subset = t.test(
  subset_participants$input_rule_rating[subset_participants$Condition == "Describe" 
                                        & subset_participants$is_target_rule == TRUE
                                        & as.factor(subset_participants$Revision) == 0],
  subset_participants$input_rule_rating[subset_participants$Condition == "Explain" 
                                        & subset_participants$is_target_rule == TRUE
                                        & as.factor(subset_participants$Revision) == 0],
  var.equal = T
)


# Wilcoxon signed-rank test showing that target rule is different from all other rules across both groups
eval_summary_other_rules_subset = subset_participants %>%
  filter(is_target_rule == FALSE) %>% # for target rule, summarize across participants
  group_by(is_target_rule, Condition, subjID) %>%
  summarize(mean_subj_rating = mean(input_rule_rating))

eval_difference_subset = subset_participants %>%
  group_by(subjID, Condition) %>%
  filter(is_target_rule == TRUE) %>%
  inner_join(., eval_summary_other_rules_subset, by = "subjID") %>%
  mutate(diff = input_rule_rating - mean_subj_rating) %>%
  select(subjID, Condition.x, diff)

wil_exp_subset = wilcox.test(eval_difference_subset$diff[eval_difference_subset$Condition.x == "Explain"], exact = F)
wil_des_subset = wilcox.test(eval_difference_subset$diff[eval_difference_subset$Condition.x == "Describe"], exact = F)

wil_exp_subset
wil_des_subset
qnorm(wil_exp_subset$p.value / 2)
qnorm(wil_des_subset$p.value / 2)




# COVARIATE ANALYSIS: MEMORY ===================================================

plot_memory_data(memory_summary)
t_mem = t.test(memory_subject_summary$subj_accuracy[memory_subject_summary$Condition == "Describe"],
               memory_subject_summary$subj_accuracy[memory_subject_summary$Condition == "Explain"],
               var.equal = T)
t_mem
round(t_mem$estimate[1], 2)
round(t_mem$estimate[2], 2)
round(t_mem$statistic, 2)
round(t_mem$parameter, 1)
round(t_mem$p.value, 2)

# Did participants perform above chance?
t.test(memory_subject_summary$subj_accuracy[memory_subject_summary$Condition == "Explain"], mu = 0.5,
       equal.var = T)
t.test(memory_subject_summary$subj_accuracy[memory_subject_summary$Condition == "Describe"], mu = 0.5,
       equal.var = T)


# TODO split out this chart and analysis by positive and negative probes


# Compare people who got correct rule and those who didn't
memory_hypothesis_join = generation_free_resp_coded %>%
  select(subjID, Condition, Revision) %>%
  inner_join(., memory_subject_summary, on = subjID)

t.test(memory_hypothesis_join$subj_accuracy[memory_hypothesis_join$Revision == 0],
       memory_hypothesis_join$subj_accuracy[memory_hypothesis_join$Revision == 1],
       var.equal = T)



# COVARIATE ANALYSIS: TIME =====================================================

plot_time_data(completion_time_summary, ylab = "Seconds", ymax = 1000, title = "Mean time on experiment")
t_time = t.test(summary_data$experiment_completion_time[summary_data$Condition == "Describe"],
                summary_data$experiment_completion_time[summary_data$Condition == "Explain"],
                var.equal = T)
t_time
round(t_time$estimate[1], 0) # seconds on task
round(t_time$estimate[2], 0) # seconds on task
round(t_time$statistic, 2)
round(t_time$parameter, 1)
round(t_time$p.value, 3)

# TODO include this graph instead of the above?
plot_time_data(trial_time_summary, ylab = "Seconds", ymax = 80, title = "Mean time on trials")
t_trials = t.test(trial_time_subject_summary$mean_trial_completion[trial_time_subject_summary$Condition == "Describe" &
                                                                     trial_time_subject_summary$Pilot == FALSE],
                  trial_time_subject_summary$mean_trial_completion[trial_time_subject_summary$Condition == "Explain" &
                                                                     trial_time_subject_summary$Pilot == FALSE],
                  var.equal = T)
t_trials
round(t_trials$estimate[1], 1) # seconds on trials
round(t_trials$estimate[2], 1) # seconds on trials
round(t_trials$statistic, 3)
round(t_trials$parameter, 1)
round(t_trials$p.value, 2)


# Compare people who got correct rule and those who didn't
time_hypothesis_join = generation_free_resp_coded %>%
  select(subjID, Condition, Revision) %>%
  inner_join(., summary_data, on = subjID) %>%
  select(subjID, Condition, Revision, experiment_completion_time)

t.test(time_hypothesis_join$experiment_completion_time[time_hypothesis_join$Revision == 0],
       time_hypothesis_join$experiment_completion_time[time_hypothesis_join$Revision == 1],
       var.equal = T)





# APPENDIX ANALYSIS: PREDICTIONS ===============================================

plot_prediction_summary(prediction_summary)

# TODO get error bars on this similar to Bonawitz/Griffiths

# Fit regressions to data just to ensure no signal
# TODO if including this, revise models to only look at trials > 4 
# (or change graph above to look at all trials)
mod_exp = lm(data = prediction_summary[prediction_summary$Condition == "Explain",], accuracy ~ trial_index)
mod_des = lm(data = prediction_summary[prediction_summary$Condition == "Describe",], accuracy ~ trial_index)

summary(mod_exp)
summary(mod_des)





