#'
#' This script looks at whether people are sensitive to the exploitability of their opponents.
#' We plot correlation between minimum entropy within a given dyad for various entropy measures
#' and win count differential between that minimum entropy player and their opponent. 
#' If one player in a dyad has a very low entropy, then that dyad should have a *high* win count differential 
#' because their opponent should detect the dependency.
#' If both players have high entropy on a given measure, then we should not expect high
#' win count differential
#' 


## SUMMARY:
# Prev move and prev two moves are most meaningful. Prev move approaches significance (p = 0.08) 
# and prev 2moves is significant (p = 0.05)
# All others are not significant, have outliers, etc.


rm(list = ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")
source('data_analysis_expected_value.R') # NB: nested sourcing here. this sources 02 which sources 01 which sources 00...

# GLOBALS
xlab = "Minimum within-dyad Shannon entropy (S)"
ylab = "Dyad win count differential"



### MOVE DISTRIBUTION ==========================================================

# Combine win count differential and relevant entropy 
entropy_win_diff_comparison_move_dist = data %>%
  inner_join(player.entropy, by = "player_id") %>%
  group_by(game_id, player_id, entropy.0) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(opp_win_count = ifelse(is.na(lag(n, 1)), lead(n, 1), lag(n, 1))) %>%
  group_by(game_id) %>%
  filter(entropy.0 == min(entropy.0)) %>%
  mutate(win_diff = (n - opp_win_count)) %>%
  rename(min_entropy_move_dist = entropy.0) %>%
  select(game_id, player_id, min_entropy_move_dist, win_diff)


# Get regression terms and correlation between min entropy and win count differential
model = lm(data = entropy_win_diff_comparison_move_dist, win_diff ~ min_entropy_move_dist)
summary(model)
cor.test(entropy_win_diff_comparison_move_dist$min_entropy_move_dist, 
         entropy_win_diff_comparison_move_dist$win_diff)
# Summary: relationship is not significant in wrong direction (cor = -0.042, p = 0.75)


# Scatter plot entropy vals against win count differential
entropy_win_diff_comparison_move_dist %>%
  ggplot(aes(x = min_entropy_move_dist, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over move distribution") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))



### PLAYER PREV MOVE DISTRIBUTION ==============================================

# Combine win count differential and relevant entropy 
entropy_win_diff_comparison_prev_move_dist = data %>%
  inner_join(player.prev.move.entropy, by = "player_id") %>%
  group_by(game_id, player_id, entropy.1.player) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(opp_win_count = ifelse(is.na(lag(n, 1)), lead(n, 1), lag(n, 1))) %>%
  group_by(game_id) %>%
  filter(entropy.1.player == min(entropy.1.player)) %>%
  mutate(win_diff = (n - opp_win_count)) %>%
  rename(min_entropy_prev_move = entropy.1.player) %>%
  select(game_id, player_id, min_entropy_prev_move, win_diff)


# Get regression terms and correlation between min entropy and win count differential
model = lm(data = entropy_win_diff_comparison_prev_move_dist, win_diff ~ min_entropy_prev_move)
summary(model)
cor_vals = cor.test(entropy_win_diff_comparison_prev_move_dist$min_entropy_prev_move, 
         entropy_win_diff_comparison_prev_move_dist$win_diff)
cor_vals
# Summary: relationship approaching significance in right direction (cor = 0.234, p = 0.08)


# Generate predictions for input on graph
predictions = data.frame(
  entropy = seq(1.2, 1.6, by = 0.1)
)
predictions = predictions %>%
  mutate(win_diff = model$coefficients[['(Intercept)']] + model$coefficients[['min_entropy_prev_move']] * entropy)

labelx = 1.3
labely = 0
cor = round(cor_vals$estimate, 3)
cor_p = round(cor_vals$p.value, 3)

# Scatter plot entropy vals against win count differential
entropy_win_diff_comparison_prev_move_dist %>%
  ggplot(aes(x = min_entropy_prev_move, y = win_diff)) +
  geom_point(aes(color = "prev_move"), alpha = 0.75, size = 5) +
  geom_line(data = predictions, aes(x = entropy, y = win_diff), size = 3, color = "red") +
  geom_label(aes(x = labelx, y = labely, label = str_c("r = ", cor, " (p = ", cor_p, ")")), color = "red", size = 6,
             label.padding = unit(0.5, "lines")) +
  labs(x = xlab, y = ylab) +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = c("prev_move" = "Previous move dependency"),
                      begin = 0.2, end = 0.8) +
  # ggtitle("Previous move entropy predicts win count differential") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 32, face = "bold"),
        legend.text = element_text(size = 24, face = "bold"))





### OPPONENT PREV MOVE DISTRIBUTION ============================================

# Combine win count differential and relevant entropy 
entropy_win_diff_comparison_opponent_prev_move_dist = data %>%
  inner_join(opponent.prev.move.entropy, by = "player_id") %>%
  group_by(game_id, player_id, entropy.1.opponent) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(opp_win_count = ifelse(is.na(lag(n, 1)), lead(n, 1), lag(n, 1))) %>%
  group_by(game_id) %>%
  filter(entropy.1.opponent == min(entropy.1.opponent)) %>%
  mutate(win_diff = (n - opp_win_count)) %>%
  rename(min_entropy_opponent_prev_move = entropy.1.opponent) %>%
  select(game_id, player_id, min_entropy_opponent_prev_move, win_diff)

# Remove outlier; toggle commenting this out
entropy_win_diff_comparison_opponent_prev_move_dist = entropy_win_diff_comparison_opponent_prev_move_dist %>%
  ungroup() %>%
  filter(min_entropy_opponent_prev_move != min(min_entropy_opponent_prev_move))


# Get regression terms and correlation between min entropy and win count differential
model = lm(data = entropy_win_diff_comparison_opponent_prev_move_dist, 
           win_diff ~ min_entropy_opponent_prev_move)
summary(model)
cor.test(entropy_win_diff_comparison_opponent_prev_move_dist$min_entropy_opponent_prev_move, 
         entropy_win_diff_comparison_opponent_prev_move_dist$win_diff)
# Summary: has noticeable outlier; significant in wrong direction (cor = -0.262, p = 0.05)
# With outlier removed, relationship is not significant in wrong direction (cor = -0.075, p = 0.58)


# Scatter plot entropy vals against win count differential
entropy_win_diff_comparison_opponent_prev_move_dist %>%
  ggplot(aes(x = min_entropy_opponent_prev_move, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over move distribution given opponent previous move") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))




### TRANSITION DISTRIBUTION ====================================================

# Combine win count differential and relevant entropy 
entropy_win_diff_comparison_transition_dist = data %>%
  inner_join(player.transition.entropy, by = "player_id") %>%
  group_by(game_id, player_id, transition.entropy.0) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(opp_win_count = ifelse(is.na(lag(n, 1)), lead(n, 1), lag(n, 1))) %>%
  group_by(game_id) %>%
  filter(transition.entropy.0 == min(transition.entropy.0)) %>%
  mutate(win_diff = (n - opp_win_count)) %>%
  rename(min_entropy_transition = transition.entropy.0) %>%
  select(game_id, player_id, min_entropy_transition, win_diff)


# Get regression terms and correlation between min entropy and win count differential
model = lm(data = entropy_win_diff_comparison_transition_dist, 
           win_diff ~ min_entropy_transition)
summary(model)
cor.test(entropy_win_diff_comparison_transition_dist$min_entropy_transition, 
         entropy_win_diff_comparison_transition_dist$win_diff)
# Summary: relationship is not significant in right direction (cor = 0.151, p = 0.26)


# Scatter plot entropy vals against win count differential
entropy_win_diff_comparison_transition_dist %>%
  ggplot(aes(x = min_entropy_transition, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over transition distribution (+/-/0)") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))




### TRANSITION-OUTCOME DISTRIBUTION ============================================

# Combine win count differential and relevant entropy 
entropy_win_diff_comparison_transition_outcome_dist = data %>%
  inner_join(player.transition.outcome.entropy, by = "player_id") %>%
  group_by(game_id, player_id, transition.entropy.1.player.outcome) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(opp_win_count = ifelse(is.na(lag(n, 1)), lead(n, 1), lag(n, 1))) %>%
  group_by(game_id) %>%
  filter(transition.entropy.1.player.outcome == min(transition.entropy.1.player.outcome)) %>%
  mutate(win_diff = (n - opp_win_count)) %>%
  rename(min_entropy_transition_outcome = transition.entropy.1.player.outcome) %>%
  select(game_id, player_id, min_entropy_transition_outcome, win_diff)

# Remove outlier; toggle commenting this out
entropy_win_diff_comparison_transition_outcome_dist = entropy_win_diff_comparison_transition_outcome_dist %>%
  ungroup() %>%
  filter(min_entropy_transition_outcome != min(min_entropy_transition_outcome))


# Get regression terms and correlation between min entropy and win count differential
model = lm(data = entropy_win_diff_comparison_transition_outcome_dist, 
           win_diff ~ min_entropy_transition_outcome)
summary(model)
cor.test(entropy_win_diff_comparison_transition_outcome_dist$min_entropy_transition_outcome, 
         entropy_win_diff_comparison_transition_outcome_dist$win_diff)
# Summary: has noticeable outlier; not significant in wrong direction (cor = -0.016, p = 0.91)
# With outlier removed, relationship *is* significant in right direction (cor = 0.284, p = 0.03)


# Scatter plot entropy vals against win count differential
entropy_win_diff_comparison_transition_outcome_dist %>%
  ggplot(aes(x = min_entropy_transition_outcome, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over transition distribution (+/-/0) given previous outcome") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))



### PLAYER PREV 2MOVE DISTRIBUTION =============================================

# Combine win count differential and relevant entropy 
entropy_win_diff_comparison_prev_2move_dist = data %>%
  inner_join(player.prev.2move.entropy, by = "player_id") %>%
  group_by(game_id, player_id, entropy.2.player) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(opp_win_count = ifelse(is.na(lag(n, 1)), lead(n, 1), lag(n, 1))) %>%
  group_by(game_id) %>%
  filter(entropy.2.player == min(entropy.2.player)) %>%
  mutate(win_diff = (n - opp_win_count)) %>%
  rename(min_entropy_prev_2moves = entropy.2.player) %>%
  select(game_id, player_id, min_entropy_prev_2moves, win_diff)


# Get regression terms and correlation between min entropy and win count differential
model = lm(data = entropy_win_diff_comparison_prev_2move_dist, 
           win_diff ~ min_entropy_prev_2moves)
summary(model)
cor_vals = cor.test(entropy_win_diff_comparison_prev_2move_dist$min_entropy_prev_2moves, 
         entropy_win_diff_comparison_prev_2move_dist$win_diff)
cor_vals
# Summary: relationship is significant in right direction (cor = 0.263, p = 0.05)

# Generate predictions for input on graph
predictions = data.frame(
  entropy = seq(0.8, 1.6, by = 0.1)
)
predictions = predictions %>%
  mutate(win_diff = model$coefficients[['(Intercept)']] + model$coefficients[['min_entropy_prev_2moves']] * entropy)

labelx = 1.1
labely = 0
cor = round(cor_vals$estimate, 3)
cor_p = round(cor_vals$p.value, 3)


# Scatter plot entropy vals against win count differential
entropy_win_diff_comparison_prev_2move_dist %>%
  ggplot(aes(x = min_entropy_prev_2moves, y = win_diff)) +
  geom_point(aes(color = "prev_2moves"), alpha = 0.75, size = 5) +
  geom_line(data = predictions, aes(x = entropy, y = win_diff), size = 3, color = "red") +
  geom_label(aes(x = labelx, y = labely, label = str_c("r = ", cor, " (p = ", cor_p, ")")), color = "red", size = 6,
             label.padding = unit(0.5, "lines")) +
  labs(x = xlab, y = ylab) +
  # ggtitle("Entropy over moves given previous two moves") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = c("prev_2moves" = "Previous two-move dependency"),
                      begin = 0.2, end = 0.8) +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 32, face = "bold"),
        legend.text = element_text(size = 24, face = "bold"))




### PLAYER-OPPONENT PREV MOVE DISTRIBUTION =====================================

# Combine win count differential and relevant entropy 
entropy_win_diff_comparison_player_opponent_prev_move_dist = data %>%
  inner_join(player.opponent.prev.move.entropy, by = "player_id") %>%
  group_by(game_id, player_id, entropy.2.player.opponent) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(opp_win_count = ifelse(is.na(lag(n, 1)), lead(n, 1), lag(n, 1))) %>%
  group_by(game_id) %>%
  filter(entropy.2.player.opponent == min(entropy.2.player.opponent)) %>%
  mutate(win_diff = (n - opp_win_count)) %>%
  rename(min_entropy_player_opponent_prev_move = entropy.2.player.opponent) %>%
  select(game_id, player_id, min_entropy_player_opponent_prev_move, win_diff)


# Get regression terms and correlation between min entropy and win count differential
model = lm(data = entropy_win_diff_comparison_player_opponent_prev_move_dist, 
           win_diff ~ min_entropy_player_opponent_prev_move)
summary(model)
cor.test(entropy_win_diff_comparison_player_opponent_prev_move_dist$min_entropy_player_opponent_prev_move, 
         entropy_win_diff_comparison_player_opponent_prev_move_dist$win_diff)
# Summary: not significant in wrong direction (cor = -0.015, p = 0.91)
# Looks like an outlier here but not sure it's extreme enough to remove


# Scatter plot entropy vals against win count differential
entropy_win_diff_comparison_player_opponent_prev_move_dist %>%
  ggplot(aes(x = min_entropy_player_opponent_prev_move, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over moves given player and opponent previous move") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))



### TRANSITION PREV-OUTCOME-PREV-TRANSITION DISTRIBUTION =======================

# Combine win count differential and relevant entropy 
entropy_win_diff_comparison_transition_prev_outcome_prev_transition_dist = data %>%
  inner_join(player.transition.prev.transition.prev.outcome.entropy, by = "player_id") %>%
  group_by(game_id, player_id, transition.entropy.2.player.prev.transition.prev.outcome) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(opp_win_count = ifelse(is.na(lag(n, 1)), lead(n, 1), lag(n, 1))) %>%
  group_by(game_id) %>%
  filter(transition.entropy.2.player.prev.transition.prev.outcome == min(transition.entropy.2.player.prev.transition.prev.outcome)) %>%
  mutate(win_diff = (n - opp_win_count)) %>%
  rename(min_entropy_transition_prev_outcome_prev_transition = transition.entropy.2.player.prev.transition.prev.outcome) %>%
  select(game_id, player_id, min_entropy_transition_prev_outcome_prev_transition, win_diff)


# Get regression terms and correlation between min entropy and win count differential
model = lm(data = entropy_win_diff_comparison_transition_prev_outcome_prev_transition_dist, 
           win_diff ~ min_entropy_transition_prev_outcome_prev_transition)
summary(model)
cor.test(entropy_win_diff_comparison_transition_prev_outcome_prev_transition_dist$min_entropy_transition_prev_outcome_prev_transition, 
         entropy_win_diff_comparison_transition_prev_outcome_prev_transition_dist$win_diff)
# Summary: not significant in right direction (cor = 0.115, p = 0.39)


# Scatter plot entropy vals against win count differential
entropy_win_diff_comparison_transition_prev_outcome_prev_transition_dist %>%
  ggplot(aes(x = min_entropy_transition_prev_outcome_prev_transition, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over transition distribution given player's previous transition, previous outcome") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))













