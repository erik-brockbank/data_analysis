#'
#' This script looks at whether people are sensitive to the exploitability of their opponents.
#' We plot correlation between minimum entropy within a given dyad for various entropy measures
#' and win count differential. If one player in a dyad has a very low entropy, then that dyad
#' should have a *high* win count differential because their opponent should detect the dependency.
#' If both players have high entropy on a given measure, then we should not expect high
#' win count differential
#' 



rm(list=ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")
source('02_data_analysis-transition_entropy.R') # NB: nested sourcing here. this sources 01 which sources 00
source('data_analysis_expected_value.R')

# GLOBALS
xlab = "Minimum within-dyad Shannon entropy (S)"
ylab = "Dyad win count differential"

xlab_win_diff = "Maximum expected win count differential"


# Get data structure that combines win count differential and meaningful entropy values
entropy_win_diff_comparison = data %>%
  # entropy over player move distributions
  inner_join(player.entropy, by = "player_id") %>%
  # entropy over player move distributions given previous move
  inner_join(player.prev.move.entropy, by = "player_id") %>%
  # entropy over player move distributions given opponent previous move
  inner_join(opponent.prev.move.entropy, by = "player_id") %>%
  # entropy over player transitions
  inner_join(player.transition.entropy, by = "player_id") %>%
  # entropy for distribution of transitions given player's previous outcome (this is among highest variances)
  inner_join(player.transition.outcome.entropy, by = "player_id") %>%
  # entropy for distribution of moves given player's previous two moves
  inner_join(player.prev.2move.entropy, by = "player_id") %>%
  # entropy for distribution of moves given player's previous move, opponent previous  move
  inner_join(player.opponent.prev.move.entropy, by = "player_id") %>%
  # entropy for distribution of transitions given player's previous transition, previous outcome
  inner_join(player.transition.prev.transition.prev.outcome.entropy, by = "player_id") %>%
  group_by(game_id, player_id, 
           entropy.0, entropy.1.player, entropy.1.opponent, 
           transition.entropy.0, transition.entropy.1.player.outcome, 
           entropy.2.player, entropy.2.player.opponent,
           transition.entropy.2.player.prev.transition.prev.outcome) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(opp_win_count = lag(n, 1),
         min_entropy_move_dist = min(entropy.0),
         min_entropy_prev_move = min(entropy.1.player),
         min_entropy_opponent_prev_move = min(entropy.1.opponent),
         min_entropy_transition = min(transition.entropy.0),
         min_entropy_transition_prev_outcome = min(transition.entropy.1.player.outcome),
         min_entropy_prev_2moves = min(entropy.2.player),
         min_entropy_player_opponent_prev_move = min(entropy.2.player.opponent),
         min_entropy_transition_prev_outcome_prev_transition = min(transition.entropy.2.player.prev.transition.prev.outcome)) %>%
  filter(!is.na(opp_win_count)) %>%
  mutate(win_diff = abs(n - opp_win_count)) %>%
  select(game_id, player_id, 
         min_entropy_move_dist, min_entropy_prev_move, min_entropy_opponent_prev_move,
         min_entropy_transition, min_entropy_transition_prev_outcome,
         min_entropy_prev_2moves, min_entropy_player_opponent_prev_move,
         min_entropy_transition_prev_outcome_prev_transition,
         win_diff)



# Get data structure that combines win count differential and meaningful expected win count differential values
expected_win_empirical_win_diff_comparison = data %>%
  # exp. win diff over player move distributions
  inner_join(player_utils, by = "player_id") %>%
  # exp. win diff over player move distributions given previous move
  inner_join(player_prev_move_utils, by = "player_id") %>%
  # exp. win diff over player move distributions given opponent previous move
  inner_join(opponent_prev_move_utils, by = "player_id") %>%
  # exp. win diff over player transitions
  inner_join(player_transition_utils, by = "player_id") %>%
  # exp. win diff for distribution of transitions given player's previous outcome (this is among highest variances)
  inner_join(player_transition_prev_outcome_utils, by = "player_id") %>%
  group_by(game_id, player_id, win_diff.x, win_diff.y, win_diff.x.x, win_diff.y.y, win_diff) %>%
  count(win_count = player_outcome == "win") %>%
  filter(win_count == TRUE) %>%
  group_by(game_id) %>%
  mutate(opp_win_count = lag(n, 1),
         exp_win_diff_move_dist = max(win_diff.x),
         exp_win_diff_prev_move = max(win_diff.y),
         exp_win_diff_opponent_prev_move = max(win_diff.x.x),
         exp_win_diff_transition = max(win_diff.y.y),
         exp_win_diff_transition_prev_outcome = max(win_diff)) %>%
  filter(!is.na(opp_win_count)) %>%
  mutate(win_diff = abs(n - opp_win_count)) %>%
  select(game_id, player_id, 
         exp_win_diff_move_dist, exp_win_diff_prev_move, exp_win_diff_opponent_prev_move,
         exp_win_diff_transition, exp_win_diff_transition_prev_outcome,
         win_diff)


# Scatter plot entropy vals against win count differential

## 1. Entropy over move distribution
entropy_win_diff_comparison %>%
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


model = lm(data = entropy_win_diff_comparison, win_diff ~ min_entropy_move_dist)
summary(model)

cor.test(entropy_win_diff_comparison$min_entropy_move_dist, entropy_win_diff_comparison$win_diff)

# Summary: not significant (likely not enough variance in move distribution entropy)


## 1.5 Expected win differential over move distribution
expected_win_empirical_win_diff_comparison %>%
  ggplot(aes(x = exp_win_diff_move_dist, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab_win_diff, y = ylab) +
  ggtitle("Expected win differential over move distribution") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))


model = lm(data = expected_win_empirical_win_diff_comparison, win_diff ~ exp_win_diff_move_dist)
summary(model)

cor.test(expected_win_empirical_win_diff_comparison$win_diff, expected_win_empirical_win_diff_comparison$exp_win_diff_move_dist)

# Summary: not significant: cor = 0.146 (p = 0.275)


## 2. Entropy over transition distribution
entropy_win_diff_comparison %>%
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


model = lm(data = entropy_win_diff_comparison, win_diff ~ min_entropy_transition)
summary(model)

cor.test(entropy_win_diff_comparison$min_entropy_transition, entropy_win_diff_comparison$win_diff)

# Summary: significant, cor = -0.283 (p = 0.03)

## 2.5 Expected win differential over transition distribution
expected_win_empirical_win_diff_comparison %>%
  ggplot(aes(x = exp_win_diff_transition, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab_win_diff, y = ylab) +
  ggtitle("Expected win differential over transition distribution") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))


model = lm(data = expected_win_empirical_win_diff_comparison, win_diff ~ exp_win_diff_transition)
summary(model)

cor.test(expected_win_empirical_win_diff_comparison$win_diff, expected_win_empirical_win_diff_comparison$exp_win_diff_transition)

# Summary: not significant but close: cor = 0.232 (p = 0.08)


## 3. Entropy over move distribution given opponent's previous move
entropy_win_diff_comparison %>%
  ggplot(aes(x = min_entropy_opponent_prev_move, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 2) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over move distribution given opponent's previous move") +
  individ_plot_theme


model = lm(data = entropy_win_diff_comparison, win_diff ~ min_entropy_opponent_prev_move)
summary(model)

cor.test(entropy_win_diff_comparison$min_entropy_opponent_prev_move, entropy_win_diff_comparison$win_diff)

# Summary: significant cor = -0.397 (p = 0.002). Large outlier.
# Remove outlier
entropy_win_diff_tmp = entropy_win_diff_comparison %>%
  ungroup() %>%
  filter(min_entropy_opponent_prev_move != min(min_entropy_opponent_prev_move))

# Re-run analysis
entropy_win_diff_tmp %>%
  ggplot(aes(x = min_entropy_opponent_prev_move, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 2) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over move distribution given opponent's previous move (outlier removed)") +
  individ_plot_theme

model = lm(data = entropy_win_diff_tmp, win_diff ~ min_entropy_opponent_prev_move)
summary(model)

cor.test(entropy_win_diff_tmp$min_entropy_opponent_prev_move, entropy_win_diff_tmp$win_diff)

# Summary: still significant, cor = -0.298 (p = 0.025)



## 3.5 Expected win differential over move distribution given opponent's previous move
expected_win_empirical_win_diff_comparison %>%
  ggplot(aes(x = exp_win_diff_opponent_prev_move, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab_win_diff, y = ylab) +
  ggtitle("Expected win differential over move given opponent previous move") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))


model = lm(data = expected_win_empirical_win_diff_comparison, win_diff ~ exp_win_diff_opponent_prev_move)
summary(model)

cor.test(expected_win_empirical_win_diff_comparison$win_diff, expected_win_empirical_win_diff_comparison$exp_win_diff_opponent_prev_move)

# Summary: highly significant: cor = 0.41 (p = 0.001). Big outlier. 

# Remove outlier
expected_win_diff_tmp = expected_win_empirical_win_diff_comparison %>%
  ungroup() %>%
  filter(exp_win_diff_opponent_prev_move != max(exp_win_diff_opponent_prev_move))

expected_win_diff_tmp %>%
  ggplot(aes(x = exp_win_diff_opponent_prev_move, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab_win_diff, y = ylab) +
  ggtitle("Expected win differential over move given opponent previous move (outlier removed)") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))


model = lm(data = expected_win_diff_tmp, win_diff ~ exp_win_diff_opponent_prev_move)
summary(model)

cor.test(expected_win_diff_tmp$win_diff, expected_win_diff_tmp$exp_win_diff_opponent_prev_move)

# Summary: still significant: cor = 0.31 (p = 0.02)


## 4. Entropy over move distribution given player's previous move
model = lm(data = entropy_win_diff_comparison, win_diff ~ min_entropy_prev_move)
model_sum = summary(model)

cor.test(entropy_win_diff_comparison$min_entropy_prev_move, entropy_win_diff_comparison$win_diff)

# Generate predictions for input on graph
predictions = data.frame(
  entropy = seq(0, 2, by = 0.1)
)
predictions = predictions %>%
  mutate(win_diff = model$coefficients[['(Intercept)']] + model$coefficients[['min_entropy_prev_move']] * entropy)
         #lower_ci = (model$coefficients[['(Intercept)']] - model_sum$coefficients[3]) + (model$coefficients[['min_entropy_prev_move']] - model_sum$coefficients[4]) * entropy,
         #upper_ci = (model$coefficients[['(Intercept)']] + model_sum$coefficients[3]) + (model$coefficients[['min_entropy_prev_move']] + model_sum$coefficients[4]) * entropy)

entropy_win_diff_comparison %>%
  ggplot(aes(x = min_entropy_prev_move, y = win_diff)) +
  geom_point(aes(color = "prev_move"), alpha = 0.75, size = 5) +
  geom_line(data = predictions, aes(x = entropy, y = win_diff), size = 3, color = "red") +
  #geom_ribbon(data = predictions, aes(x = entropy, ymin = lower_ci, ymax = upper_ci), stat = "identity", position = "identity", alpha = 0.75, color = "red", fill = "red") +
  labs(x = xlab, y = ylab) +
  xlim(c(1.2, 1.6)) +
  ylim(c(0, 60)) +
  ggtitle("Move dependency predicts win count differential") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = c("prev_move" = "Previous move dependency"),
                      begin = 0.2, end = 0.8) +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 32, face = "bold"),
        legend.text = element_text(size = 24, face = "bold")
        )



# Summary: significant, cor = -0.294 (p = 0.025). No single outlier as above.


## 4.5 Expected win differential over move distribution given player's previous move
expected_win_empirical_win_diff_comparison %>%
  ggplot(aes(x = exp_win_diff_prev_move, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab_win_diff, y = ylab) +
  ggtitle("Expected win differential over move given previous move") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))


model = lm(data = expected_win_empirical_win_diff_comparison, win_diff ~ exp_win_diff_prev_move)
summary(model)

cor.test(expected_win_empirical_win_diff_comparison$win_diff, expected_win_empirical_win_diff_comparison$exp_win_diff_prev_move)

# Summary: not significant: cor = 0.2 (p = 0.43)


## 5. Entropy over transition distribution given previous outcome
entropy_win_diff_comparison %>%
  ggplot(aes(x = min_entropy_transition_prev_outcome, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 2) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over transition distribution given previous outcome") +
  individ_plot_theme


model = lm(data = entropy_win_diff_comparison, win_diff ~ min_entropy_transition_prev_outcome)
summary(model)

cor.test(entropy_win_diff_comparison$min_entropy_transition_prev_outcome, entropy_win_diff_comparison$win_diff)

# Summary: significant, cor = -0.429 (p = 0.0008). Large outlier.
# Remove outlier
entropy_win_diff_tmp = entropy_win_diff_comparison %>%
  ungroup() %>%
  filter(min_entropy_transition_prev_outcome != min(min_entropy_transition_prev_outcome))

# Re-run analysis
entropy_win_diff_tmp %>%
  ggplot(aes(x = min_entropy_transition_prev_outcome, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 2) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over transition distribution given previous outcome (outlier removed)") +
  individ_plot_theme

model = lm(data = entropy_win_diff_tmp, win_diff ~ min_entropy_transition_prev_outcome)
summary(model)

cor.test(entropy_win_diff_tmp$min_entropy_transition_prev_outcome, entropy_win_diff_tmp$win_diff)

# Summary: still significant, cor = -0.339 (p = 0.0099)


## 5.5 Expected win differential over transition distribution given previous outcome
expected_win_empirical_win_diff_comparison %>%
  ggplot(aes(x = exp_win_diff_transition_prev_outcome, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab_win_diff, y = ylab) +
  ggtitle("Expected win differential over transiiton given previous outcome") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))


model = lm(data = expected_win_empirical_win_diff_comparison, win_diff ~ exp_win_diff_transition_prev_outcome)
summary(model)

cor.test(expected_win_empirical_win_diff_comparison$win_diff, expected_win_empirical_win_diff_comparison$exp_win_diff_transition_prev_outcome)

# Summary: highly significant: cor = 0.37 (p = 0.004). Big outlier.

# Remove outlier
expected_win_diff_tmp = expected_win_empirical_win_diff_comparison %>%
  ungroup() %>%
  filter(exp_win_diff_transition_prev_outcome != max(exp_win_diff_transition_prev_outcome))

expected_win_diff_tmp %>%
  ggplot(aes(x = exp_win_diff_transition_prev_outcome, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 5) +
  labs(x = xlab_win_diff, y = ylab) +
  ggtitle("Expected win differential over transiiton given previous outcome (outlier removed)") +
  individ_plot_theme +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 36, face = "bold"))


model = lm(data = expected_win_diff_tmp, win_diff ~ exp_win_diff_transition_prev_outcome)
summary(model)

cor.test(expected_win_diff_tmp$win_diff, expected_win_diff_tmp$exp_win_diff_transition_prev_outcome)

# Summary: still significant (barely): cor = 0.261 (p = 0.05)


## 6. Entropy over move distribution given previous two moves
entropy_win_diff_comparison %>%
  ggplot(aes(x = min_entropy_prev_2moves, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 2) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over move distribution given previous two moves") +
  individ_plot_theme


model = lm(data = entropy_win_diff_comparison, win_diff ~ min_entropy_prev_2moves)
summary(model)

cor.test(entropy_win_diff_comparison$min_entropy_prev_2moves, entropy_win_diff_comparison$win_diff)

# Summary: significant, cor = -0.348 (p = 0.008).


## 7. Entropy over move distribution given player's previous move, opponent's previous move
entropy_win_diff_comparison %>%
  ggplot(aes(x = min_entropy_player_opponent_prev_move, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 2) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over move distribution given player's previous move, opponent's previous move") +
  individ_plot_theme


model = lm(data = entropy_win_diff_comparison, win_diff ~ min_entropy_player_opponent_prev_move)
summary(model)

cor.test(entropy_win_diff_comparison$min_entropy_player_opponent_prev_move, entropy_win_diff_comparison$win_diff)

# Summary: significant, cor = -0.457 (p = 0.0003).



## 8. Entropy over transition distribution given player's previous transition, previous  outcome
entropy_win_diff_comparison %>%
  ggplot(aes(x = min_entropy_transition_prev_outcome_prev_transition, y = win_diff)) +
  geom_point(color = "blue", alpha = 0.75, size = 2) +
  labs(x = xlab, y = ylab) +
  ggtitle("Entropy over transition distribution given player's previous transition, previous outcome") +
  individ_plot_theme


model = lm(data = entropy_win_diff_comparison, win_diff ~ min_entropy_transition_prev_outcome_prev_transition)
summary(model)

cor.test(entropy_win_diff_comparison$min_entropy_transition_prev_outcome_prev_transition, entropy_win_diff_comparison$win_diff)

# Summary: highly significant, cor = -0.495 (p = 7.793e-05).
