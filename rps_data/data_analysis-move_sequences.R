#'
#' This script tests whether the count of one, two, and three-move sequences
#' is consistent with what would be expected by chance.
#' For example, a three move sequence of 0-0-3 has a 1/9 chance while 1-1-1 has a 2/9 chance
#' 
#' This script should *not* be used for general data overview, cleaning etc. 
#' (that happens in `data_processing.R`) and should not be used for analyses that extend
#' beyond move distribution dependencies.
#' 



setwd("/Users/erikbrockbank/web/vullab/data_analysis/rps_data/")
source('00_data_processing.R') # script used for data processing/cleanup




# ANALYSIS: 1-move sequences
seq.counts1 = data %>%
  filter(player_move != "none") %>%
  count(player_move) %>%
  mutate(pct = n / sum(n),
         pct_expected = c(1/3, 1/3, 1/3)) # calculated by hand

seq.counts1
chisq.test(x = seq.counts1$n, p = seq.counts1$pct_expected)


# ANALYSIS: 2-move sequences
seq.counts2 = data %>%
  group_by(player_id) %>%
  mutate(prev.move = lag(player_move, 1), # one move back (previous move)
         prev.move2 = lag(player_move, 2)) %>% # two moves back
  filter(!is.na(prev.move), !is.na(prev.move2), # lag calls above set NA for lag on first two moves: ignore it here
         player_move != "none", prev.move != "none", prev.move2 != "none") %>%
  group_by(player_id, round_index) %>%
  mutate(rock.count = sum(prev.move == "rock", prev.move2 == "rock"),
         paper.count = sum(prev.move == "paper", prev.move2 == "paper"),
         scissors.count = sum(prev.move == "scissors", prev.move2 == "scissors"),
         min.col = min(c(rock.count, paper.count, scissors.count)),
         mid.col = median(c(rock.count, paper.count, scissors.count)),
         max.col = max(c(rock.count, paper.count, scissors.count)),
         seq.type = paste(min.col, mid.col, max.col, sep = "-")) %>%
  ungroup() %>%
  count(seq.type) %>%
  mutate(pct = n / sum(n),
         pct_expected = c(1/3, 2/3)) # calculated by hand

seq.counts2
chisq.test(x = seq.counts2$n, p = seq.counts2$pct_expected)


# ANALYSIS: 3-move sequences
seq.counts3 = data %>%
  group_by(player_id) %>%
  mutate(prev.move = lag(player_move, 1), # one move back (previous move)
         prev.move2 = lag(player_move, 2), # two moves back
         prev.move3 = lag(player_move, 3)) %>% # three moves back
  filter(!is.na(prev.move), !is.na(prev.move2), !is.na(prev.move3), # lag calls above set NA for lag on first three moves: ignore it here
         player_move != "none", prev.move != "none", prev.move2 != "none", prev.move3 != "none") %>%
  group_by(player_id, round_index) %>%
  mutate(rock.count = sum(prev.move == "rock", prev.move2 == "rock", prev.move3 == "rock"),
         paper.count = sum(prev.move == "paper", prev.move2 == "paper", prev.move3 == "paper"),
         scissors.count = sum(prev.move == "scissors", prev.move2 == "scissors", prev.move3 == "scissors"),
         min.col = min(c(rock.count, paper.count, scissors.count)),
         mid.col = median(c(rock.count, paper.count, scissors.count)),
         max.col = max(c(rock.count, paper.count, scissors.count)),
         seq.type = paste(min.col, mid.col, max.col, sep = "-")) %>%
  ungroup() %>%
  count(seq.type) %>%
  mutate(pct = n / sum(n),
         pct_expected = c((1/9), (6/9), (2/9))) # calculated by hand

seq.counts3
chisq.test(seq.counts3$n, p = seq.counts3$pct_expected)




