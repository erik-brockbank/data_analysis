#' Script for generating (fake) prediction data for use in phystables poster/write-up

rm(list=ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/phystables_env/")
library(tidyverse)

# Set levels and labels for containment, complexity data (should align with real data graphs)
containment.levels = c(1, 2, 3)
complexity.levels = c(0, 1, 2, 3) 
containment.labels = c(
  '1' = "low containment",
  '2' = "medium containment",
  '3' = "high containment"
)
complexity.labels = c(
  '0' = "none",
  '1' = "low",
  '2' = "medium",
  '3' = "high"
)


# Data frame template for generated data
data.template = data.frame(
  'containment' = numeric(), # values from containment.levels above
  'complexity' = numeric(), # values from complexity.levels above
  'response.time' = numeric() # values will be continuous (fake) RTs
)


# Graph theme copied over from analysis script `data_processing.R`
default.theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 64, hjust = 0.5),
  axis.title.y = element_text(face = "bold", size = 48),
  axis.title.x = element_text(face = "bold", size = 48),
  # axis text
  axis.text.y = element_blank(),
  axis.text.x = element_text(face = "bold", size = 24, vjust = 0.65, hjust = 0.5, angle = 45),
  # facet text
  strip.text = element_text(face = "bold", size = 36),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line(color = "black")
)



### PREDICTIONS: SIMULATION ONLY ###
data.sim = data.template
increment = 250 # ms used as starting point, will not be displayed numerically
for (containment in containment.levels) {
  # min.rt = match(containment, containment.levels) * increment
  min.rt = increment
  # max.rt = match(containment, containment.levels) * increment + length(complexity.levels) * increment
  max.rt = length(complexity.levels) * increment
  rt.vals = seq(from = min.rt,
                to = max.rt,
                by = increment)
  data.sim = rbind(data.sim, data.frame(containment = containment,
                                        complexity = complexity.levels,
                                        response.time = rt.vals))
}

data.sim %>%
  ggplot(aes(x = complexity, y = response.time)) +
  geom_point(size = 5, color = "red") +
  geom_line(size = 2, color = "red") +
  scale_x_continuous(labels = complexity.labels) +
  facet_wrap(.~containment,
             scales = "free",
             labeller = labeller(containment = containment.labels),
             strip.position = "right") +
  scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1250, by = increment)) +
  labs(x = "", y = "RT") +
  ggtitle("Simulation only") +
  default.theme +
  theme()




### PREDICTIONS: TOPOLOGY ONLY ###
data.top = data.template
increment = 250 # ms used as starting point, will not be displayed numerically
for (containment in containment.levels) {
  # min.rt = (length(containment) + 1 - match(containment, containment.levels)) * increment
  # max.rt = (length(containment) + 1 - match(containment, containment.levels)) * increment + length(complexity.levels) * increment
  rt.level = (length(containment.levels) + 1 - match(containment, containment.levels)) * increment
  rt.vals = seq(from = rt.level,
                to = rt.level + 1,
                by = increment)
  data.top = rbind(data.top, data.frame(containment = containment,
                                        complexity = complexity.levels,
                                        response.time = rt.vals))
}

data.top %>%
  ggplot(aes(x = complexity, y = response.time)) +
  geom_point(size = 5, color = "red") +
  geom_line(size = 2, color = "red") +
  scale_x_continuous(labels = complexity.labels) +
  facet_wrap(.~containment,
             scales = "free",
             labeller = labeller(containment = containment.labels),
             strip.position = "right") +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = increment)) +
  labs(x = "", y = "RT") +
  ggtitle("Topology only") +
  default.theme


### PREDICTIONS: META-REASONING ###
data.meta = data.template
increment = 250 # ms used as starting point, will not be displayed numerically
for (containment in containment.levels) {
  # min.rt = match(containment, containment.levels) * increment
  min.rt = increment
  # max.rt = match(containment, containment.levels) * increment + length(complexity.levels) * increment
  max.rt = length(complexity.levels) * increment
  rt.vals = seq(from = min.rt,
                to = max.rt + 1,
                by = increment)
  if (match(containment, containment.levels) == length(containment.levels) - 1) {
    rt.vals[4] = rt.vals[3]
  }
  if (match(containment, containment.levels) == length(containment.levels)) {
    rt.vals[3] = rt.vals[2]
    rt.vals[4] = rt.vals[2]
  }
  data.meta = rbind(data.meta, data.frame(containment = containment,
                                        complexity = complexity.levels,
                                        response.time = rt.vals))
}

data.meta %>%
  ggplot(aes(x = complexity, y = response.time)) +
  geom_point(size = 5, color = "red") +
  geom_line(size = 2, color = "red") +
  scale_x_continuous(labels = complexity.labels) +
  facet_wrap(.~containment,
             scales = "free",
             labeller = labeller(containment = containment.labels),
             strip.position = "right") +
  scale_y_continuous(limits = c(0, 1250), breaks = seq(0, 1250, by = increment)) +
  labs(x = "Simulation complexity", y = "RT") +
  ggtitle("Flexible reasoning") +
  default.theme +
  theme()



