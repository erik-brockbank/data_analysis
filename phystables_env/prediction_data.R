#' Script for generating (fake) prediction data for use in phystables poster/write-up

rm(list=ls())
setwd("/Users/erikbrockbank/web/vullab/data_analysis/phystables_env/")


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
  plot.title = element_text(face = "bold", size = 32),
  axis.title.y = element_text(face = "bold", size = 32),
  axis.title.x = element_text(face = "bold", size = 32),
  # axis text
  axis.text.x = element_text(size = 24),
  axis.text.y = element_text(size = 20),
  # facet text
  strip.text = element_text(size = 28),
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
  min.rt = match(containment, containment.levels) * increment
  max.rt = match(containment, containment.levels) * increment + length(complexity.levels) * increment
  rt.vals = seq(from = min.rt,
                to = max.rt - 1,
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
             labeller = labeller(containment = containment.labels)) +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 500)) +
  labs(x = "Simulation complexity", y = "Mean response time (ms)") +
  #ggtitle("Response time across complexity, containment levels") +
  default.theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(vjust = 0.65, hjust = 0.5, angle = 45))




### PREDICTIONS: TOPOLOGY ONLY ###
data.top = data.template
increment = 250 # ms used as starting point, will not be displayed numerically
for (containment in containment.levels) {
  min.rt = (length(containment) + 1 - match(containment, containment.levels)) * increment
  max.rt = (length(containment) + 1 - match(containment, containment.levels)) * increment + length(complexity.levels) * increment
  rt.vals = seq(from = max.rt,
                to = min.rt + 1,
                by = -increment)
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
             labeller = labeller(containment = containment.labels)) +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 500)) +
  labs(x = "Simulation complexity", y = "Mean response time (ms)") +
  #ggtitle("Response time across complexity, containment levels") +
  default.theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(vjust = 0.65, hjust = 0.5, angle = 45))


### PREDICTIONS: META-REASONING ###
data.meta = data.template
increment = 250 # ms used as starting point, will not be displayed numerically
for (containment in containment.levels) {
  min.rt = match(containment, containment.levels) * increment
  max.rt = match(containment, containment.levels) * increment + length(complexity.levels) * increment
  rt.vals = seq(from = min.rt,
                to = max.rt - 1,
                by = increment)
  if (match(containment, containment.levels) == length(containment.levels) -1) {
    rt.vals[4] = rt.vals[3] - 100
  }
  if (match(containment, containment.levels) == length(containment.levels)) {
    rt.vals[3] = rt.vals[3] - 200
    rt.vals[4] = rt.vals[2] - 200
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
             labeller = labeller(containment = containment.labels)) +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 500)) +
  labs(x = "Simulation complexity", y = "Mean response time (ms)") +
  #ggtitle("Response time across complexity, containment levels") +
  default.theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(vjust = 0.65, hjust = 0.5, angle = 45))



