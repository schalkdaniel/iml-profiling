library(dplyr)
library(ggplot2)
library(tidyr)

# This script gives:
#   - bm:               Benchmark data with all changes etc.
#   - experiments:      The full data.frame with all setups
#   - experiments_done: The data.frame with just the finished experiments
source("prepare-data.R")

# Add time and memory for specific calls:
bm = experiments_done %>% appendChanges(call_names = c("\"private$run.ale\"", "\"calculate.ale.num\""), add_relative = TRUE)


## Visualize memory consumption:
## -----------------------------------------

# For the calls:
bm %>%
  group_by(n, p, learner, batch, rep) %>%
  dplyr::summarize(mem_change = mem_change[call == "\"private$run.ale\""] + mem_change[call == "\"calculate.ale.num\""]) %>%
  group_by(n, p, learner, batch) %>%
  dplyr::summarize(med_mem_change = median(mem_change), se_lower = median(mem_change)-sd(mem_change), se_upper = sd(mem_change) + median(mem_change)) %>%
  ggplot(aes(x = n, y = med_mem_change, color = as.factor(p))) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0.2, alpha = 0.5) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold")
    ) +    labs(color = "Number of\nColumns") +
    xlab("Number of Rows\n(log10 Scale)") +
    ylab("Memory Change\n(log10 Scale)") +
    scale_y_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10") +
    facet_grid(paste0("batch: ", batch) ~ learner)


# In total:
bm %>%
  group_by(n, p, learner, batch, rep) %>%
  dplyr::summarize(mem_change = mem_total[1]) %>%
  group_by(n, p, learner, batch) %>%
  dplyr::summarize(med_mem_change = median(mem_change), se_lower = median(mem_change)-sd(mem_change), se_upper = sd(mem_change) + median(mem_change)) %>%
  ggplot(aes(x = n, y = med_mem_change, color = as.factor(p))) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0.2, alpha = 0.5) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold")
    ) +    labs(color = "Number of\nColumns") +
    xlab("Number of Rows\n(log10 Scale)") +
    ylab("Memory Change\n(log10 Scale)") +
    scale_y_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10") +
    facet_grid(paste0("batch: ", batch) ~ learner)



## Visualize a specific setup (e.g. setup number 10):

setup_number = 10L
experiments_done[setup_number,]

load(paste0("profile-data-fe/", experiments_done$hash[setup_number], ".Rda"))

p_data %>%
  mutate(call = rownames(.)) %>%
  filter(mem.diff != 0) %>%
  pivot_longer(cols = c("mem.diff", "time.diff"), values_to = "diff", names_to = "what") %>%
  ggplot(aes(x = call, y = diff)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_grid(what ~ ., scales = "free")
