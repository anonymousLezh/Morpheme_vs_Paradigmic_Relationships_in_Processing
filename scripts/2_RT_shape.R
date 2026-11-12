#####################
#####################
# Anonymous
# Fall 2026
##################### 
# Explore the shape of response time
##################### 

source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "setup_r_libs.R"))

library(dplyr)
library(tidyverse)
library(ggplot2)

# Load data
df_TwoSuff <- read.csv("../output/df_2suff-clean.csv")
df_ThreeSuff <- read.csv("../output/df_3suff-clean.csv")

###########
# Plot the two datasets
###########
df_combined <- bind_rows(
  df_ThreeSuff %>% mutate(Group = "Three Suffixes"),
  df_TwoSuff %>% mutate(Group = "Two Suffixes")
)

# 2. Plot the overlapping histogram
rt_plot_by_dataset <- ggplot(df_combined, aes(x = rt, fill = Group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "white") +
  scale_fill_manual(values = c( "Three Suffixes" = "#B86018","Two Suffixes" = "#BABCAA")) +
  labs(
    title = "Distribution of Reaction Times by Dataset",
    x = "Reaction Time (ms)",
    y = "Count",
    fill = "Dataset"
  ) +
  theme_minimal()

rt_plot_by_dataset
ggsave(filename = "../figures/RT_by_dataset.png", plot = rt_plot_by_dataset, width = 8, height = 6, dpi = 300)

###########
# Plot by experimental version
###########

# Combine datasets
df_pooled <- bind_rows(df_TwoSuff, df_ThreeSuff)

rt_plot_by_exp <- ggplot(df_pooled, aes(x = rt, fill = factor(exp))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "white") +
  scale_fill_manual(values = c("#537B58", "#D17089")) +
  labs(
    title = "Distribution of Reaction Times by Experimental version",
    x = "Reaction Time (ms)",
    y = "Count",
    fill = "Experiment (exp)"
  ) +
  theme_minimal()

rt_plot_by_exp
ggsave(filename = "../figures/RT_by_expVersion.png", plot = rt_plot_by_exp, width = 8, height = 6, dpi = 300)
