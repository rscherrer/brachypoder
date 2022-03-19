rm(list = ls())

library(tidyverse)
library(brachypoder)

theme_set(theme_classic())

# Set folder where to find the data
root <- "extdata/sim-example"

# Read the data
data <- read_data(root, variables = c("time", "patchsizes"), ncols = c(-10, 1))

# Reshape the data
data <- data %>%
  mutate(deme = rep(1:5, nrow(data) / 5), patch = rep(c(FALSE, TRUE), nrow(data) / 2)) %>%
  rename(n = "patchsizes")

# Relabel
data <- data %>% mutate(patch_lab = if_else(patch, "Facilitated", "Unfacilitated"))

# Read the proportion of good patches
pgood <- read_parameters(root)$pgood
pgood <- pgood[-1]

# Add information about the coverage in good patches
data <- data %>%
  group_by(deme) %>%
  nest() %>%
  ungroup() %>%
  mutate(pgood = pgood) %>%
  unnest(data) %>%
  mutate(patch_size = if_else(patch, pgood, 1 - pgood))

# Plot
plot <- data %>%
  ggplot(aes(x = time, y = n, group = interaction(deme, patch), color = patch_lab, alpha = patch_size)) +
  geom_line() +
  xlab("Time (generations)") +
  ylab("No. individuals") +
  labs(color = NULL, alpha = "% cover") +
  scale_color_manual(values = c("gray10", "gray60"))

plot
