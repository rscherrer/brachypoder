rm(list = ls())

library(tidyverse)
library(brachypoder)

theme_set(theme_classic())

# Set folder where to find the data
root <- system.file("extdata", "sim-example", package = "brachypoder")

# Read the data
data <- read_data(root, variables = c("time", "patchsizes"), ncols = c(-10, 1))

# Reshape the data
data <- data %>%
  mutate(deme = rep(1:5, nrow(data) / 5), patch = rep(c(FALSE, TRUE), nrow(data) / 2)) %>%
  rename(n = "patchsizes")

# Relabel
data <- data %>% mutate(patch_lab = if_else(patch, "Facilitated", "Unfacilitated"))

# Plot
plot <- data %>%
  ggplot(aes(x = time, y = n, group = interaction(deme, patch), color = patch_lab)) +
  geom_point() +
  geom_line() +
  xlab("Time (generations)") +
  ylab("No. individuals") +
  labs(color = NULL) +
  scale_color_manual(values = c("gray10", "gray60"))

ggsave("test.png", plot, width = 5, height = 3, dpi = 300)
