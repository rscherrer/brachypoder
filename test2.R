rm(list = ls())

library(tidyverse)
library(brachypoder)

root <- system.file("extdata", "sim-example", package = "brachypoder")

data <- read_individual_data(root)

data %>%
  ggplot(aes(x = time, y = y)) +
  geom_point()
