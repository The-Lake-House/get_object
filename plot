#!/usr/bin/env Rscript

library(tidyr)
library(dplyr)
library(ggplot2)

data <- read.table("results", sep = "\t")
colnames(data) <- 1:ncol(data)
data <- cbind(data, i = 1:nrow(data))

data <- pivot_longer(data, seq(1, ncol(data) - 1), names_to = "rep", values_to = "time")

# Remove outliers (passed timeout)
data <- data[data[["time"]] <= 5000000000, ]

# Change unit from ns to ms
data[["time"]] <- data[["time"]] / 1e6

stats <- data %>%
    group_by(i) %>%
    summarize(n = n(), mean = mean(time), sd = sd(time))

plot <- ggplot(stats, aes(i, mean)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Number of Inserts", y = "Mean Runtime [ms]")

ggsave("get_object.pdf", plot = plot, width = 10)
ggsave("get_object.svg", plot = plot, width = 10)
