# compare_samples.R
# Task 3.3(e) - Compare the interval and stratified samples on
# ship_type, speed, and collection_type. Saves plots to 03_report/graphs.

source("02_code/R/functions/api_helpers.R")
library(dplyr)
library(ggplot2)
library(tidyr)

# ---- 1. Load both samples ----
interval   <- read.csv("01_data/sample_intervals.csv")
stratified <- read.csv("01_data/sample_stratified.csv")

# ---- 2. Enrich the interval sample with ship_type (local join) ----
# The interval sample was pulled from ais_dynamic without ship_type, so we
# attach it from the cached static lookup. The stratified sample already has it.
ship_lookup <- readRDS("01_data/ship_lookup.rds")
interval <- interval %>% left_join(ship_lookup, by = "mmsi")

# ---- 3. Tag each sample and combine for plotting ----
interval$sample   <- "Interval"
stratified$sample <- "Stratified"

common_cols <- c("mmsi", "speed", "collection_type", "ship_type", "sample")
combined <- bind_rows(
  interval[, common_cols],
  stratified[, common_cols]
)

# Consistent greyscale fills for the two samples across all plots (dark vs
# mid-grey) for a clean, print-friendly look with good contrast.
sample_colours <- c("Interval" = "grey25", "Stratified" = "grey65")

# Output directory for graphs
graph_dir <- "03_report/graphs"
dir.create(graph_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 4a. ship_type: grouped bars of proportions ----
ship_df <- combined %>%
  mutate(ship_type = ifelse(is.na(ship_type), "(not reported)", ship_type)) %>%
  count(sample, ship_type) %>%
  group_by(sample) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_ship <- ggplot(ship_df, aes(x = reorder(ship_type, -prop), y = prop, fill = sample)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = sample_colours) +
  labs(title = "Ship type distribution: interval vs stratified sample",
       x = "Ship type", y = "Proportion within sample", fill = "Sample") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(graph_dir, "compare_ship_type.png"),
       p_ship, width = 9, height = 5, dpi = 300)

# ---- 4b. collection_type: grouped bars of proportions ----
coll_df <- combined %>%
  count(sample, collection_type) %>%
  group_by(sample) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_coll <- ggplot(coll_df, aes(x = collection_type, y = prop, fill = sample)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = sample_colours) +
  labs(title = "Collection type distribution: interval vs stratified sample",
       x = "Collection type", y = "Proportion within sample", fill = "Sample") +
  theme_minimal(base_size = 12)

ggsave(file.path(graph_dir, "compare_collection_type.png"),
       p_coll, width = 7, height = 5, dpi = 300)

# ---- 4c. speed: overlaid density curves ----
p_speed <- ggplot(combined, aes(x = speed, colour = sample, fill = sample)) +
  geom_density(alpha = 0.25) +
  scale_colour_manual(values = sample_colours) +
  scale_fill_manual(values = sample_colours) +
  coord_cartesian(xlim = c(0, 30)) +   # focus on the plausible speed range
  labs(title = "Speed distribution: interval vs stratified sample",
       x = "Speed (knots)", y = "Density", colour = "Sample", fill = "Sample") +
  theme_minimal(base_size = 12)

ggsave(file.path(graph_dir, "compare_speed.png"),
       p_speed, width = 8, height = 5, dpi = 300)

cat("Saved 3 comparison plots to", graph_dir, "\n")
