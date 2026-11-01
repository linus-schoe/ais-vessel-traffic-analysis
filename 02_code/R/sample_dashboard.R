# sample_dashboard.R
# Task 4.1 - Interactive leaflet maps of the interval sample.
#   (a) all points, coloured by speed
#   (b) satellite-only points, for comparison

source("02_code/R/functions/api_helpers.R")
library(leaflet)
library(dplyr)
library(htmlwidgets)

# ---- Load the interval sample (Task 3.2); fall back to provided sample.csv ----
sample_path <- "01_data/sample_intervals.csv"
if (!file.exists(sample_path)) sample_path <- "01_data/sample.csv"
points <- read.csv(sample_path) %>%
  filter(!is.na(latitude), !is.na(longitude))

# ---- Shared speed palette (capped at 25 kn so extremes don't wash out scale) ----
speed_pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 25))

# ---- 4.1(a) Full sample map ----
map_all <- build_speed_map(points, "Speed (knots)<br>All records", speed_pal)
saveWidget(map_all, "03_report/graphs/sample_map.html", selfcontained = TRUE)

# ---- 4.1(b) Satellite-only map ----
points_sat <- points %>% filter(collection_type == "satellite")
map_sat <- build_speed_map(points_sat, "Speed (knots)<br>Satellite only", speed_pal)
saveWidget(map_sat, "03_report/graphs/sample_map_satellite.html", selfcontained = TRUE)

cat("Full sample points:", nrow(points), "\n")
cat("Satellite-only points:", nrow(points_sat), "\n")
cat("Saved both maps to 03_report/graphs/\n")
