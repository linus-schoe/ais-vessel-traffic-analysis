# ais_dynamic_sample_points.R
# Task 3.2 - Interval (cluster) sampling of ais_dynamic for 2024-01-24

# Load reusable helper functions (defines base_url, format_ts, fetch_interval)
source("02_code/R/functions/api_helpers.R")

# ---- 1. Build the sampling frame: all 288 five-minute intervals ----
day_start       <- as.POSIXct("2024-01-24 00:00:00", tz = "UTC")
interval_starts <- day_start + seq(0, by = 5 * 60, length.out = 288)
interval_ends   <- interval_starts + 5 * 60

# ---- 2. Randomly select 100 intervals (reproducible via seed) ----
set.seed(2026)
selected <- sample(seq_len(288), size = 100)

# ---- 3. Pull up to 100 obs per selected interval, stop at 10,000 ----
target_n  <- 10000
collected <- list()
n_total   <- 0

for (i in selected) {
  batch <- safe_fetch(interval_starts[i], interval_ends[i], max_obs = 100)
  if (!is.null(batch) && length(batch) > 0 && nrow(batch) > 0) {
    collected[[length(collected) + 1]] <- batch
    n_total <- n_total + nrow(batch)
  }
  cat("Interval", i, "- collected so far:", n_total, "\n")
  if (n_total >= target_n) break
}

# Combine all batches into one data frame
sample_intervals <- do.call(rbind, collected)
cat("Total observations collected:", nrow(sample_intervals), "\n")

# ---- 3.2(b) Save the sample to 01_data ----
write.csv(sample_intervals, "01_data/sample_intervals.csv", row.names = FALSE)
cat("Saved sample_intervals.csv with", nrow(sample_intervals), "rows\n")
