# ais_dynamic_sample_stratified.R
# Task 3.3 - Stratified sampling of ais_dynamic for 2024-01-24
# Strata = (ship_type, hour of day)

source("02_code/R/functions/api_helpers.R")
library(dplyr)

day_start <- as.POSIXct("2024-01-24 00:00:00", tz = "UTC")

# ---- 1. Build mmsi -> ship_type lookup from ais_static (cached) ----
lookup_cache <- "01_data/ship_lookup.rds"
if (file.exists(lookup_cache)) {
  ship_lookup <- readRDS(lookup_cache)
  say("Loaded ship_lookup from cache.\n")
} else {
  ship_lookup <- api_get("/ais_static?select=mmsi,ship_type&limit=1000000")
  saveRDS(ship_lookup, lookup_cache)
  say("Saved ship_lookup. Rows:", nrow(ship_lookup), "\n")
}

# ---- 2. Per-vessel hourly record counts for 2024-01-24 (cached) ----
vh_cache <- "01_data/vessel_hour.rds"
if (file.exists(vh_cache)) {
  vessel_hour <- readRDS(vh_cache)
  say("Loaded vessel_hour from cache.\n")
} else {
  say("No cache - querying API hour by hour (slow)...\n")
  hour_counts <- list()
  for (h in 0:23) {
    s <- day_start + h * 3600
    e <- s + 3600
    path <- paste0("/ais_dynamic?select=mmsi,count()",
                   "&msg_timestamp=gte.", format_ts(s),
                   "&msg_timestamp=lt.",  format_ts(e))
    df <- api_get(path)
    if (!is.null(df) && nrow(df) > 0) {
      df$hour <- h
      hour_counts[[length(hour_counts) + 1]] <- df
    }
    say("Hour", h, "-", if (is.null(df)) 0 else nrow(df), "vessels\n")
  }
  vessel_hour <- do.call(rbind, hour_counts) %>%
    left_join(ship_lookup, by = "mmsi")
  saveRDS(vessel_hour, vh_cache)
  say("Saved vessel_hour.\n")
}

# ---- Build strata (ship_type, hour) by aggregating vessel_hour ----
strata <- vessel_hour %>%
  group_by(ship_type, hour) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  arrange(desc(n))

say("Strata:", nrow(strata), "| Total records:", sum(strata$n), "\n")

# Data-quality check: records with unreported ship_type (see Task 2.3)
say("Strata with NA ship_type:", sum(is.na(strata$ship_type)), "\n")
print(strata %>% filter(is.na(ship_type)) %>% summarise(na_records = sum(n)))

# ---- 3.3(b) Proportional allocation of 10,000 observations ----
total_sample  <- 10000
total_records <- sum(strata$n)
alloc <- strata %>%
  mutate(raw  = total_sample * n / total_records,
         base = pmax(1, round(raw)),
         base = pmin(base, n))
diff <- total_sample - sum(alloc$base)
if (diff != 0) {
  idx <- order(alloc$n, decreasing = TRUE)[1:abs(diff)]
  alloc$base[idx] <- alloc$base[idx] + sign(diff)
}
alloc <- alloc %>% rename(allocation = base) %>% select(ship_type, hour, n, allocation)
say("Total allocated:", sum(alloc$allocation), "\n")

# ---- 3.3(c) Retrieve the allocated records from each stratum (cached) ----
# True random row retrieval is not available through PostgREST, so a two-stage
# random selection (random vessels + random subsample of their records) is used
# as a documented approximation.
sample_cache <- "01_data/sample_stratified.rds"
if (file.exists(sample_cache)) {
  sample_stratified <- readRDS(sample_cache)
  say("Loaded stratified sample from cache.\n")
} else {
  set.seed(2026)
  parts <- list()
  for (r in seq_len(nrow(alloc))) {
    st   <- alloc$ship_type[r]
    hh   <- alloc$hour[r]
    need <- alloc$allocation[r]

    if (is.na(st)) {
      pool <- vessel_hour %>% filter(hour == hh, is.na(ship_type))
    } else {
      pool <- vessel_hour %>% filter(hour == hh, ship_type == st)
    }

    pool <- pool[sample(nrow(pool)), ]
    pool$cum <- cumsum(pool$count)
    n_keep <- which(pool$cum >= need)[1]
    n_keep <- min(n_keep, 200)           # safety cap on URL length
    keep   <- pool[seq_len(n_keep), ]

    say(sprintf("Stratum %d/%d | %s h%d | need %d | vessels %d\n",
                r, nrow(alloc),
                ifelse(is.na(st), "NA", st), hh, need, n_keep))

    s <- day_start + hh * 3600
    e <- s + 3600
    batch <- fetch_stratum(keep$mmsi, s, e, sum(keep$count))
    if (!is.null(batch) && nrow(batch) > 0) {
      if (nrow(batch) > need) batch <- batch[sample(nrow(batch), need), ]
      batch$ship_type <- st
      batch$hour      <- hh
      parts[[length(parts) + 1]] <- batch
    } else {
      say("  -> no data returned for this stratum\n")
    }
  }
  sample_stratified <- bind_rows(parts)
  saveRDS(sample_stratified, sample_cache)
  say("Saved stratified sample.\n")
}

say("Stratified sample rows:", nrow(sample_stratified), "\n")
print(head(sample_stratified))

# ---- 3.3(d) Save the stratified sample to 01_data ----
write.csv(sample_stratified, "01_data/sample_stratified.csv", row.names = FALSE)
say("Saved sample_stratified.csv with", nrow(sample_stratified), "rows\n")
