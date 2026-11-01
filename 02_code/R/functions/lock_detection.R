# lock_detection.R
# Rule-based heuristic for detecting ship-lock passages in a single vessel's
# AIS track. A lock shows up as a temporally extended, spatially tight cluster
# of near-stationary points (see report Section 3.2.3).

# Haversine great-circle distance in metres between two lat/lon points.
haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000                      # Earth radius in metres
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 +
       cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

# Detect potential lock passages from a vessel track.
# Parameters (thresholds) are exposed and documented so they can be tuned.
detect_lock_passages <- function(track,
                                 speed_thresh     = 1,    # knots: below = "stopped"
                                 max_gap_min      = 30,   # min:  break group on larger time gap
                                 max_gap_m        = 300,  # m:    break group on larger spatial jump
                                 min_duration_min = 5,    # min:  discard groups shorter than this
                                 min_points       = 3) {  # discard groups with fewer points
  # Empty result with the correct columns, used when nothing is detected.
  empty <- data.frame(start_time = as.POSIXct(character()),
                       end_time = as.POSIXct(character()),
                       duration_minutes = numeric(),
                       latitude = numeric(), longitude = numeric(),
                       n_observations = integer())

  # 1. Sort chronologically and parse the timestamp.
  track <- track[order(track$msg_timestamp), ]
  track$ts <- as.POSIXct(track$msg_timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  # 2. Keep only near-stationary points (candidate lock points).
  slow <- track[!is.na(track$speed) & track$speed < speed_thresh, ]
  if (nrow(slow) == 0) return(empty)

  # 3. Group consecutive candidate points; start a new group whenever the
  #    time gap OR the spatial jump to the previous candidate is too large.
  n <- nrow(slow)
  group_id <- integer(n); group_id[1] <- 1
  if (n > 1) {
    for (i in 2:n) {
      dt_min <- as.numeric(difftime(slow$ts[i], slow$ts[i - 1], units = "mins"))
      dist_m <- haversine_m(slow$latitude[i - 1], slow$longitude[i - 1],
                            slow$latitude[i],     slow$longitude[i])
      group_id[i] <- if (dt_min > max_gap_min || dist_m > max_gap_m)
                       group_id[i - 1] + 1 else group_id[i - 1]
    }
  }
  slow$group <- group_id

  # 4 & 5. Summarise each group, then discard implausibly short/small ones.
  passages <- do.call(rbind, lapply(split(slow, slow$group), function(g) {
    data.frame(
      start_time       = min(g$ts),
      end_time         = max(g$ts),
      duration_minutes = round(as.numeric(difftime(max(g$ts), min(g$ts), units = "mins")), 1),
      latitude         = mean(g$latitude),
      longitude        = mean(g$longitude),
      n_observations   = nrow(g)
    )
  }))

  passages <- passages[passages$duration_minutes >= min_duration_min &
                       passages$n_observations  >= min_points, ]
  rownames(passages) <- NULL
  passages
}
