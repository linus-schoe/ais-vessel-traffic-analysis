# api_helpers.R
# Reusable helper functions for querying the AIS PostgREST API.

library(httr)
library(jsonlite)

# Base URL for the PostgREST API
base_url <- "https://aidaho-edu.uni-hohenheim.de/aisdb"

# Print a message and flush immediately, so output appears in real time
# even when the script is run non-interactively via Rscript.
say <- function(...) { cat(...); flush.console() }

# Format a POSIXct time as ISO-8601 UTC, as expected by PostgREST.
format_ts <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Fetch up to max_obs dynamic records from a single time interval.
# Uses a 30s timeout and forces HTTP/1.1 to avoid HTTP/2 stream errors.
fetch_interval <- function(start_time, end_time, max_obs = 100) {
  url <- paste0(
    base_url, "/ais_dynamic",
    "?select=mmsi,msg_timestamp,latitude,longitude,speed,course,heading,status,collection_type",
    "&msg_timestamp=gte.", format_ts(start_time),
    "&msg_timestamp=lt.",  format_ts(end_time),
    "&limit=", max_obs
  )
  res <- GET(url, timeout(30), config(http_version = 2))  # 2 = HTTP/1.1
  fromJSON(content(res, as = "text"))
}

# Fetch one interval with one retry; returns NULL on repeated failure.
safe_fetch <- function(start_time, end_time, max_obs = 100) {
  for (attempt in 1:2) {
    result <- tryCatch(
      fetch_interval(start_time, end_time, max_obs),
      error = function(e) {
        say("  (attempt", attempt, "failed:", conditionMessage(e), ")\n")
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }
  NULL
}

# Generic GET against the API with timeout, HTTP/1.1, and one retry.
# Returns a parsed data frame, or NULL if both attempts fail.
api_get <- function(path) {
  url <- paste0(base_url, path)
  for (attempt in 1:2) {
    result <- tryCatch({
      res <- GET(url, timeout(120), config(http_version = 2))
      fromJSON(content(res, as = "text"))
    }, error = function(e) {
      say("  (attempt", attempt, "failed:", conditionMessage(e), ")\n")
      NULL
    })
    if (!is.null(result)) return(result)
  }
  NULL
}

# Fetch dynamic records for a set of MMSIs within a time window.
# Used for stratified sampling (records of selected vessels in one hour).
fetch_stratum <- function(mmsi_vec, start_time, end_time, limit_n) {
  path <- paste0(
    "/ais_dynamic?select=mmsi,msg_timestamp,latitude,longitude,speed,course,heading,status,collection_type",
    "&mmsi=in.(", paste(mmsi_vec, collapse = ","), ")",
    "&msg_timestamp=gte.", format_ts(start_time),
    "&msg_timestamp=lt.",  format_ts(end_time),
    "&limit=", limit_n
  )
  api_get(path)
}

# Build a speed-coloured leaflet map from a set of AIS points.
# Expects columns: longitude, latitude, speed, mmsi, collection_type, msg_timestamp.
# `speed_pal` is a colorNumeric palette passed in by the caller.
build_speed_map <- function(df, title, speed_pal) {
  leaflet(df) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng = ~longitude, lat = ~latitude,
      # Thin dark outline keeps pale (slow-speed) markers visible on the
      # light CartoDB.Positron basemap; fill carries the speed colour.
      radius = 3, stroke = TRUE, color = "#333333", weight = 0.4, opacity = 0.6,
      fillColor = ~speed_pal(pmin(speed, 25)), fillOpacity = 0.8,
      popup = ~paste0("MMSI: ", mmsi,
                      "<br>Speed: ", speed, " kn",
                      "<br>Collection: ", collection_type,
                      "<br>Time: ", msg_timestamp)
    ) %>%
    addLegend("bottomright", pal = speed_pal, values = c(0, 25),
              title = title, opacity = 0.9)
}

# Fetch dynamic records for a single MMSI, optionally within a time window.
# Returns a data frame ordered by msg_timestamp.
fetch_vessel_track <- function(mmsi, start_time = NULL, end_time = NULL, limit_n = 100000) {
  path <- paste0("/ais_dynamic?select=mmsi,msg_timestamp,latitude,longitude,speed,course,heading,status,collection_type",
                 "&mmsi=eq.", mmsi)
  if (!is.null(start_time)) path <- paste0(path, "&msg_timestamp=gte.", format_ts(start_time))
  if (!is.null(end_time))   path <- paste0(path, "&msg_timestamp=lt.",  format_ts(end_time))
  path <- paste0(path, "&order=msg_timestamp.asc&limit=", limit_n)
  api_get(path)
}
