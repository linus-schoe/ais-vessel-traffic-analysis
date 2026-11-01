# ais_dynamic_individual_paths.R
# Task 4.2 - Query, process, and visualise AIS tracks of individual vessels.

source("02_code/R/functions/api_helpers.R")
library(leaflet)
library(dplyr)
library(htmlwidgets)

# ---- 4.2(a) Inspect MMSI 2579999 ----
v_2579999 <- fetch_vessel_track(2579999)

cat("Records for MMSI 2579999:", nrow(v_2579999), "\n")
cat("Time span:", as.character(min(v_2579999$msg_timestamp)),
    "to", as.character(max(v_2579999$msg_timestamp)), "\n")
cat("Distinct positions:", nrow(unique(v_2579999[, c("latitude","longitude")])), "\n")
print(summary(v_2579999[, c("latitude","longitude","speed")]))
print(head(v_2579999))

# ---- 4.2(b) Examine MMSI 563040400 ----
# First, inspect the overall extent of this vessel's data to choose a window.
v_563 <- fetch_vessel_track(563040400)
cat("Records:", nrow(v_563), "\n")
cat("Time span:", as.character(min(v_563$msg_timestamp)),
    "to", as.character(max(v_563$msg_timestamp)), "\n")
cat("Lat range:", round(range(v_563$latitude, na.rm = TRUE), 3), "\n")
cat("Lon range:", round(range(v_563$longitude, na.rm = TRUE), 3), "\n")
print(summary(v_563$speed))

# ---- 4.2(b) Visualise MMSI 563040400 track ----
# Vessel sits in the Port of Constanta area (Romania) for ~4 days.
# Build a focused map of its movements using the shared speed-map helper.
v_563 <- v_563 %>% filter(!is.na(latitude), !is.na(longitude))
speed_pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 12))

map_563 <- leaflet(v_563) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    # Thin dark outline keeps pale (slow-speed) markers visible on the
    # light CartoDB.Positron basemap; fill carries the speed colour.
    radius = 3, stroke = TRUE, color = "#333333", weight = 0.4, opacity = 0.6,
    fillColor = ~speed_pal(pmin(speed, 12)), fillOpacity = 0.8,
    popup = ~paste0("Time: ", msg_timestamp, "<br>Speed: ", speed, " kn")
  ) %>%
  addLegend("bottomright", pal = speed_pal, values = c(0, 12),
            title = "Speed (knots)<br>MMSI 563040400")

saveWidget(map_563, "03_report/graphs/track_563040400.html", selfcontained = TRUE)
cat("Saved track map for MMSI 563040400\n")

# ---- 4.2(c) Analyse MMSI 211430830 on the Danube (Vienna - Linz) ----
win_start <- as.POSIXct("2021-01-17 00:00:00", tz = "UTC")
win_end   <- as.POSIXct("2021-01-20 00:00:00", tz = "UTC")

v_211 <- fetch_vessel_track(211430830, win_start, win_end)

cat("Records:", nrow(v_211), "\n")
cat("Time span:", as.character(min(v_211$msg_timestamp)),
    "to", as.character(max(v_211$msg_timestamp)), "\n")
cat("Lat range:", round(range(v_211$latitude, na.rm = TRUE), 4), "\n")
cat("Lon range:", round(range(v_211$longitude, na.rm = TRUE), 4), "\n")
print(summary(v_211$speed))

# Look at how speed and time spacing behave - locks show up as slow, clustered points
v_211 <- v_211[order(v_211$msg_timestamp), ]
v_211$ts <- as.POSIXct(v_211$msg_timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
v_211$gap_s <- c(NA, as.numeric(diff(v_211$ts)))   # seconds between consecutive msgs
cat("\nSlow records (speed < 1 kn):", sum(v_211$speed < 1, na.rm = TRUE),
    "of", nrow(v_211), "\n")
print(summary(v_211$gap_s))

# ---- 4.2(d) Detect ship-lock passages for MMSI 211430830 ----
source("02_code/R/functions/lock_detection.R")

locks <- detect_lock_passages(v_211)
cat("Detected lock passages:", nrow(locks), "\n")
print(locks)

# ---- 4.2(e) Plot trajectory with detected lock passages ----
v_211_geo <- v_211 %>% filter(!is.na(latitude), !is.na(longitude))

lock_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Full route as a connecting line (chronological order)
  addPolylines(data = v_211_geo, lng = ~longitude, lat = ~latitude,
               color = "#3366cc", weight = 2, opacity = 0.6) %>%
  # Regular trajectory points: small, grey
  addCircleMarkers(data = v_211_geo, lng = ~longitude, lat = ~latitude,
                   radius = 2, stroke = FALSE, fillOpacity = 0.5,
                   color = "#666666", group = "Trajectory") %>%
  # Detected lock passages: large, red, with an info popup
  addCircleMarkers(data = locks, lng = ~longitude, lat = ~latitude,
                   radius = 9, stroke = TRUE, weight = 2,
                   color = "#b10026", fillColor = "#e31a1c", fillOpacity = 0.8,
                   popup = ~paste0("Detected lock<br>Start: ", start_time,
                                   "<br>Duration: ", duration_minutes, " min",
                                   "<br>Obs: ", n_observations),
                   group = "Detected locks") %>%
  addLegend("bottomright",
            colors = c("#666666", "#e31a1c"),
            labels = c("Trajectory point", "Detected lock passage"),
            title = "MMSI 211430830", opacity = 0.8)

saveWidget(lock_map, "03_report/graphs/track_211430830_locks.html", selfcontained = TRUE)
cat("Saved lock-detection map for MMSI 211430830\n")
