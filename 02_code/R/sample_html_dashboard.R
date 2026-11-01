# =============================================================================
# Task 6.1 - Static HTML dashboard
# -----------------------------------------------------------------------------
# Reads the interval sample produced in Task 3 (01_data/sample_intervals.csv),
# draws the same speed-coloured leaflet map as in Task 4.1, and writes it to a
# SELF-CONTAINED HTML file (sample_points.html) that the NGINX container serves
# under https://<your-server>/sample_points.html.
#
# Only the two path variables below should ever need adjusting on another
# machine. Run this from the repository root (the R project working directory).
# =============================================================================

# ---- Packages ---------------------------------------------------------------
# install.packages(c("readr", "dplyr", "leaflet", "htmlwidgets"))
library(readr)        # fast CSV reading
library(dplyr)        # data wrangling
library(leaflet)      # interactive map
library(htmlwidgets)  # saveWidget(): export the map as standalone HTML

# ---- Paths (the only things a user may need to change) ----------------------
sample_csv  <- file.path("01_data", "sample_intervals.csv")  # input sample
output_html <- file.path("www", "sample_points.html")        # NGINX doc-root
dir.create(dirname(output_html), showWarnings = FALSE, recursive = TRUE)

# ---- Load the interval sample, drop rows without coordinates ----------------
sample_points <- readr::read_csv(sample_csv, show_col_types = FALSE) |>
  dplyr::filter(!is.na(latitude), !is.na(longitude))

# ---- Speed colour scale -----------------------------------------------------
# Yellow (slow) -> orange -> dark red (fast). The scale is capped at 25 knots so
# that a handful of implausibly high values do not dominate it; faster points
# simply take the top colour. The same encoding is explained in the legend.
speed_cap <- 25
sample_points <- sample_points |>
  dplyr::mutate(speed_capped = pmin(dplyr::coalesce(speed, 0), speed_cap))

pal <- leaflet::colorNumeric(
  palette = c("#FFEDA0", "#FEB24C", "#FC4E2A", "#B10026"),  # yellow -> dark red
  domain  = c(0, speed_cap)
)

# ---- Build the map (one helper, shared with the Task 4.1 dashboard) ---------
build_speed_map <- function(df) {
  leaflet::leaflet(df) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addCircleMarkers(
      lng = ~longitude, lat = ~latitude,
      # Thin dark outline keeps pale (slow-speed) markers visible on the
      # light CartoDB.Positron basemap; fill carries the speed colour.
      radius = 3, stroke = TRUE, color = "#333333", weight = 0.4, opacity = 0.6,
      fillColor = ~pal(speed_capped), fillOpacity = 0.8,
      popup  = ~paste0("MMSI: ", mmsi, "<br>Speed: ", round(speed, 1), " kn"),
      clusterOptions = leaflet::markerClusterOptions()  # avoid 10k raw markers
    ) |>
    leaflet::addLegend(
      position = "bottomright", pal = pal, values = c(0, speed_cap),
      title = "Speed (knots)", opacity = 1
    )
}

speed_map <- build_speed_map(sample_points)

# ---- Save as a SELF-CONTAINED HTML file -------------------------------------
# saveWidget() can be fussy about output paths that contain sub-directories, so
# we write to a temp file first and then copy it into the NGINX document root.
# selfcontained = TRUE inlines all JS/CSS so the file works as a single asset.
# (Requires pandoc, which ships with RStudio: rmarkdown::pandoc_available().)
tmp_html <- file.path(tempdir(), "sample_points.html")
htmlwidgets::saveWidget(
  speed_map,
  file          = tmp_html,
  selfcontained = TRUE,
  title         = "AIS interval sample - speed map"
)
file.copy(tmp_html, output_html, overwrite = TRUE)

message("Wrote self-contained dashboard to: ", normalizePath(output_html))
