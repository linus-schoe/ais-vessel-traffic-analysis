# =============================================================================
# Task 7.3 - Preprocess the data for the Shiny app
# -----------------------------------------------------------------------------
# The deployed Shiny app must NOT query the API at runtime. This script does all
# the expensive work once and writes a small prepared file that the app reads at
# start-up.
#
# It reuses the Task 5 pipeline (H3 spatial matching of AIS records to German
# rivers) and extends Task 5.2(g) - which projected only the Rhine - to compute
# the distance along the river for EVERY on-river record, on every river. The
# result is one row per on-river AIS record on 2022-04-23 with three columns:
#   river_name : the river the record was matched to
#   ship_type  : vessel type joined from the static lookup ("(not reported)" if NA)
#   dist_km    : distance of the record along that river (km, see orientation note)
#
# Output: shiny/app/data/ais_app_data.csv  (the path the app reads from; this
# folder is bind-mounted into the Shiny container in docker-compose.yaml).
#
# All inputs are read from the cached .rds files in 01_data produced by the
# Task 5 script, so this runs OFFLINE without touching the API. Run from the
# repository root (the R project working directory).
# =============================================================================

library(sf)
library(dplyr)
library(h3jsr)
library(readr)

sf_use_s2(TRUE)

# ---- Inputs (cached by ais_traffic_density.R) -------------------------------
ais_de        <- readRDS("01_data/ais_germany_20220423.rds")   # AIS records, 2022-04-23
german_rivers <- readRDS("01_data/german_rivers.rds")          # 9 cleaned river geometries
ship_lookup   <- readRDS("01_data/ship_lookup.rds")            # mmsi -> ship_type

# ---- Output path (where the Shiny app expects the file) ---------------------
output_csv <- file.path("shiny", "app", "data", "ais_app_data.csv")
dir.create(dirname(output_csv), showWarnings = FALSE, recursive = TRUE)

H3_RES   <- 7      # same resolution as Task 5
BUFFER_M <- 750    # same river buffer as Task 5 (~1.5 km corridor)

# ============================================================
# 1) Match each AIS record to a river via H3 (mirrors Task 5.2 b/c)
# ============================================================

# AIS points -> H3 cells
ais_de$h3 <- point_to_cell(ais_de[, c("longitude", "latitude")], res = H3_RES)

# Rivers (lines) -> buffered corridors -> covering H3 cells
rivers_buf <- german_rivers %>%
  st_transform(25832) %>%
  st_buffer(BUFFER_M) %>%
  st_transform(4326)

river_cells <- lapply(seq_len(nrow(rivers_buf)), function(i) {
  unique(unlist(polygon_to_cells(rivers_buf[i, ], res = H3_RES, simple = TRUE)))
})
names(river_cells) <- rivers_buf$name

# cell -> river lookup, resolving shared confluence cells to the larger river
cell_to_river <- data.frame(
  h3    = unlist(river_cells),
  river = rep(names(river_cells), lengths(river_cells)),
  stringsAsFactors = FALSE
)
river_size <- sapply(river_cells, length)
cell_to_river$river_size <- river_size[cell_to_river$river]
cell_to_river <- cell_to_river %>%
  arrange(h3, desc(river_size)) %>%
  distinct(h3, .keep_all = TRUE)

ais_de$river_name <- cell_to_river$river[match(ais_de$h3, cell_to_river$h3)]
ais_de <- ais_de %>% filter(!is.na(river_name))   # keep only on-river records

cat("On-river records:", nrow(ais_de), "\n")
print(sort(table(ais_de$river_name), decreasing = TRUE))

# ============================================================
# 2) Distance along the river, generalised to every river
# ============================================================
# Task 5.2(g) built ONE continuous reference line for the Rhine by orienting its
# disconnected pieces north->south and concatenating them. Here we generalise
# that to any river: a river is oriented along its DOMINANT axis (the longer side
# of its bounding box). Rivers that mainly run north-south (e.g. the Rhine) keep
# the exact Task 5 orientation, so the Rhine distribution matches the report;
# rivers that mainly run east-west (e.g. the Danube) are oriented west->east.
# Distances are therefore approximate and only meaningful relative to one river.

# Build a single continuous reference line for one river geometry (WGS84 in,
# metric reference line out, in EPSG:25832).
build_reference_line <- function(river_geom) {
  pieces <- st_cast(st_geometry(river_geom), "LINESTRING")

  # Decide the dominant axis from the river's bounding box.
  bb <- st_bbox(river_geom)
  y_dominant <- (bb["ymax"] - bb["ymin"]) >= (bb["xmax"] - bb["xmin"])

  if (y_dominant) {
    # North->south convention (origin at the northern end), identical to the
    # Rhine handling in Task 5.2(g), so the Rhine distribution matches the report.
    oriented <- lapply(pieces, function(p) {
      m <- st_coordinates(p)[, c("X", "Y")]
      if (m[1, "Y"] < m[nrow(m), "Y"]) m <- m[nrow(m):1, , drop = FALSE]  # high lat first
      m
    })
    order_vals <- -sapply(oriented, function(m) m[1, "Y"])   # northernmost piece first
  } else {
    # West->east convention (origin at the western end) for rivers that mainly
    # run east-west, such as the Danube.
    oriented <- lapply(pieces, function(p) {
      m <- st_coordinates(p)[, c("X", "Y")]
      if (m[1, "X"] > m[nrow(m), "X"]) m <- m[nrow(m):1, , drop = FALSE]  # low lon first
      m
    })
    order_vals <- sapply(oriented, function(m) m[1, "X"])    # westernmost piece first
  }

  oriented   <- oriented[order(order_vals)]
  all_coords <- do.call(rbind, oriented)
  st_sfc(st_linestring(all_coords), crs = 4326) %>% st_transform(25832)
}

# For each river, project its records onto that river's reference line.
ais_sf <- st_as_sf(ais_de, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(25832)

ais_de$dist_km <- NA_real_
for (nm in unique(ais_de$river_name)) {
  idx      <- which(ais_de$river_name == nm)
  ref_line <- build_reference_line(german_rivers[german_rivers$name == nm, ])
  along_m  <- st_line_project(ref_line, st_geometry(ais_sf)[idx])
  ais_de$dist_km[idx] <- as.numeric(along_m) / 1000
}

# ============================================================
# 3) Join ship type and write the prepared file
# ============================================================
app_data <- ais_de %>%
  left_join(ship_lookup, by = "mmsi") %>%
  mutate(ship_type = ifelse(is.na(ship_type), "(not reported)", ship_type)) %>%
  select(river_name, ship_type, dist_km) %>%
  filter(!is.na(dist_km))

readr::write_csv(app_data, output_csv)

cat("\nWrote", nrow(app_data), "rows to", output_csv, "\n")
cat("Rivers:", paste(sort(unique(app_data$river_name)), collapse = ", "), "\n")
cat("Ship types:", paste(sort(unique(app_data$ship_type)), collapse = ", "), "\n")
