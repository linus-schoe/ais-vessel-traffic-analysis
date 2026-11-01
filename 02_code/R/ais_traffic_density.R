# ais_traffic_density.R
# Task 5 - AIS traffic density on German rivers using H3 spatial indexing.

source("02_code/R/functions/api_helpers.R")
library(sf)
library(rnaturalearth)
library(dplyr)
library(ggplot2)

sf_use_s2(TRUE)

# ============================================================
# 5.1(a) German river geometries from rnaturalearth
# ============================================================

# Germany boundary (for clipping rivers to the country)
germany <- ne_countries(scale = "large", country = "Germany", returnclass = "sf")

# Global river centre-lines (physical layer). Cache locally to avoid re-download.
rivers_cache <- "01_data/ne_rivers.rds"
if (file.exists(rivers_cache)) {
  rivers_world <- readRDS(rivers_cache)
} else {
  rivers_world <- ne_download(scale = 10, type = "rivers_lake_centerlines",
                              category = "physical", returnclass = "sf")
  saveRDS(rivers_world, rivers_cache)
}

# Keep only rivers that intersect Germany, then clip to the boundary
rivers_world  <- st_make_valid(rivers_world)
german_rivers <- rivers_world[st_intersects(rivers_world, germany, sparse = FALSE), ]
german_rivers <- st_intersection(german_rivers, germany)

# ---- Clean / dedupe ----
german_rivers <- german_rivers %>%
  filter(!st_is_empty(geometry)) %>%        # drop empty geometries
  filter(!is.na(name))                       # drop unnamed fragments

# Normalise names: "Rhein", "Rhine", and "Rhin" are all the same river under
# German / English / French spellings. Verified by inspecting the "Rhin"
# geometry, which lies on the Upper Rhine near Basel (lon ~7.5-8.7, lat ~47.5-49),
# not a separate Brandenburg river. All three are therefore merged. "Donau" is
# the Danube (German name).
german_rivers$name[german_rivers$name %in% c("Rhein", "Rhine", "Rhin")] <- "Rhine"

german_rivers <- german_rivers %>%
  group_by(name) %>%                         # merge multi-part features per river
  summarise(geometry = st_union(geometry), .groups = "drop")

# Drop rivers with a negligible length inside Germany. The Warta is almost
# entirely Polish and reduces to a ~10 m sliver after clipping (0.01 km), so it
# is not a meaningful German river for traffic analysis.
german_rivers$len_km <- as.numeric(st_length(german_rivers)) / 1000
german_rivers <- german_rivers %>% filter(len_km >= 5)

cat("German rivers found:", nrow(german_rivers), "\n")
print(german_rivers$name)

saveRDS(german_rivers, "01_data/german_rivers.rds")

# ============================================================
# 5.1(b) Map the German rivers, each a different colour
# ============================================================

river_map <- ggplot() +
  geom_sf(data = germany, fill = "grey95", colour = "grey70", linewidth = 0.3) +
  geom_sf(data = german_rivers, aes(colour = name), linewidth = 0.8) +
  # Task 5.1(b) requires a distinct colour per river, so each river gets its
  # own hue from the default qualitative palette.
  scale_colour_manual(values = scales::hue_pal()(nrow(german_rivers))) +
  labs(title = "Major rivers in Germany (Natural Earth)",
       colour = "River") +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_text(size = 7))

ggsave("03_report/graphs/german_rivers.png", river_map,
       width = 8, height = 9, dpi = 300)
cat("Saved german_rivers.png\n")

# ============================================================
# 5.1(c) Query ais_germany for all records on 2022-04-23
# ============================================================

ag_cache <- "01_data/ais_germany_20220423.rds"
if (file.exists(ag_cache)) {
  ais_de <- readRDS(ag_cache)
  say("Loaded ais_germany sample from cache.\n")
} else {
  ais_de <- api_get(paste0(
    "/ais_germany?select=mmsi,msg_timestamp,latitude,longitude,speed,course,heading,status,collection_type",
    "&msg_timestamp=gte.2022-04-23T00:00:00Z",
    "&msg_timestamp=lt.2022-04-24T00:00:00Z",
    "&limit=1000000"))
  saveRDS(ais_de, ag_cache)
  say("Saved ais_germany sample to cache.\n")
}

say("Records on 2022-04-23:", nrow(ais_de), "\n")
say("Lat range:", round(range(ais_de$latitude, na.rm = TRUE), 3), "\n")
say("Lon range:", round(range(ais_de$longitude, na.rm = TRUE), 3), "\n")

# ============================================================
# 5.2(b) Compute H3 indices (resolution 7) for AIS points and rivers
# ============================================================
library(h3jsr)

H3_RES <- 7   # assignment-specified resolution

# ---- AIS points -> H3 cells ----
ais_de$h3 <- point_to_cell(ais_de[, c("longitude", "latitude")], res = H3_RES)

# ---- Rivers (lines) -> buffered polygons -> H3 cells ----
# Rivers are line geometries; polygon_to_cells needs polygons. Buffer each river
# line into a thin corridor, then fill it with H3 cells. A res-7 cell is ~1.2 km
# across, so a 750 m buffer (~1.5 km corridor) keeps the river at least one cell
# wide continuously and absorbs the offset between a vessel's reported position
# and the river centre-line. Buffering is done in a metric CRS (UTM 32N).
buffer_m <- 750

rivers_buf <- german_rivers %>%
  st_transform(25832) %>%
  st_buffer(buffer_m) %>%
  st_transform(4326)

river_cells <- lapply(seq_len(nrow(rivers_buf)), function(i) {
  cells <- polygon_to_cells(rivers_buf[i, ], res = H3_RES, simple = TRUE)
  unique(unlist(cells))
})
names(river_cells) <- rivers_buf$name

for (nm in names(river_cells)) {
  cat(nm, ":", length(river_cells[[nm]]), "H3 cells\n")
}
cat("Total unique river cells:", length(unique(unlist(river_cells))), "\n")


# ============================================================
# 5.2(c) Match AIS records to rivers via H3 cells
# ============================================================

# Build a cell -> river lookup. Where a cell is claimed by more than one river
# (e.g. at confluences like Rhine/Main or Rhine/Mosel), we assign it to the river
# whose buffered corridor contains the most cells overall as a deterministic
# tie-break -- but we first record multiplicity so we can report overlaps.
cell_to_river <- data.frame(
  h3    = unlist(river_cells),
  river = rep(names(river_cells), lengths(river_cells)),
  stringsAsFactors = FALSE
)

# How many cells are shared by >1 river?
multi <- cell_to_river %>% count(h3) %>% filter(n > 1)
cat("Cells claimed by more than one river:", nrow(multi), "\n")

# Multi-river rule: keep one river per cell. We resolve ties by river size
# (number of cells), so a shared confluence cell is attributed to the larger
# river. This is deterministic and documented.
river_size <- sapply(river_cells, length)
cell_to_river$river_size <- river_size[cell_to_river$river]
cell_to_river <- cell_to_river %>%
  arrange(h3, desc(river_size)) %>%
  distinct(h3, .keep_all = TRUE)        # keep the larger river per shared cell

# Classify each AIS record
ais_de$on_river   <- ais_de$h3 %in% cell_to_river$h3
ais_de$river_name <- cell_to_river$river[match(ais_de$h3, cell_to_river$h3)]

cat("Records on a river:", sum(ais_de$on_river), "\n")
cat("Records elsewhere:", sum(!ais_de$on_river), "\n")

# ============================================================
# 5.2(d) Percentage of records on a river vs elsewhere
# ============================================================
pct_river <- round(100 * sum(ais_de$on_river) / nrow(ais_de), 1)
cat("On a German river:", pct_river, "%\n")
cat("Elsewhere:", round(100 - pct_river, 1), "%\n")

# ============================================================
# 5.2(e) Ship-type distribution: river vs elsewhere
# ============================================================
library(tidyr)

# ais_germany has no ship_type; join it from the cached static lookup by mmsi.
ship_lookup <- readRDS("01_data/ship_lookup.rds")
ais_de <- ais_de %>% left_join(ship_lookup, by = "mmsi")

# Label location and ship type (NA -> "(not reported)")
plot_df <- ais_de %>%
  mutate(location  = ifelse(on_river, "On river", "Elsewhere"),
         ship_type = ifelse(is.na(ship_type), "(not reported)", ship_type)) %>%
  count(location, ship_type) %>%
  group_by(location) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

type_map <- ggplot(plot_df,
                   aes(x = reorder(ship_type, -prop), y = prop, fill = location)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("On river" = "grey25", "Elsewhere" = "grey65")) +
  labs(title = "Ship-type distribution: on a river vs elsewhere (2022-04-23)",
       x = "Ship type", y = "Proportion within location", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("03_report/graphs/rivertype_vs_elsewhere.png", type_map,
       width = 9, height = 5, dpi = 300)
cat("Saved rivertype_vs_elsewhere.png\n")

# ============================================================
# 5.2(f) Records per river, sorted descending
# ============================================================
river_summary <- ais_de %>%
  filter(on_river) %>%
  count(river_name, name = "records") %>%
  arrange(desc(records))

print(river_summary)

# ============================================================
# 5.2(g) Traffic distribution along the Rhine
# ============================================================

rhine_geom <- german_rivers %>% filter(name == "Rhine")          # WGS84
rhine_pts  <- ais_de %>% filter(on_river, river_name == "Rhine")

rhine_sf <- st_as_sf(rhine_pts, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(25832)

# The Rhine clips into 11 disconnected pieces. To build ONE continuous reference
# line without distorting its length, we keep each piece's natural vertex order,
# orient each piece north->south, then order the pieces by latitude and join them.
pieces <- st_cast(st_geometry(rhine_geom), "LINESTRING")

oriented <- lapply(pieces, function(p) {
  m <- st_coordinates(p)[, c("X", "Y")]
  # ensure the piece runs north (high lat) -> south (low lat)
  if (m[1, "Y"] < m[nrow(m), "Y"]) m <- m[nrow(m):1, , drop = FALSE]
  m
})

# order the pieces by their northernmost latitude, descending (north first)
north_lat <- sapply(oriented, function(m) m[1, "Y"])
oriented  <- oriented[order(-north_lat)]

# concatenate the ordered pieces into a single continuous line
all_coords <- do.call(rbind, oriented)
ref_line   <- st_sfc(st_linestring(all_coords), crs = 4326) %>% st_transform(25832)

# Project each AIS point onto this reference line to get distance along the river
along_m <- st_line_project(ref_line, st_geometry(rhine_sf))
rhine_pts$along_km <- as.numeric(along_m) / 1000

# Bin into ~10 km segments and count records per bin
bin_width <- 10
rhine_pts$bin <- floor(rhine_pts$along_km / bin_width) * bin_width
rhine_dist <- rhine_pts %>% count(bin, name = "records") %>% arrange(bin)

rhine_plot <- ggplot(rhine_dist, aes(x = bin, y = records)) +
  geom_col(fill = "grey30", width = bin_width * 0.9) +
  labs(title = "AIS traffic distribution along the Rhine (2022-04-23)",
       x = "Distance along the Rhine (km, north -> south)",
       y = "AIS records (10 km bins)") +
  theme_minimal(base_size = 12)

ggsave("03_report/graphs/rhine_distribution.png", rhine_plot,
       width = 9, height = 5, dpi = 300)

cat("Saved rhine_distribution.png\n")
cat("Reference line length (km):", round(as.numeric(st_length(ref_line)) / 1000, 1), "\n")
cat("Total Rhine records binned:", sum(rhine_dist$records), "\n")
