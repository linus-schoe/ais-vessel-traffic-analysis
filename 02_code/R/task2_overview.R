
# Load required packages
library(httr)
library(jsonlite)
library(dplyr)

base_url <- "https://aidaho-edu.uni-hohenheim.de/aisdb"

#--------------------------Task 2.1-----------------------------------------------

# Test connection - get 5 rows from ais_static
response <- GET(paste0(base_url, "/ais_static?limit=5")) # paste0 concatenates strings

# Check the status code - should be 200 if successful
status_code(response)

# Parse and view the data
data <- fromJSON(content(response, as = "text"))
print(data)


# Dynamic data
# IMPORTANT: always filter dynamic data, never query without filters
response_dynamic <- GET(paste0(base_url, "/ais_dynamic?select=mmsi,msg_timestamp,latitude,longitude,speed,course,heading,status,collection_type&msg_timestamp=gte.2024-01-24T00:00:00Z&msg_timestamp=lte.2024-01-24T00:05:00Z&limit=100"))

dynamic_data <- fromJSON(content(response_dynamic, as = "text"))
print(head(dynamic_data))
print(nrow(dynamic_data))
print(names(dynamic_data))

#-------------------------Task 2.2 -------------------------------------------
# Extract just the table/view names from the API
response_resources <- GET(base_url)
resources <- fromJSON(content(response_resources, as = "text"))

# Extract just the path names (table names)
table_names <- names(resources$paths)
# Filter to only show the actual data tables, not rpc functions
data_tables <- table_names[!grepl("^/rpc/", table_names)]
print("Available data tables:")
print(data_tables)

#-------------------------Task 2.3b -------------------------------------------
# Count rows in ais_static using content-range header
response_count <- GET(
  paste0(base_url, "/ais_static"),
  add_headers("Prefer" = "count=exact"),
  config(nobody = TRUE)
)
static_count <- headers(response_count)$`content-range`
print(paste("ais_static content-range:", static_count))

# Count rows in ais_dynamic directly, using the same content-range approach as
# above. A HEAD request (nobody = TRUE) with Prefer: count=exact makes PostgREST
# return the exact total in the Content-Range header (".../<total>") without
# transferring any rows, so it stays well within the API limits (~13 s for the
# full hypertable).
response_dynamic_count <- GET(
  paste0(base_url, "/ais_dynamic"),
  add_headers("Prefer" = "count=exact"),
  config(nobody = TRUE)
)
dynamic_count <- headers(response_dynamic_count)$`content-range`
print(paste("ais_dynamic content-range:", dynamic_count))

# Distinct vessels:
# ais_static has one row per vessel (mmsi is PRIMARY KEY), so the number of
# distinct vessels equals its row count. We verify the row count directly.
response_distinct_static <- GET(
  paste0(base_url, "/ais_static?select=mmsi"),
  add_headers("Prefer" = "count=exact"),
  config(nobody = TRUE)
)
print(paste("Distinct vessels (ais_static rows):",
            headers(response_distinct_static)$`content-range`))

#-------------------------Task 2.3c (vessel types) ----------------------------
# PostgREST automatic group-by: selecting a column alongside count() groups by
# that column. order=count.desc sorts most frequent first.
response_types <- GET(paste0(base_url,
  "/ais_static?select=ship_type,count()&order=count.desc"))
ship_types <- fromJSON(content(response_types, as = "text"))
print(ship_types)

#-------------------------Task 2.3d (flags) -----------------------------------
# Number of distinct flags: group by flag, count rows per flag.
# The number of rows returned = number of distinct flags.
response_flags <- GET(paste0(base_url,
  "/ais_static?select=flag,count()&order=count.desc"))
flags <- fromJSON(content(response_flags, as = "text"))

print(paste("Number of distinct flags:", nrow(flags)))
print(head(flags, 10))   # top 10 most frequent flags

#-------------------------Task 2.3e (column overview) -------------------------

# --- Numeric columns: min, max, avg, and non-missing count ---
# PostgREST can compute several aggregates in one request.
numeric_summary <- function(col) {
  url <- paste0(base_url, "/ais_static?select=",
                "min:", col, ".min(),",
                "max:", col, ".max(),",
                "avg:", col, ".avg(),",
                "n:",   col, ".count()")
  res <- GET(url)
  out <- fromJSON(content(res, as = "text"))
  out$column <- col
  out
}

# count() ignores NULLs, so n = number of non-missing observations
num_cols <- c("draught", "length", "width")
numeric_stats <- do.call(rbind, lapply(num_cols, numeric_summary))
print(numeric_stats)

# --- Categorical column: most frequent destinations ---
response_dest <- GET(paste0(base_url,
  "/ais_static?select=destination,count()&order=count.desc&limit=10"))
destinations <- fromJSON(content(response_dest, as = "text"))
print(destinations)

# How many vessels report a real IMO number (0 is used as "missing")
response_imo <- GET(paste0(base_url, "/ais_static?imo=gt.0"),
                    add_headers("Prefer" = "count=exact"),
                    config(nobody = TRUE))
print(paste("Vessels with IMO > 0:", headers(response_imo)$`content-range`))

#-------------------------Task 2.4a (Benelux ships) ---------------------------
# Benelux flags: Belgium (BE), Netherlands (NL), Luxembourg (LU)
response_benelux <- GET(
  paste0(base_url, "/ais_static?flag=in.(BE,NL,LU)"),
  add_headers("Prefer" = "count=exact"),
  config(nobody = TRUE)
)
print(paste("Benelux ships:", headers(response_benelux)$`content-range`))

#-------------------------Task 2.4b (German cargo ships) ----------------------
# German cargo ships: flag = DE AND ship_type = Cargo
response_de_cargo <- GET(
  paste0(base_url, "/ais_static?flag=eq.DE&ship_type=eq.Cargo"),
  add_headers("Prefer" = "count=exact"),
  config(nobody = TRUE)
)
print(paste("German cargo ships:", headers(response_de_cargo)$`content-range`))

# Of those, how many are longer than 150 m (add length filter)
response_de_cargo_150 <- GET(
  paste0(base_url, "/ais_static?flag=eq.DE&ship_type=eq.Cargo&length=gt.150"),
  add_headers("Prefer" = "count=exact"),
  config(nobody = TRUE)
)
print(paste("German cargo ships > 150m:", headers(response_de_cargo_150)$`content-range`))

#-------------------------Task 2.4c (EXPRESS in name) -------------------------
# Vessels with "EXPRESS" anywhere in the name (case-insensitive).
# PostgREST pattern match: * is the wildcard, ilike = case-insensitive like.
response_express <- GET(
  paste0(base_url, "/ais_static?name=ilike.*EXPRESS*"),
  add_headers("Prefer" = "count=exact"),
  config(nobody = TRUE)
)
print(paste("Vessels with EXPRESS in name:", headers(response_express)$`content-range`))

#-------------------------Task 2.4d (first 5 min of 2022) ---------------------
# Record count in the first five minutes of 2022.
response_2022_count <- GET(
  paste0(base_url, "/ais_dynamic?",
         "msg_timestamp=gte.2022-01-01T00:00:00Z&",
         "msg_timestamp=lt.2022-01-01T00:05:00Z"),
  add_headers("Prefer" = "count=exact"),
  config(nobody = TRUE)
)
print(paste("Records in first 5 min of 2022:",
            headers(response_2022_count)$`content-range`))

# Distinct vessels: fetch only the mmsi column for that window,
# then count unique values in R.
response_2022_distinct <- GET(
  paste0(base_url, "/ais_dynamic?select=mmsi&",
         "msg_timestamp=gte.2022-01-01T00:00:00Z&",
         "msg_timestamp=lt.2022-01-01T00:05:00Z&",
         "limit=100000"))
vessels_2022 <- fromJSON(content(response_2022_distinct, as = "text"))
print(paste("Distinct vessels:", length(unique(vessels_2022$mmsi))))

#-------------------------Task 2.4e (fast vessels, 30-min window) -------------
# Window: 2021-05-04 14:00:00 to 14:30:00 UTC, speed > 12 knots.
# First: distinct vessels moving faster than 12 knots in that window.
response_fast <- GET(
  paste0(base_url, "/ais_dynamic?select=mmsi&",
         "msg_timestamp=gte.2021-05-04T14:00:00Z&",
         "msg_timestamp=lt.2021-05-04T14:30:00Z&",
         "speed=gt.12&",
         "limit=1000000"))
fast_vessels <- fromJSON(content(response_fast, as = "text"))
fast_mmsi <- unique(fast_vessels$mmsi)
print(paste("Distinct vessels faster than 12 knots:", length(fast_mmsi)))
# Of those fast vessels, how many are cargo? Cross-reference ais_static.
# Build an in.() list from the fast vessel MMSIs.
mmsi_list <- paste(fast_mmsi, collapse = ",")

response_fast_cargo <- GET(
  paste0(base_url, "/ais_static?",
         "mmsi=in.(", mmsi_list, ")&",
         "ship_type=eq.Cargo"),
  add_headers("Prefer" = "count=exact"),
  config(nobody = TRUE)
)
print(paste("Of those, cargo vessels:", headers(response_fast_cargo)$`content-range`))

#-------------------------Task 2.4f (limiting records) ------------------------
# The limit parameter restricts how many rows PostgREST returns.
# Example: return only 5 vessels from ais_static.
response_limit <- GET(paste0(base_url, "/ais_static?select=mmsi,name,flag&limit=5"))
limited <- fromJSON(content(response_limit, as = "text"))
print(limited)
