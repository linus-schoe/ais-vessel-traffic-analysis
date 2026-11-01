# AIS VESSEL TRAFFIC ANALYSIS

This repository contains my project for the course "Introduction to Data Science with R and RStudio".

The project analyses **Automatic Identification System (AIS)** vessel-tracking data, covering more than **826 million position records** 
from 2021 to 2024, served via a PostgreSQL/TimescaleDB database and a PostgREST API. It spans the full data science workflow, from API-based 
data access and sampling to spatial analysis and the deployment of a containerised web application.

- Group: 20 (Linus Schönewald & Robert Kaminski)
- Course Instructor: Dr. Johannes Bleher
- Grade: 1.0

## Project Overview

1. **Data Exploration:** Querying the AIS database via a PostgREST API and assessing data quality, such as missing values, placeholder entries and coverage bias.
2. **Sampling:** Comparing an interval (cluster) sample with a stratified sample of 10,000 records each. The stratified sample turned out to be more representative.
3. **Vessel Behaviour:** Reconstructing individual vessel tracks and building a rule-based heuristic that detects ship-lock passages on the Danube.
4. **Traffic on German Rivers:** Matching AIS records to German rivers using the H3 hexagonal index. About 37% of German traffic runs on inland rivers, led by the Rhine and the Elbe.
5. **Deployment:** A static map dashboard and an interactive Shiny app, served via Docker Compose and NGINX.

## Tech Stack

R (`dplyr`, `ggplot2`, `sf`, `h3jsr`, `leaflet`, Shiny) · PostgreSQL / PostgREST · Docker · NGINX · LaTeX

## Repository Structure

```
ais-vessel-traffic-analysis/
├── 00_docs/                          <- Assignment sheet and guidelines
├── 01_data/                          <- Generated samples and cached API/geometry data (.csv / .rds)
├── 02_code/
│   └── R/
│       ├── task2_overview.R          <- API resources, table sizes, summary statistics
│       ├── ais_dynamic_sample_points.R      <- Interval (cluster) sample
│       ├── ais_dynamic_sample_stratified.R  <- Stratified sample
│       ├── compare_samples.R         <- Comparison of both samples
│       ├── sample_dashboard.R        <- Speed-coloured Leaflet maps
│       ├── ais_dynamic_individual_paths.R   <- Vessel tracks and lock detection
│       ├── ais_traffic_density.R     <- H3 matching of AIS records to German rivers
│       ├── sample_html_dashboard.R   <- Static HTML dashboard export
│       ├── prepare_shiny_data.R      <- Data preprocessing for the Shiny app
│       └── functions/                <- Reusable helpers (API queries, lock detection)
├── 03_report/
│   ├── report.tex / report.pdf       <- Written report
│   └── graphs/                       <- All figures used in the report
├── docker-compose.yaml               <- NGINX + Shiny stack
├── nginx/conf.d/                     <- NGINX config (static files + reverse proxy)
├── README.md
├── shiny/                            <- Shiny app (Dockerfile + app/)
└── www/                              <- NGINX document root (landing page + dashboard)
```

## Data Source

The AIS data were provided by Dr. Johannes Bleher via a PostgreSQL database at the University of Hohenheim.

## Reproducibility

Random samples use a fixed seed (`set.seed(2026)`). Large API results are cached as `.rds` files in `01_data/`, 
so most scripts reproduce fully offline. Only `task2_overview.R`, `ais_dynamic_sample_points.R` and `ais_dynamic_individual_paths.R` 
need access to the university-hosted database. Their outputs are already included.

**Run the analysis** (from the repository root, or open the `.Rproj` in RStudio):

```bash
Rscript 02_code/R/ais_traffic_density.R
```

**Run the web application locally:**

```bash
docker compose up -d --build
```

Then open `http://localhost:7080/` for the landing page, `/sample_points.html` for the dashboard and `/ais_app/` for the Shiny app.