# Shiny service image: rocker/shiny + the packages the app needs.
# The app itself is bind-mounted at runtime (see docker-compose.yaml), so
# changes to shiny/app are picked up without rebuilding.
FROM rocker/shiny:4.4.1

# Install the R packages used by app.R (rocker/shiny ships shiny but not these)
RUN install2.r --error --skipinstalled \
    ggplot2 \
    dplyr \
    readr \
    scales \
    && rm -rf /tmp/downloaded_packages

# shiny-server serves every sub-directory of /srv/shiny-server as an app at
# /<dirname>. Our app is mounted to /srv/shiny-server/ais_app, so it is
# reachable at http://shiny:3838/ais_app/ inside the compose network.
EXPOSE 3838
