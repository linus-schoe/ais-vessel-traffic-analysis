# =============================================================================
# Task 7.2 (Option A) - Interactive AIS traffic-density Shiny app
# -----------------------------------------------------------------------------
# Extends Task 5.2(g): instead of only the Rhine, the user picks ANY river and
# one or more ship types, then clicks "Refresh" to redraw the distance-along-
# river distribution (10 km bins) for 2022-04-23.
#
# The app reads ONLY the small prepared file data/ais_app_data.csv produced by
# 02_code/R/prepare_shiny_data.R - it never queries the API itself (Task 7.3).
# =============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# ---- Load prepared data once at startup -------------------------------------
# Working directory of a shiny-server app is the app folder, so this relative
# path resolves to <app>/data/ais_app_data.csv inside the container.
ais <- readr::read_csv("data/ais_app_data.csv", show_col_types = FALSE)

river_choices <- sort(unique(ais$river_name))
type_choices  <- sort(unique(ais$ship_type))

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("AIS traffic density on German rivers (2022-04-23)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "river", "River:",
        choices  = river_choices,
        selected = if ("Rhine" %in% river_choices) "Rhine" else river_choices[1]
      ),
      selectizeInput(
        "types", "Ship type(s):",
        choices  = type_choices,
        selected = type_choices,          # default: all types
        multiple = TRUE
      ),
      actionButton("refresh", "Refresh plot", class = "btn-primary"),
      helpText("Pick a river and one or more ship types, then click Refresh.")
    ),
    mainPanel(plotOutput("densityPlot", height = "480px"))
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Recompute only when the button is pressed (ignoreNULL = FALSE also runs it
  # once at start-up so the plot is not blank before the first click).
  plot_data <- eventReactive(input$refresh, ignoreNULL = FALSE, {
    ais |>
      dplyr::filter(river_name == input$river,
                    ship_type  %in% input$types)
  })

  output$densityPlot <- renderPlot({
    df <- plot_data()
    validate(need(nrow(df) > 0, "No AIS records for this river / ship-type selection."))

    ggplot(df, aes(x = dist_km)) +
      geom_histogram(binwidth = 10, boundary = 0,
                     fill = "#2C7FB8", colour = "white") +
      labs(
        title = paste0("AIS traffic along the ", isolate(input$river),
                       " (2022-04-23)"),
        x = "Distance along the river (km, north -> south)",
        y = "AIS records (10 km bins)"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
