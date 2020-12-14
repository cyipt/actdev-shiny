
# install/load packages ---------------------------------------------------

remotes::install_github("itsleeds/jts")
remotes::install_github("robinlovelace/ukboundaries")
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tmap)
library(tidyverse)

# persistent settings -----------------------------------------------------

s = c(
  `Grey basemap` = "CartoDB.Positron",
  `Coloured basemap` = "Esri.WorldTopoMap",
  `OSM existing cycle provision` = "https://b.tile-cyclosm.openstreetmap.fr/cyclosm/{z}/{x}/{y}.png",
  `PCT commuting, Go Dutch` = "https://npttile.vs.mythic-beasts.com/commute/v2/dutch/{z}/{x}/{y}.png",
  `PCT schools, Go Dutch` = "https://npttile.vs.mythic-beasts.com/school/v2/dutch/{z}/{x}/{y}.png",
  `Satellite image` = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'"
)
tms = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
jts_mode_names = c("PT", "Cyc", "Car")
jtsm_collapsed = paste0(jts_mode_names, collapse = "|")
jtsmt = paste0(jts_mode_names, "t")
jtsmt_collapsed = paste0(jtsmt, collapse = "|")

# get/preprocess data -----------------------------------------------------

jts_tables_sub = jts::jts_tables %>% 
  filter(str_detect(table_code, "jts04"))
services = unique(jts_tables_sub$service)
years = unique(jts_tables_sub$year)[1:4]
modes = c("Walking + Public Transport", "Cycling", "Driving")
length(destination_types)
jts0401 = jts::get_jts_data(table = "jts0401")
jts_data = jts0401 %>% 
  select(1:4, matches(jtsmt_collapsed) & matches("5000")) %>% 
  rename(code = LA_Code)
lads = ukboundaries::lad2011_simple
jts_data = left_join(lads, jts_data)
jts_vars = setdiff(names(jts_data), c("name", "code", "altname", "Region", "LA_Name", "geometry"))

# run app (to split out into ui/server) -----------------------------------

shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      textInput(inputId = "location", label = "Zoom to location", placeholder = "Leeds", value = "Leeds"),
      selectInput(inputId = "tableid", label = "Service (journey times to, placeholder)", choices = services),
      selectInput(inputId = "year", label = "Year (placeholder)", choices = years),
      selectInput(inputId = "modes", label = "Mode of travel (placeholder)", choices = modes),
      sliderInput(inputId = "slider1", label = "Transparency", min = 0, max = 1, value = 0.3),
      selectInput("var", "Variable", jts_vars)
    ),
    body = dashboardBody(
      tags$style(type = "text/css", "#map {height: calc(100vh - 150px) !important;}"),
      box(title = "Map", width = 12, tmap::tmapOutput(outputId = "map"))
    ),
    controlbar = dashboardControlbar(),
    title = "DashboardPage"
  ),
  server = function(input, output, session) {
    
    output$map = renderTmap({
      bb = tmaptools::geocode_OSM(q = input$location)$bbox
      if(is.null(bb)) bb = sf::st_bbox(spData::lnd)
      tm_shape(jts_data, bbox = bb) +
        tm_polygons(jts_vars[1], zindex = 401, alpha = 0.3) +
        tm_basemap(server = s, tms = tms)
    })
    
    observe({
      var = input$var
      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(jts_data) +
          tm_polygons(var, zindex = 401, alpha = 1 - input$slider1)
      })
    })
  }
)
