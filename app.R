library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tmap)
library(tidyverse)
remotes::install_github("itsleeds/jts")
remotes::install_github("robinlovelace/ukboundaries")
jts0401 = jts::get_jts_data(table = "jts0401")

jts_mode_names = c("PT", "Cyc", "Car")
jtsm_collapsed = paste0(jts_mode_names, collapse = "|")

jtsmt = paste0(jts_mode_names, "t")
jtsmt_collapsed = paste0(jtsmt, collapse = "|")
jts_data = jts0401 %>% 
  select(1:4, matches(jtsmt_collapsed) & matches("5000")) %>% 
  rename(code = LA_Code)

lads = ukboundaries::lad2011_simple
jts_data = left_join(lads, jts_data)

jts_vars = setdiff(names(jts_data), c("name", "code", "altname", "geometry"))
shinyApp(
  ui = dashboardPage(
    options = list(sidebarExpandOnHover = TRUE),
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      minified = TRUE,
      collapsed = TRUE,
      textInput(inputId = "location", label = "Zoom to location", placeholder = "Leeds", value = "Leeds"),
      sliderInput(inputId = "slider1", label = "Demo slider", min = 0, max = 100, value = 50),
      selectInput("var", "Variable", jts_vars)
    ),
    body = dashboardBody(
      box(title = "Map", width = 12, tmap::tmapOutput(outputId = "map"))
    ),
    controlbar = dashboardControlbar(),
    title = "DashboardPage"
  ),
  server = function(input, output, session) {
    
    output$map <- renderTmap({
      bb = tmaptools::geocode_OSM(q = input$location)$bbox
      if(is.null(bb)) bb = sf::st_bbox(spData::lnd)
      tm_shape(jts_data, bbox = bb) +
        tm_polygons(jts_vars[1], zindex = 401, alpha = 0.2)
    })
    
    observe({
      var = input$var
      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(jts_data) +
          tm_polygons(var, zindex = 401, alpha = 0.2)
      })
    })
  }
)
