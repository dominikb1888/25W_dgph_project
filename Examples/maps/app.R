# example and data from:
# https://ipyleaflet.readthedocs.io/en/latest/layers/geo_json.html
# https://ipyleaflet.readthedocs.io/en/latest/layers/marker.html

library(shiny)
library(bslib)
library(leaflet)
library(jsonlite)

country_boundaries <- read_json("europe_110.geo.json")

random_color <- function() {
  sample(c("red", "yellow", "green", "orange"), size = 1)
}

for (i in seq_along(country_boundaries$features)) {
  country_boundaries$features[[i]]$style <- list(fillColor = random_color())
}

ui <- page_fluid(
  markdown("## A `leaflet` Map"),
  leafletOutput("map") 
)

server <- function(input, output) {
  output$map <- renderLeaflet({ 
  
    leaflet(leafletOptions(preferCanvas = TRUE)) |> 
      setView(0.34580993652344, 50.6252978589571, zoom = 3) |> 
      addGeoJSON( 
        country_boundaries, 
        opacity = 1, 
        dashArray = "9", 
        fillOpacity = 0.1, 
        color = "black", 
        weight = 1 
      ) |> 
      addCircleMarkers(12.92302, 48.4293) 
    
  }) 
}

shinyApp(ui = ui, server = server)