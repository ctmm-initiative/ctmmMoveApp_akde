library(shiny)
library(ctmm)
library(sf)
library(glue)
library(dplyr)
library(purrr)
library(mapview)
library(leaflet)

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id) ## all IDs of UI functions need to be wrapped in ns()
  tagList(
    fluidRow(
      column(4,
             wellPanel(
               sliderInput(
                 ns("isopleth_levels"),
                 "Isopleth levels:",
                 min = 0.01, max = .99, value = .95
               ),
             )
      )
    ),
    leafletOutput(ns("map"))
  )
}

shinyModule <- function(input, output, session, data){ ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns ## all IDs of UI functions need to be wrapped in ns()
  
  hr <- akde(data[[1]], data[[2]])
  
  output$map <- renderLeaflet({
  
    akde_sf <- map(hr, ~ sf::st_as_sf(ctmm::SpatialPolygonsDataFrame.UD(.x, level.UD = input$isopleth_levels))) |> bind_rows()
    m <- mapview(akde_sf)
    # export as geopackage
    akde_sf |> 
      sf::st_write(appArtifactPath(glue::glue("homerange.gpkg")))
    m@map
  })
  
  return(reactive({ 
    list(data, hr)
  })) ## if data are not modified, the unmodified input data must be returned
}
