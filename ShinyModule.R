library(shiny)
library(shinyWidgets)
library(ctmm)
library(sf)
library(glue)
library(dplyr)
library(purrr)
library(mapview)
library(leaflet)
library(zip)


shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id) ## all IDs of UI functions need to be wrapped in ns()
  tagList(
    titlePanel("Autocorrelated Kernel Density Estimate"),
    fluidRow(
      column(4,
             wellPanel(
               sliderInput(
                 ns("isopleth_levels"),
                 "Isopleth level:",
                 min = 0.01, max = .99, value = .95
               ),
             )
      )
    ),
    hr(), 
    h4("Map controll"), 
    sliderInput(ns("opacity"), "Opacity", min = 0, max = 1, value = 0.5), 
    selectizeInput(ns("what"), "What do you want to see?", c("Lower CI" = "low", "Estimate" = "est", "Upper CI" = "high"), multiple = TRUE, selected = "est"), 
#    uiOutput(ns("pickAnimals")), 
    leafletOutput(ns("map"))
  )
}

shinyModule <- function(input, output, session, data){ ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns ## all IDs of UI functions need to be wrapped in ns()
  
  hr <- akde(data[[2]], data[[1]])
  bbx <- map(hr, ~ st_as_sf(ctmm::SpatialPolygonsDataFrame.UD(.x, level.UD = 0.97)) |> 
               st_transform(4326)) |> 
    map(st_bbox)
  bbx <- do.call(rbind, bbx)
  
  hr.iso <- reactive({
    akde_sf <- map(hr, ~ sf::st_as_sf(ctmm::SpatialPolygonsDataFrame.UD(.x, level.UD = input$isopleth_levels))) |> bind_rows()
    
    # export as geopackage
    akde_sf |> 
      sf::st_write(appArtifactPath(glue::glue("homerange.gpkg")), append = FALSE)
    
    akde_sf
  })
  
 # output$pickAnimals <- renderCachedPlot({
 #   pickerInput("animals", choices = names(hr), options = list(`actions-box` = TRUE), multiple = TRUE)
 # })
  
  
  hr.leaflet <- reactive({
    akde_sf <- hr.iso() |> 
      mutate(ID = sub("[0-9]{1,2}%.*$", "", name),
             what = purrr::map_chr(stringr::str_split(name, " "), ~ tail(.x,1))) %>% 
      mutate(what = as.factor(what)) 
    akde_sf$what <- factor(akde_sf$what, levels = c("low", "est", "high"))
    akde_sf <- akde_sf %>%  
      arrange(desc(what)) %>% 
      st_transform(4326) |> 
      group_split(ID)
    names(akde_sf) <- names(hr)
    akde_sf
  })
  
  output$map <- renderLeaflet({
    
    
    leaflet() |> 
      addTiles(group = "OSM") |> 
      addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', 
               options = providerTileOptions(noWrap = TRUE), group="World Imagery") |> 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") |> 
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") |> 
      fitBounds(min(bbx[, 1]), min(bbx[, 2]), max(bbx[, 3]), max(bbx[, 4]))
    
  })
  
  what.cols = c(low = "#ddfff7", est = "#93e1d8", high = "#ffa69e")
  observe({
    m <- leafletProxy("map") |> 
      clearShapes() |> 
      clearControls()
    for (i in 1:length(hr.leaflet())) {
      m <- m |> addPolygons(data = hr.leaflet()[[i]] |> 
                              filter(what %in% input$what), 
                            group = names(hr.leaflet())[i], 
                            color = what.cols[input$what],
                            fillColor = what.cols[input$what], 
                            fillOpacity = input$opacity)
    }
    m |> 
      addLayersControl(
        baseGroups = c("OSM", "Wolrd Imagery", "Toner", "Toner Lite"),
        overlayGroups = names(hr.leaflet()),
        options = layersControlOptions(collapsed = TRUE)
      ) |> 
      addLegend(
        position = "bottomright",
        colors = c("#ddfff7","#93e1d8","#ffa69e"), 
        labels  = c("low", "est", "high")) 
    
  })
  
  
  
  # Artefact: summary
  observe({
    xx <- map(hr, ~ summary(.x, level.UD = input$isopleth_levels)$CI)
    xx <- do.call(rbind, xx)
    xx |> as_tibble() |> 
      mutate(id = names(hr), unit = rownames(xx), level = input$isopleth_levels) |> 
      select(id, level, unit, low, est, high) |> 
      write.csv(appArtifactPath(glue::glue("akde_summary.csv")))
  })
  
  # Artefact: tifs
  dir.create(targetDirUDs <- tempdir())
  
  r <- lapply(names(hr), function(x) 
    writeRaster(hr[[x]], file.path(targetDirUDs, paste0(x, ".tif")), overwrite = TRUE))
  
  zip::zip(
    zipfile = appArtifactPath("uds.zip"),
    files = list.files(targetDirUDs, full.names = TRUE, pattern = "tif$"),
    mode = "cherry-pick"
  )
  
  return(reactive({ 
    c(data[[2]], list(hr), data[[1]])
  })) ## if data are not modified, the unmodified input data must be returned
}
