library(shiny)
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
    leafletOutput(ns("map"))
  )
}

shinyModule <- function(input, output, session, data){ ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns ## all IDs of UI functions need to be wrapped in ns()
  
  hr <- akde(data[[2]], data[[1]])
  
  output$map <- renderLeaflet({
    
    akde_sf <- map(hr, ~ sf::st_as_sf(ctmm::SpatialPolygonsDataFrame.UD(.x, level.UD = input$isopleth_levels))) |> bind_rows()
    
    # export as geopackage
    akde_sf |> 
      sf::st_write(appArtifactPath(glue::glue("homerange.gpkg")), append = FALSE)
    
    akde_sf <- akde_sf %>% 
      mutate(ID = sub("[0-9]{1,2}%.*$", "", name),
             what = purrr::map_chr(stringr::str_split(name, " "), ~ tail(.x,1))) %>% 
      mutate(what = as.factor(what)) 
    akde_sf$what <- factor(akde_sf$what, levels = c("low", "est", "high"))
    akde_sf <- akde_sf %>%  
      arrange(desc(what)) %>% 
      group_split(ID)
    names(akde_sf) <- names(hr)
    m <- mapview(akde_sf, 
                 #layer.name = "what", 
                 zcol = "what",
                 legend = TRUE,
                 col.regions=list("#ddfff7","#93e1d8","#ffa69e"))
    m@map
  })
  
  # Artefact: summary
  xx <- map(hr, ~ summary(.x)$CI)
  xx <- do.call(rbind, xx)
  xx |> as_tibble() |> 
    mutate(id = names(hr), unit = rownames(xx)) |> 
    select(id, unit, low, est, high) |> 
    write.csv(appArtifactPath(glue::glue("akde_summary.txt")))
  
  # Artefact: tifs
  dir.create(targetDirUDs <- tempdir())
  
  r <- lapply(names(hr), function(x) 
    writeRaster(hr[[x]], file.path(targetDirUDs, paste0(x, ".tif"))))
  
  zip::zip(
    zipfile = appArtifactPath("uds.zip"),
    files = list.files(targetDirUDs, full.names = TRUE, pattern = "tif$"),
    mode = "cherry-pick"
  )
  
  return(reactive({ 
    c(data[[2]], list(hr), data[[1]])
  })) ## if data are not modified, the unmodified input data must be returned
}
