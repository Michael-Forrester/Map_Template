Map_plot <- function(mapData,
                     featureName,
                     analyticalData,
                     mapStyle,
                     metric,
                     logScale=FALSE) {
  
  #############################################################################
  ## Inputs ###################################################################
  # mapData:        JSON file describing geographic boundaries.
  # featureName:    The feature in map Data which delineates the geographic
  #                 region (e.g. country)
  # analyticalData: Tibble with data to plot on the map
  # mapStyle:       The type of regions being visualised (e.g. countries). Must
  #                 exist as a column name in analyticalData.
  # metric:         The value to be plotted. Must
  #                 exist as a column name in analyticalData.
  #
  ## Outputs ##################################################################
  # m: List object describing Leaflet map.
  # {mapStyle}.html: Map visulaisation in html format.
  #############################################################################
  
  # Load packages
  library(leaflet)
  library(sf)
  library(dplyr)
  library(scales)
  library(htmlwidgets)
  library(textshape)
  
  ## Initialise map
  m <- leaflet() %>%
    setView(lng = 0.1278, lat = 51.5074, zoom = 1)
  # Set index to mapStyle
  df_indexed <- analyticalData |>
    column_to_rownames(mapStyle)
  
  ## Add analytical data
  mapData[[metric]] <- df_indexed[mapData[[featureName]], metric]
  # Replace missing values with 0
  mapData[[metric]][is.na(mapData[[metric]])] <- 0
  
  ## Colour bar
  
  colours <- c("#FFFFFF", "#28A197", "#12436D")
  
  if (logScale) {
    
    # log scale sequence
    max_power <- 1 + ceiling(log(max(df_indexed[[metric]]),
                                 base=logScale)
                             )
    c_log <- c(0,
               logScale^(seq(0,
                             max_power,
                             length.out = max_power+1)
                         )
               )
    # colour scale
    colourArray <- colorRampPalette(colours)(max_power+2)
    
    # colour-picker function
    pal <- colorBin(
      palette = colourArray,
      domain = c_log,
      bins = c_log,
      na.color = "white"
    )
    
  } else {
    
    # colour-picker function
    pal <- colorNumeric(
      palette = colours,
      domain = c(0,
                 max(df_indexed[[metric]])
                 ),
      na.color = "white"
    )
    
  }
  
  # Generate map with tooltips
  m <- m %>%
    addPolygons(
      data = mapData,
      fillColor = ~pal(get(metric)),
      fillOpacity = 0.9,
      color = "black",
      weight = 1,
      label = ~paste0(
        "<b>", mapStyle, ":</b> ", get(featureName), "<br>",
        "<b>", metric, ":</b> ", scales::comma(get(metric))
      ) %>% lapply(htmltools::HTML),
      highlightOptions = highlightOptions(
        weight = 2,
        color = "white",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    htmlwidgets::prependContent(htmltools::tags$style(".leaflet-container { background: #ADD8E6; }" )) %>%
    addLegend(
      pal = pal,
      values = mapData[[metric]],
      title = metric,
      labFormat = labelFormat(),
      opacity = 1
    )
  
  # Save map
  saveWidget(m, file = paste0(mapStyle, ".html"))
  
  return(m)
  
}