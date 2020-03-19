#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize=200*1024^2)
  
  #### UI ####
  
  output$sample_size_UI <- renderUI({
    if (tibble::is.tibble(df())) {
    shiny::sliderInput(inputId = "sample_size",
                       round = TRUE,
                        label = "Sample size",
                        value = if (nrow(df())>1000) 1000 else nrow(df()),
                        min = 1,
                        max = nrow(df()))
    }
  })
  
  output$long_range_UI <- renderUI({
    min_max <- if (input$longlat_invert == "Lat/Long") round(c(min(df()$Lat), max(df()$Lat)), digits = 2) else round(c(min(df()$Long), max(df()$Long)), digits = 2) 
    
    if (tibble::is.tibble(df())) {
      shiny::sliderInput(inputId = "long_range",
                         round = FALSE,
                         label = "Longitude range",
                         value = min_max,
                         min = min_max[1],
                         max = min_max[2],
                         width = "100%")
    }
  })
  
  output$lat_range_UI <- renderUI({
    if (tibble::is.tibble(df())) {
      min_max <- if (input$longlat_invert == "Lat/Long") round(c(min(df()$Long), max(df()$Long)), digits = 2) else round(c(min(df()$Lat), max(df()$Lat)), digits = 2)
      
      shiny::sliderInput(inputId = "lat_range",
                         round = TRUE,
                         label = "Latitude range",
                         value = min_max,
                         min = min_max[1],
                         max = min_max[2],
                         width = "100%")
    }
  })
  
  df <- callModule(mod_file_input_server, "file_input_ui_1")
  
  df_f <- reactive({
    
    if (is.numeric(input$sample_size)) {
      df_filtered <- df() %>% 
        dplyr::slice(base::sample(x = 1:nrow(df()),
                                  size = input$sample_size,
                                  replace = FALSE))
      
      if (input$longlat_invert == "Lat/Long")  {
        df_filtered <- df_filtered %>% 
          dplyr::rename(lon_new = Lat,
                        lat_new = Long) %>% 
          dplyr::transmute(Long = lon_new, Lat = lat_new, Value)
      }
      
      df_filtered <- df_filtered %>% 
        dplyr::filter(Long>=input$long_range[1]&Long<=input$long_range[2],
                      Lat>=input$lat_range[1]&Lat<=input$lat_range[2])
      return(df_filtered)
    }

    
  })
  
  sf <- reactive({
    if (is.null(df_f())==FALSE) {
      sf_temp <- sf::st_as_sf(df_f(),
                   coords = c("Long","Lat"),
                   crs = 4326) %>% 
        sf::st_transform(crs = 3857)
      if (input$geolocate_panel==TRUE) {
        
        if (is.element("Country (World)", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_world(resolution = 60) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_world(resolution = 60) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
        
        if (is.element("NUTS0", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_nuts_eu(level = 0)%>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_nuts_eu(level = 0)%>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
        
        if (is.element("NUTS1", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_nuts_eu(level = 1) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_nuts_eu(level = 1) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
        
        if (is.element("NUTS2", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_nuts_eu(level = 2) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_nuts_eu(level = 2) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
        
        if (is.element("NUTS3", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_nuts_eu(level = 3) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_nuts_eu(level = 3) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
        
        if (is.element("LAU", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_lau_eu() %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   longlat2map::ll_get_lau_eu() %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
      }
      
      return(sf_temp)
    }
  })
  
  output$df_DT <- DT::renderDT(
    if (is.null(sf())==FALSE) sf() %>% sf::st_drop_geometry(),
    options = list(
      pageLength = 5
      ),
    rownames = FALSE
    )

  output$map_gg <- renderPlot(expr = {
    if (is.null(df_f())==TRUE) {
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = longlat2map::ll_get_world(resolution = 60) %>% 
                           sf::st_transform(crs = 4326)) +
        ggplot2::theme_minimal()
    } else {
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = longlat2map::ll_get_world(resolution = 60) %>% 
                         sf::st_transform(crs = 4326)) +
      ggplot2::geom_sf(data = sf() %>% 
                         sf::st_transform(crs = 4326),
                       mapping = ggplot2::aes(colour = Value)) +
        ggplot2::scale_x_continuous(limits = as.numeric(input$long_range)) +
        ggplot2::scale_y_continuous(limits = as.numeric(input$lat_range)) +
        ggplot2::theme_minimal()
    }
  })
  
  
}
