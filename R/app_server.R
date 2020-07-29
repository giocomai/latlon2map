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
    if (tibble::is_tibble(df_original())) {
      shiny::sliderInput(inputId = "sample_size",
                         round = TRUE,
                         label = "Sample size",
                         value = if (nrow(df_original())>1000) 1000 else nrow(df_original()),
                         min = 1,
                         max = nrow(df()))
    }
  })
  
  output$long_range_UI <- renderUI({
    if (tibble::is_tibble(df())) {
      if (sum(is.element(colnames(df()), c("Latitude", "Longitude")))==2) {
      min_max <- round(c(min(df()$Longitude), max(df()$Longitude)), digits = 2)
        shiny::sliderInput(inputId = "long_range",
                           round = FALSE,
                           label = "Longitude range",
                           value = min_max,
                           min = min_max[1],
                           max = min_max[2],
                           width = "100%")
      }
    }
  })
  
  output$lat_range_UI <- renderUI({
    if (tibble::is_tibble(df())) {
      if (sum(is.element(colnames(df()), c("Latitude", "Longitude")))==2) {
        
        min_max <- round(c(min(df()$Latitude), max(df()$Latitude)), digits = 2)
        
        shiny::sliderInput(inputId = "lat_range",
                           round = TRUE,
                           label = "Latitude range",
                           value = min_max,
                           min = min_max[1],
                           max = min_max[2],
                           width = "100%")
      }
    }
  })
  
  
  output$latitude_selector_ui <- renderUI({
    if (tibble::is_tibble(df_original())) {
      shiny::selectInput(inputId = "latitude_selector",
                         label = "Latitude column:",
                         choices = c("-", colnames(df_original())),
                         selected = NA)
    }
  })
  
  output$longitude_selector_ui <- renderUI({
    if (tibble::is_tibble(df_original())) {
      shiny::selectInput(inputId = "longitude_selector",
                         label = "Longitude column:",
                         choices = c("-", colnames(df_original())),
                         selected = NA)
    }
  })
  
  output$other_columns_selector_ui <- renderUI({
    if (tibble::is_tibble(df_original())) {
      shiny::selectInput(inputId = "other_columns_selector",
                         label = "Select column(s) with data to keep",
                         choices = colnames(df_original()),
                         multiple = TRUE)
    }
  })
  
  #### read file ####
  
  df_original <- callModule(mod_file_input_server, "file_input_ui_1")
  
  df <- reactive({
    if (is.character(input$latitude_selector)==TRUE&is.character(input$longitude_selector)==TRUE) {
      if (is.character(input$other_columns_selector)==TRUE) {
        df_original() %>% 
          dplyr::select(Latitude =  which(colnames(df_original())==input$latitude_selector),
                        Longitude = which(colnames(df_original())==input$longitude_selector),
                        which(is.element(colnames(df_original()), input$other_columns_selector)))
      } else {
        df_original() %>% 
          dplyr::select(Latitude = which(colnames(df_original())==input$latitude_selector),
                        Longitude = which(colnames(df_original())==input$longitude_selector))
      }
    }
  })
  
  df_f <- reactive({
    if (tibble::is_tibble(df())) {
       if (sum(is.element(colnames(df()), c("Latitude", "Longitude")))==2) {
        if (is.numeric(input$sample_size)&is.numeric(input$long_range)&is.numeric(input$lat_range)) {
          df() %>% 
            dplyr::slice(base::sample(x = 1:nrow(df()),
                                      size = input$sample_size,
                                      replace = FALSE)) %>% 
            dplyr::filter(Longitude>=input$long_range[1]&Longitude<=input$long_range[2],
                          Latitude>=input$lat_range[1]&Latitude<=input$lat_range[2])
        }
      }
    }
  })
  
  sf <- reactive({
    if (is.null(df_f())==FALSE) {
      sf_temp <- sf::st_as_sf(df_f(),
                              coords = c("Longitude","Latitude"),
                              crs = 4326) %>% 
        sf::st_transform(crs = 3857)
      if (input$geolocate_panel==TRUE) {
        
        if (is.element("Country (World)", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_world(resolution = 60) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_world(resolution = 60) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
        
        if (is.element("NUTS0", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_nuts_eu(level = 0)%>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_nuts_eu(level = 0)%>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
        
        if (is.element("NUTS1", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_nuts_eu(level = 1) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_nuts_eu(level = 1) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
        
        if (is.element("NUTS2", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_nuts_eu(level = 2) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_nuts_eu(level = 2) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
        
        if (is.element("NUTS3", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_nuts_eu(level = 3) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_nuts_eu(level = 3) %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_within)        
          }
        }
        
        if (is.element("LAU", input$geolocate_selector)) {
          if (input$join_type=="Nearest") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_lau_eu() %>% 
                                     sf::st_transform(crs = 3857),
                                   join = sf::st_nearest_feature)
          } else if (input$join_type=="Within") {
            sf_temp <- sf::st_join(sf_temp,
                                   latlon2map::ll_get_lau_eu() %>% 
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
    
    gg_map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = latlon2map::ll_get_world(resolution = 60) %>% 
                         sf::st_transform(crs = 4326)) +
      ggplot2::theme_minimal()
    
    if (is.null(df_f())==FALSE) {
      if (ncol(sf())>1) {
        gg_map <- gg_map +
          ggplot2::geom_sf(data = sf() %>% 
                             sf::st_transform(crs = 4326),
                           mapping = ggplot2::aes_string(colour = input$other_columns_selector[[1]])) 
      } else {
        gg_map <- gg_map +
          ggplot2::geom_sf(data = sf() %>% 
                             sf::st_transform(crs = 4326))
      }
      gg_map <- gg_map +  
        ggplot2::scale_x_continuous(limits = as.numeric(input$long_range)) +
        ggplot2::scale_y_continuous(limits = as.numeric(input$lat_range)) +
        ggplot2::theme_minimal()
    }
    gg_map
  })
  
  
}