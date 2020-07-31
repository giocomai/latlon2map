#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize=golem::get_golem_options("shiny.maxRequestSize"))
  if (is.null(golem::get_golem_options("ll_folder_path"))==FALSE) {
    latlon2map::ll_set_folder(path = golem::get_golem_options("ll_folder_path"))
  }
  shiny::addResourcePath(prefix = "app", directoryPath = system.file("app", package = "latlon2map"))
  
  #### UI ####
  
  output$sample_size_UI <- renderUI({
    if (tibble::is_tibble(df_original())) {
      if (nrow(df_original())>0) {
        shiny::sliderInput(inputId = "sample_size",
                           round = TRUE,
                           label = "Sample size",
                           value = if (nrow(df_original())>1000) 1000 else nrow(df_original()),
                           min = 1,
                           max = nrow(df_original()))
      }
    }
  })
  
  ##### slider latitude/longitude UI #####
  
  output$long_range_UI <- renderUI({
    if (tibble::is_tibble(df())) {
      if (sum(is.element(colnames(df()), c("Latitude", "Longitude")))==2) {
      min_max <- c(min(df()$Longitude), max(df()$Longitude))
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
        
        min_max <- c(min(df()$Latitude), max(df()$Latitude))
        
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
  
  observe({
    shiny::updateSliderInput(session = session,
                             inputId = "long_range",
                             value = c(input$map_gg_brush[["xmin"]],
                                       input$map_gg_brush[["xmax"]]))
    shiny::updateSliderInput(session = session,
                             inputId = "lat_range",
                             value = c(input$map_gg_brush[["ymin"]],
                                       input$map_gg_brush[["ymax"]]))
    session$resetBrush("map_gg_brush")
  })
  
  output$reset_full_range_UI <- renderUI({
    if (is.data.frame(sf())==TRUE) {
  #    if((sum(input$lat_range!=c(min(df()$Latitude), max(df()$Latitude)))>0|sum(input$long_range!=c(min(df()$Longitude), max(df()$Longitude)))>0))
      actionButton("reset_full_range", "Reset coordinate range")
    } 
  })
  
  observe({
    input$reset_full_range
    if (is.null(input$reset_full_range)==FALSE) {
        shiny::updateSliderInput(session = session,
                                 inputId = "long_range",
                                 value =  c(min(df()$Longitude), max(df()$Longitude)))
        shiny::updateSliderInput(session = session,
                                 inputId = "lat_range",
                                 value = c(min(df()$Latitude), max(df()$Latitude)))
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
                         label = "Additional column(s)",
                         choices = colnames(df_original()),
                         multiple = TRUE)
    }
  })
  
  output$colour_column_selector_ui <- renderUI({
    if (tibble::is_tibble(df())) {
      shiny::selectInput(inputId = "colour_column_selector",
                         label = "Column to use for colour of points",
                         choices = c("-", colnames(df())),
                         multiple = FALSE)
    }
  })
  
  output$size_column_selector_ui <- renderUI({
    if (tibble::is_tibble(df())) {
      shiny::selectInput(inputId = "size_column_selector",
                         label = "Column to use for size of points",
                         choices = c("-", colnames(df())),
                         multiple = FALSE)
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
                              crs = 4326,
                              remove = FALSE) %>% 
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
      
      return(sf_temp %>% sf::st_transform(crs = 4326))
    }
  })
  
  ##### tables #####
  
  output$df_DT <- DT::renderDT(
    if (is.null(sf())==FALSE) {
      
      if (input$map_type=="Dynamic"&is.null(input$map_lf_bounds)==FALSE) {
        sf() %>%
          sf::st_drop_geometry() %>% 
          dplyr::filter(Latitude < input$map_lf_bounds[["north"]],
                        Latitude > input$map_lf_bounds[["south"]],
                        Longitude < input$map_lf_bounds[["east"]],
                        Longitude > input$map_lf_bounds[["west"]])
      } else if (is.null(input$map_gg_)==FALSE&input$map_type=="Static") {
        sf() %>%
          sf::st_drop_geometry() %>% 
          shiny::brushedPoints(input$map_gg_brush, xvar = "Longitude", yvar = "Latitude")
      } else {
        sf() %>%
          sf::st_drop_geometry()
      }
    },
    options = list(
      pageLength = 5,
      dom = "tip"
    ),
    rownames = FALSE,
    filter = "top",
    caption = "Data points visible in current view"
  )
  
  output$df_DT_clicked <- DT::renderDT(
    if (is.null(sf())==FALSE) {
      
      if (is.null(input$map_lf_marker_click)==FALSE&input$map_type=="Dynamic") {
        sf() %>%
          sf::st_drop_geometry() %>% 
          dplyr::filter(Latitude == input$map_lf_marker_click[["lat"]],
                        Longitude == input$map_lf_marker_click[["lng"]]) 
      } else if (is.null(input$map_gg_dblclick)==FALSE&input$map_type=="Static") {
        sf() %>%
          sf::st_drop_geometry() %>% 
          shiny::nearPoints(input$map_gg_dblclick, xvar = "Longitude", yvar = "Latitude")
      }
    },
    options = list(
      pageLength = 3,
      dom = "tip"
    ),
    rownames = FALSE,
    filter = "top",
    caption = "Last clicked"
  )
  
  
  
  ##### maps #####
  map_gg_reactive <- shiny::reactive({
    if (input$map_type=="Static") {
      gg_map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = latlon2map::ll_get_world(resolution = 60) %>% 
                           sf::st_transform(crs = 4326)) +
        ggplot2::theme_minimal()
      
      if (is.null(df_f())==FALSE) {
        if (input$highlight_mode=="Manually selected rows") {
          sf_selected <- sf() %>% 
            dplyr::mutate(Selected = is.element(dplyr::row_number(),input$df_DT_rows_selected))
          if (sum(sf_selected$Selected)>0) {
            if (input$only_selected==TRUE) {
              gg_map <- gg_map +
                ggplot2::geom_sf(data = sf_selected %>% 
                                   dplyr::filter(Selected),
                                 mapping = ggplot2::aes(colour = Selected)) +
                ggplot2::scale_color_manual(values = c("#6f2c91")) +
                ggplot2::theme(legend.position = "none")
              
            } else {
              gg_map <- gg_map +
                ggplot2::geom_sf(data = sf_selected %>% 
                                   dplyr::filter(!Selected),
                                 colour = "#a6ce39") +
                ggplot2::geom_sf(data = sf_selected %>% 
                                   dplyr::filter(Selected), colour = "#6f2c91") +
                ggplot2::theme(legend.position = "none")
            }
          } else {
            gg_map <- gg_map +
              ggplot2::geom_sf(data = sf_selected, colour = "#6f2c91")
          }
        } else if (input$highlight_mode=="Data columns") {
          if (input$colour_column_selector!="-"&input$size_column_selector!="-") {
            gg_map <- gg_map +
              ggplot2::geom_sf(data = sf(),
                               mapping = ggplot2::aes_string(colour = input$colour_column_selector,
                                                             size = input$size_column_selector)) 
          } else if (input$colour_column_selector!="-"&input$size_column_selector=="-") {
            gg_map <- gg_map +
              ggplot2::geom_sf(data = sf(),
                               mapping = ggplot2::aes_string(colour = input$colour_column_selector)) 
          } else if (input$colour_column_selector=="-"&input$size_column_selector!="-") {
            gg_map <- gg_map +
              ggplot2::geom_sf(data = sf(),
                               mapping = ggplot2::aes_string(size = input$size_column_selector)) 
          }
        } else {
          gg_map <- gg_map +
            ggplot2::geom_sf(data = sf())
        }
        gg_map <- gg_map +  
          ggplot2::scale_x_continuous(limits = as.numeric(input$long_range)) +
          ggplot2::scale_y_continuous(limits = as.numeric(input$lat_range)) 
      }
      gg_map
    }
  })
  
  output$map_gg <- renderPlot(expr = {
    map_gg_reactive()
  })
  
  map_lf_reactive <- shiny::reactive({
    if (input$map_type=="Dynamic") {
      base_lf <- leaflet::leaflet() %>%
        leaflet::addTiles(group = "OpenStreetMap") %>%
        leaflet::addProviderTiles(provider = "Stamen.Terrain", group = "Terrain") %>%
        leaflet::addProviderTiles(provider = "Stamen.TonerLite", group = "Black and white") %>%
        leaflet::addProviderTiles(provider = "Esri.WorldImagery", group = "Satellite") %>% 
        leaflet::addLayersControl(
          baseGroups = c("OpenStreetMap",
                         "Terrain",
                         "Black and white",
                         "Satellite"),
          options = leaflet::layersControlOptions(collapsed = FALSE))
      if (is.data.frame(sf())==TRUE) {
        if (input$highlight_mode=="Manually selected rows") {
          sf_selected <- sf() %>% 
            dplyr::mutate(Selected = is.element(dplyr::row_number(),input$df_DT_rows_selected)) %>% 
            dplyr::mutate(selected_colour = dplyr::if_else(condition = Selected,
                                                           true = "#6f2c91",
                                                           false = "#a6ce39"))
          if (sum(sf_selected$Selected)>0) {
            base_lf %>%
              leaflet::addCircleMarkers(data = sf_selected %>% dplyr::filter(!Selected),
                                        color = ~selected_colour,
                                        group = "Not selected") %>% 
              leaflet::addCircleMarkers(data = sf_selected %>% dplyr::filter(Selected),
                                        color = ~selected_colour,
                                        group = "Selected") %>% 
              leaflet::addLayersControl(baseGroups = c("OpenStreetMap",
                                                       "Terrain",
                                                       "Black and white",
                                                       "Satellite"),
                                        overlayGroups = c("Selected",
                                                          "Not selected"),
                                        options = leaflet::layersControlOptions(collapsed = FALSE))
          } else {
            base_lf %>%
              leaflet::addCircleMarkers(data = sf(),
                                        color = "#6f2c91")
          }
        } else if (input$highlight_mode=="Data columns") {
          
          if (is.numeric(sf()[[input$colour_column_selector]])==TRUE) {
            pal <- leaflet::colorNumeric(
              palette = 'Blues',
              domain = sf()[[input$colour_column_selector]],
              reverse = TRUE
            )
          } else {
            pal <- leaflet::colorFactor(
              palette = 'Dark2',
              domain = sf()[[input$colour_column_selector]]
            )
          }
          
          if (input$size_column_selector!="-") {
            size_resized <- scales::rescale(x = sf()[[input$size_column_selector]],
                                            to = c(1, 10))
          }
          
          if (input$colour_column_selector!="-"&input$size_column_selector!="-") {
            
            base_lf %>%
              leaflet::addCircleMarkers(data = sf(),
                                        color = ~pal(sf()[[input$colour_column_selector]]),
                                        radius = size_resized) %>% 
              leaflet::addLayersControl(baseGroups = c("OpenStreetMap",
                                                       "Terrain",
                                                       "Black and white",
                                                       "Satellite"),
                                        options = leaflet::layersControlOptions(collapsed = FALSE)) %>% 
              leaflet::addLegend(position = "bottomright",
                                 pal = pal,
                                 values = sf()[[input$colour_column_selector]])
            
          } else if (input$colour_column_selector!="-"&input$size_column_selector=="-") {
            
            base_lf %>%
              leaflet::addCircleMarkers(data = sf(),
                                        color = ~pal(sf()[[input$colour_column_selector]])) %>% 
              leaflet::addLayersControl(baseGroups = c("OpenStreetMap",
                                                       "Terrain",
                                                       "Black and white",
                                                       "Satellite"),
                                        options = leaflet::layersControlOptions(collapsed = FALSE)) %>% 
              leaflet::addLegend(position = "bottomright",
                                 pal = pal,
                                 values = sf()[[input$colour_column_selector]])
            
          } else if (input$colour_column_selector=="-"&input$size_column_selector!="-") {
            
            base_lf %>%
              leaflet::addCircleMarkers(data = sf(),
                                        radius = size_resized) %>% 
              leaflet::addLayersControl(baseGroups = c("OpenStreetMap",
                                                       "Terrain",
                                                       "Black and white",
                                                       "Satellite"),
                                        options = leaflet::layersControlOptions(collapsed = FALSE))
          }
        }
      } else {
        base_lf
      }
    }
  })
  
  output$map_lf <- leaflet::renderLeaflet({
    map_lf_reactive()
  })
  
  ##### Downloads #####
  
  output$download_map_gg_png <- downloadHandler(filename = "latlon2map.png",
                                               content = function(con) {
                                                 ggplot2::ggsave(filename = con,
                                                                 plot = map_gg_reactive(),
                                                                 type = "cairo")
                                               }
  )
  
  output$download_map_gg_pdf <- downloadHandler(filename = "latlon2map.pdf",
                                               content = function(con) {
                                                 ggplot2::ggsave(filename = con,
                                                                 plot = map_gg_reactive(),
                                                                 device = cairo_pdf)
                                               }
  )
  
  output$download_map_lf_html <- downloadHandler(filename = "latlon2map.html",
                                                content = function(con) {
                                                  htmlwidgets::saveWidget(widget = map_lf_reactive(),
                                                                          file = con)
                                                }
  )
  
  output$download_df_csv <- downloadHandler(filename = "latlon2map.csv",
                                                 content = function(con) {
                                                   readr::write_csv(x = sf() %>% sf::st_drop_geometry(),
                                                                    path = con)
                                                 }
  )
  
  output$download_df_xlsx <- downloadHandler(filename = "latlon2map.xslx",
                                            content = function(con) {
                                              writexl::write_xlsx()(x = sf() %>% sf::st_drop_geometry(),
                                                               path = con)
                                            }
  )
  
  output$download_df_ods <- downloadHandler(filename = "latlon2map.ods",
                                             content = function(con) {
                                               readODS::write_ods(x = sf() %>% sf::st_drop_geometry(),
                                                                path = con)
                                             }
  )

  
}