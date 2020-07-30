#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shiny::fluidPage(
      sidebarLayout(
        sidebarPanel =
          shiny::sidebarPanel(mod_file_input_ui("file_input_ui_1"),
                              # shiny::radioButtons(inputId = "file_type",
                              #                     label = "File type",
                              #                     choices = c(".csv",
                              #                                 ".tsv",
                              #                                 ".xlsx",
                              #                                 ".ods"),
                              #                     inline = TRUE),
                              shiny::radioButtons(inputId = "map_type",
                                                  label = "Map type",
                                                  choices = c("Static",
                                                              "Dynamic"),
                                                  selected = "Static",
                                                  inline = TRUE),
                              shiny::uiOutput(outputId = "latitude_selector_ui"),
                              shiny::uiOutput(outputId = "longitude_selector_ui"),
                              shiny::uiOutput(outputId = "other_columns_selector_ui"),
                              shiny::radioButtons(inputId = "highlight_mode",
                                                  label = "Color-code in map based on:",
                                                  choices = c("Manually selected rows",
                                                              "Data columns")
                                                    ),
                              shiny::conditionalPanel(condition = "input.highlight_mode=='Manually selected rows'",
                                                      shiny::checkboxInput(inputId = "only_selected",
                                                                           label = "Show only selected rows",
                                                                           value = FALSE)
                              ), 
                              shiny::conditionalPanel(condition = "input.highlight_mode=='Data columns'",
                                                      shiny::uiOutput(outputId = "colour_column_selector_ui"),
                                                      shiny::uiOutput(outputId = "size_column_selector_ui"),
                              ), 
                              shiny::uiOutput(outputId = "sample_size_UI"),
                              shiny::checkboxInput(inputId = "geolocate_panel",
                                                   label = "Geolocate points",
                                                   value = FALSE),
                              shiny::conditionalPanel(condition = "input.geolocate_panel==true",
                                                      shiny::radioButtons(inputId = "geolocate_selector",
                                                                                label = "Geolocate by",
                                                                                choices = c("Country (World)",
                                                                                            "NUTS0",
                                                                                            "NUTS1",
                                                                                            "NUTS2",
                                                                                            "NUTS3",
                                                                                            "LAU"),
                                                                                selected = "Country (World)",
                                                                                inline = TRUE), 
                                                      shiny::radioButtons(inputId = "join_type",
                                                                          label = "Match only if within, or also if near (e.g. offshore)?",
                                                                          choices = c("Within",
                                                                                      "Nearest")))
          ),
        mainPanel = 
          shiny::mainPanel(
            shiny::conditionalPanel(condition = "input.map_type=='Static'", {
              shiny::plotOutput(outputId = "map_gg")
            }),
            shiny::conditionalPanel(condition = "input.map_type=='Dynamic'", {
              leaflet::leafletOutput("map_lf")
            }),
            shiny::uiOutput(outputId = "long_range_UI"), 
            shiny::uiOutput(outputId = "lat_range_UI"),
            DT::DTOutput(outputId = "df_DT")),
            position = c("left", "right"),
            fluid = TRUE)
      )
    )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'latlon2map'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

