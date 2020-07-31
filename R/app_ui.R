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
                              shiny::uiOutput(outputId = "long_range_UI"),
                              shiny::uiOutput(outputId = "lat_range_UI"),
                              shiny::uiOutput(outputId = "reset_full_range_UI"),
                              shiny::uiOutput(outputId = "sample_size_UI"),
                              shiny::conditionalPanel(condition = "input.highlight_mode=='Manually selected rows'&input.map_type=='Static'",
                                                      shiny::checkboxInput(inputId = "only_selected",
                                                                           label = "Show only selected rows",
                                                                           value = FALSE)
                              ),
                              shiny::checkboxInput(inputId = "colour_code_check",
                                                   label = "Colour-code data on the map",
                                                   value = FALSE),
                              shiny::conditionalPanel(condition ="input.colour_code_check==true", 
                                                      shiny::radioButtons(inputId = "highlight_mode",
                                                                          label = "Colour-code in map based on:",
                                                                          choices = c("Manually selected rows",
                                                                                      "Data columns")
                                                      ), 
                                                      shiny::conditionalPanel(condition = "input.highlight_mode=='Data columns'",
                                                                              shiny::helpText("Make sure you have included all relevant columns in `Additional column(s)` above"),
                                                                              shiny::uiOutput(outputId = "colour_column_selector_ui"),
                                                                              shiny::uiOutput(outputId = "size_column_selector_ui"),
                                                      )), 
                              
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
                                                                                      "Nearest"))),
                              shiny::HTML("<hr />"),
                              #shiny::h3(tags$a("A tool by EDJNet", href='https://www.europeandatajournalism.eu/')),
                              shiny::h3(tags$a(tags$img(src = fs::path("www", "img", "edjnet_logo_full.svg")), href='https://www.europeandatajournalism.eu/'))
          ),
        mainPanel = 
          shiny::mainPanel(
            shiny::conditionalPanel(condition = "input.map_type=='Static'", {
              shiny::plotOutput(outputId = "map_gg", 
                                dblclick = "map_gg_dblclick",
                                brush = "map_gg_brush")
            }),
            shiny::conditionalPanel(condition = "input.map_type=='Dynamic'", {
              leaflet::leafletOutput("map_lf")
            }),
            DT::DTOutput(outputId = "df_DT_clicked"),
            DT::DTOutput(outputId = "df_DT"),
            shiny::downloadButton(outputId = "download_df_csv",
                                  label =  "Download dataset (csv)"),
          shiny::downloadButton(outputId = "download_map_gg_png",
                                label =  "Download static map as image (png)"),
          shiny::downloadButton(outputId = "download_map_gg_pdf",
                                label =  "Download static map as pdf"),
          shiny::downloadButton(outputId = "download_map_lf_html",
                                label =  "Download dynamic map as html")),
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

