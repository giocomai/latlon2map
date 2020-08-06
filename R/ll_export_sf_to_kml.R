#' Export sf objects into kml file that can be used with Google Earth, Google Maps, etc. 
#'
#' Attention: this function requires libkml. 
#' 
#' For further details on the exact meaning of each of the parameters, please consult the documentation of OGR (used by GDAL to pass parameters to .kml): https://gdal.org/user/ogr_feature_style.html
#'
#' @param sf An object of class `sf`
#' @param path Path where to save the .kml output.
#' @param name Column to be used for names. 
#' @param description Column to be used for description.
#' @param keep_other_columns Logical, defaults to TRUE. If you don't want to keep in the output data columns present in the original `sf` object, set this to FALSE.
#' @param label_text Column to be used as label text. Defaults to NULL. Corresponds to "LABEL" element in OGR.
#' @param label_font Font family to be used for the font. Defaults to "Roboto Sans, Noto Sans, Helvetica"
#' @param label_size Size of the label. Defaults to "24pt"
#' @param label_placement Defaults to "m" (centre and middle-aligned). For more options, check: https://gdal.org/user/ogr_feature_style.html
#' @param icon_url Defaults to "" for no URL. Corresponds to "SYMBOL" in OGR. In case of wrong inputs, Google Earth may show you an ugly yellow pushpin instead (i.e. default to http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png). Available icons offered by Google available at this link: http://kml4earth.appspot.com/icons.html
#' @param icon_colour Defaults to "#000000ff" (i.e. black, with 100% opacity).
#' @param line_colour Defaults to "#ffffffff" (i.e. white, with 100% opacity). Line corresponds to "PEN" in OGR. Accepts 8-digit hex codes to include transparency.
#' @param line_width Defaults to "3pt". Line corresponds to "PEN" in OGR. Besides pt (points), other acceptable units are `g`: Map Ground Units (whatever the map coordinate units are), `px` Pixels, `pt` Points (1/72 inch), `mm` Millimeters, `cm` Centimeters, `in` Inches.
#' @param fill_colour Defaults to NULL. Fill corresponds to "BRUSH" in OGR. If given, colour to be used for filling polygons. 
#'
#' @return
#' @export
#'
#' @examples
ll_export_sf_to_kml <- function(sf,
                                path,
                                name = NULL,
                                keep_other_columns = TRUE,
                                description = NULL,
                                label_text = NULL,
                                label_font = "Roboto Sans, Noto Sans, Helvetica",
                                label_size = "24pt",
                                label_placement = "m",
                                line_colour = "#ffffffff", 
                                line_width = "3px",
                                icon_url = "",
                                icon_colour = "#000000ff", 
                                fill_colour = NULL 
                                ) {
  if (is.null(fill_colour)==FALSE) {
    brush <- paste0("BRUSH(fc:", fill_colour, ");")
  } else {
    brush <- ""
  }
  
  if (is.null(label_text)==FALSE) {
    label <- paste0('LABEL(f:', label_font, ",s:", label_size, ',t:', label_text, ",m:", label_placement, ")")
  } else {
    label <- ""
  }
  
  sf_pre_kml <- sf %>% 
    dplyr::mutate(OGR_STYLE = paste0(brush,
                                     "PEN(c:", line_colour, ",w:", line_width, ");",
                                     'SYMBOL(c:', icon_colour,',id:', icon_url, ');',
                                     label))
  
  if (keep_other_columns==FALSE) {
    sf_pre_kml <- sf_pre_kml %>% 
      dplyr::select(OGR_STYLE)
  }
  
  if (is.null(name)==FALSE) {
    sf_pre_kml[["name"]] <- sf[[name]]
  }
  
  if (is.null(description)==FALSE) {
    sf_pre_kml[["description"]] <- sf[[description]]
  }
  
  sf::st_write(obj = sf_pre_kml,
               dsn = path,
               driver = "libkml")
}