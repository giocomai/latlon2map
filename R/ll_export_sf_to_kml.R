#' Export sf objects into kml file that can be used with Google Earth, Google Maps, etc. 
#'
#' Attention: this function requires libkml. 
#' 
#' For further details on the exact meaning of each of the parameters, please consult the documentation of OGR (used by GDAL to pass parameters to .kml): https://gdal.org/user/ogr_feature_style.html
#'
#' @param sf An object of class `sf`
#' @param path Path where to save the .kml output.
#' @param name Column to be used for names. 
#' @param label_text Column to be used as label text. Defaults to NULL. Corresponds to "LABEL" element in OGR.
#' @param label_font Font family to be used for the font. Defaults to ""Roboto Sans, Noto Sans, Helvetica"
#' @param label_size Size of the label. Defaults to "24pt"
#' @param label_placement Defaults to "m" (centre and middle-aligned). For more options, check: https://gdal.org/user/ogr_feature_style.html
#' @param icon_url Defaults to "" for no URL. Corresponds to "SYMBOL" in OGR. In case of wrong inputs, Google Earth may show you an ugly yellow pushpin instead (i.e. default to http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png). Available icons offered by Google available at this link: http://kml4earth.appspot.com/icons.html
#' @param icon_colour Defaults to "#FF0000" (i.e. white). Ignored if png, given. 
#' @param line_colour Defaults to "#FF0000" (i.e. white). Line corresponds to "PEN" in OGR. Accepts 8-digit hex codes to include transparency.
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
                                description = NULL, 
                                label_text = NULL,
                                label_font = "Roboto Sans, Noto Sans, Helvetica",
                                label_size = "24pt",
                                label_placement = "m",
                                line_colour = "#FF0000", 
                                line_width = "3px",
                                icon_url = "",
                                icon_colour = "#FF0000", 
                                fill_colour = NULL 
                                ) {
  sf_pre_kml <- sf %>% 
    dplyr::mutate(OGR_STYLE = paste0(dplyr::if_else(condition = is.null(fill_colour), true = "", false = paste0("BRUSH(fc:", fill_colour, ");")),
                                     "PEN(c:", line_colour, ",w:", line_width, ");",
                                     'SYMBOL(c:', icon_colour,',id:', icon_url, ');',
                                     dplyr::if_else(condition = is.null(label_text), true = "", false = paste0('LABEL(f:', label_font, ",s:", label_size, ',t:', label_text, ",m:", label_placement, ")"))))
  
  if (is.null(name)==FALSE) {
    sf_pre_kml[["name"]] <- sf[[name]]
  }
  
  if (is.null(name)==FALSE) {
    sf_pre_kml[["description"]] <- sf[[description]]
  }
  
  sf::st_write(obj = sfsf_pre_kml,
               dsn = path,
               driver = "libkml")
}