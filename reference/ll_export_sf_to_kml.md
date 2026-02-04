# Export sf objects into kml file that can be used with Google Earth, Google Maps, etc.

Attention: this function requires libkml.

## Usage

``` r
ll_export_sf_to_kml(
  sf,
  path,
  name = NULL,
  keep_other_columns = TRUE,
  description = NULL,
  label_text = NULL,
  label_font = "Roboto Sans, Noto Sans, Helvetica",
  label_size = "24pt",
  label_placement = "m",
  label_scale = NULL,
  line_colour = "#ffffffff",
  line_width = "3px",
  icon_url = "",
  icon_colour = "#000000ff",
  icon_scale = NULL,
  fill_colour = NULL
)
```

## Arguments

- sf:

  An object of class `sf`

- path:

  Path where to save the .kml output.

- name:

  Column to be used for names.

- keep_other_columns:

  Logical, defaults to TRUE. If you don't want to keep in the output
  data columns present in the original `sf` object, set this to FALSE.

- description:

  Column to be used for description.

- label_text:

  Column to be used as label text. Defaults to NULL. Corresponds to
  "LABEL" element in OGR.

- label_font:

  Font family to be used for the font. Defaults to "Roboto Sans, Noto
  Sans, Helvetica"

- label_size:

  Size of the label. Defaults to "24pt"

- label_placement:

  Defaults to "m" (centre and middle-aligned). For more options, check:
  https://gdal.org/user/ogr_feature_style.html

- label_scale:

  Scale of label. Defaults to NULL. If given, changes label size (e.g. 1
  = default, 2 = twice as big, 0.5, half as big, etc.)

- line_colour:

  Defaults to "#ffffffff" (i.e. white, with 100% opacity). Line
  corresponds to "PEN" in OGR. Accepts 8-digit hex codes to include
  transparency.

- line_width:

  Defaults to "3pt". Line corresponds to "PEN" in OGR. Besides pt
  (points), other acceptable units are `g`: Map Ground Units (whatever
  the map coordinate units are), `px` Pixels, `pt` Points (1/72 inch),
  `mm` Millimeters, `cm` Centimeters, `in` Inches.

- icon_url:

  Defaults to "" for no URL. Corresponds to "SYMBOL" in OGR. In case of
  wrong inputs, Google Earth may show you an ugly yellow pushpin instead
  (i.e. default to
  http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png).
  Available icons offered by Google available at this link:
  http://kml4earth.appspot.com/icons.html

- icon_colour:

  Defaults to "#000000ff" (i.e. black, with 100% opacity).

- icon_scale:

  Defaults to NULL. If given, changes icon size (e.g. 1 = default, 2 =
  twice as big, 0.5, half as big, etc.)

- fill_colour:

  Defaults to NULL. Fill corresponds to "BRUSH" in OGR. If given, colour
  to be used for filling polygons.

## Details

Attention: label styling is not currently functional, likely due to
issues in passing arguments to libkml. In order to change label size,
use label_scale, which directly edits the xml file.

For further details on the exact meaning of each of the parameters,
please consult the documentation of OGR (used by GDAL to pass parameters
to .kml): https://gdal.org/user/ogr_feature_style.html
