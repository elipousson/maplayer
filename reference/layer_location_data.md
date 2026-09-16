# Layer location data into a ggplot2 map

Helper function to make a ggplot2 layer from data returned by
[`get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.html).
For text geoms, the required aesthetic mapping is set based on the
name_col but values passed to mapping take precedence.

## Usage

``` r
layer_location_data(
  mapping = NULL,
  data = NULL,
  geom = "sf",
  location = NULL,
  dist = getOption("maplayer.dist"),
  diag_ratio = getOption("maplayer.diag_ratio"),
  unit = getOption("maplayer.unit", default = "meter"),
  asp = getOption("maplayer.asp"),
  package = getOption("maplayer.data_package"),
  pkg = getOption("maplayer.data_package"),
  fileext = getOption("maplayer.data_fileext", "gpkg"),
  filetype = NULL,
  fn = NULL,
  layer_fn = NULL,
  crop = TRUE,
  trim = FALSE,
  from_crs = getOption("maplayer.from_crs", 4326),
  crs = getOption("maplayer.crs", 3857),
  label_col = "name",
  smooth_params = NULL,
  shadow_params = NULL,
  basemap = FALSE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  Character string (e.g. url, file path, or name of data from package)
  for a spatial data or a `sf`, `sfc`, or `bbox` object with geometry
  overlapping the location. If data is `NULL`, all unnamed parameters
  are passed to
  [`sfext::read_sf_ext()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
  with a bbox based on location. If data is not `NULL` and not a
  data.frame, url, file path, or bbox, conversion to a sf object will
  still always be attempted with
  [`sfext::as_sf()`](https://elipousson.github.io/sfext/reference/as_sf.html).

- geom:

  A character string indicating which ggplot2 geom to use, Default:
  'sf'. Options include "sf"
  ([`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)),
  "icon"
  ([`layer_icon()`](https://elipousson.github.io/maplayer/reference/layer_icon.md)),
  "markers"
  ([`layer_markers()`](https://elipousson.github.io/maplayer/reference/layer_markers.md)),
  "sf_text"
  ([`ggplot2::geom_sf_text()`](https://ggplot2.tidyverse.org/reference/ggsf.html)),
  and "sf_label"
  ([`ggplot2::geom_sf_label()`](https://ggplot2.tidyverse.org/reference/ggsf.html)).
  See details for a full list.

- location:

  sf object. If multiple areas are provided, they are unioned into a
  single sf object using
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)

- dist:

  buffer distance in units. Optional.

- diag_ratio:

  ratio of diagonal distance of area's bounding box used as buffer
  distance. e.g. if the diagonal distance is 3000 meters and the
  "diag_ratio = 0.1" a 300 meter will be used. Ignored when `dist` is
  provided.

- unit:

  unit to adjust location by dist or diag_ratio; defaults to "meter"

- asp:

  Aspect ratio of width to height as a numeric value (e.g. 0.33) or
  character (e.g. "1:3"). If numeric,
  [`get_asp()`](https://elipousson.github.io/sfext/reference/get_asp.html)
  returns the same value without modification.

- pkg, package:

  Name of the package to search for data.

- fileext, filetype:

  File extension or type to use if passing parameters to
  [`sfext::read_sf_download()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
  or
  [`sfext::read_sf_pkg()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
  (required for extdata and cached data).

- fn:

  Function to apply to data after filtering by location but before
  returning from function.

- layer_fn:

  ggplot2 geom or custom function using lambda syntax. Use for passing
  custom mapping functions to layer_location_data beyond the supported
  geom options.

- crop:

  If `TRUE`, x is cropped to y using
  [`sf::st_crop()`](https://r-spatial.github.io/sf/reference/st_crop.html).

- trim:

  If `TRUE`, x is trimmed to y with
  [`st_trim()`](https://elipousson.github.io/sfext/reference/st_erase.html).

- from_crs:

  Coordinate reference system used to match the location CRS to the
  source data.

- crs:

  Coordinate reference system to return.

- label_col:

  Column name or id for a column with the text or labels to pass to any
  text geom.

- smooth_params:

  Optional. Logical or a list of parameters passed to
  [`smoothr::smooth()`](https://strimas.com/smoothr/reference/smooth.html).
  If `TRUE`, apply
  [`smoothr::smooth()`](https://strimas.com/smoothr/reference/smooth.html)
  to location data using default parameters. smooth_params is ignored if
  data is `NULL` (inheriting data from ggplot).

- shadow_params:

  Optional. Logical or a list of parameters passed to
  [`ggfx::with_shadow()`](https://ggfx.data-imaginist.com/reference/with_shadow.html).
  If `TRUE`, apply
  [`ggfx::with_shadow()`](https://ggfx.data-imaginist.com/reference/with_shadow.html)
  to the layer using default parameters. shadow_params is ignored if
  layer_fn is provided.

- basemap:

  Either a logical vector or ggplot object.

  If **logical** and `TRUE`, add x to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
  If `FALSE`, return x as is.

  If a **ggplot**, add x to basemap object.

  If a **ggproto** object (or list that contains a **ggproto** object),
  add x and basemap object to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- ...:

  Additional parameters passed to selected geom or layer_fn

## Using the geom parameter

This function provides a convenient option for filtering data by
location while calling a different layer function from ggplot2,
maplayer, or a different package.

Options for the geom parameter from ggplot2:

- "sf"
  ([ggplot2::geom_sf](https://ggplot2.tidyverse.org/reference/ggsf.html) -
  default)

- "sf_text" or "text
  ([ggplot2::geom_sf_text](https://ggplot2.tidyverse.org/reference/ggsf.html))

- "sf_label" or "label
  ([ggplot2::geom_sf_label](https://ggplot2.tidyverse.org/reference/ggsf.html))

Options for the geom parameter included in this package include:

- "location"
  ([layer_location](https://elipousson.github.io/maplayer/reference/layer_location.md))

- "context" or "location_context"
  ([layer_location_context](https://elipousson.github.io/maplayer/reference/layer_location_context.md))

- "icon"
  ([layer_icon](https://elipousson.github.io/maplayer/reference/layer_icon.md)),

- "mapbox"
  ([layer_mapbox](https://elipousson.github.io/maplayer/reference/layer_mapbox.md))

- "markers"
  ([layer_markers](https://elipousson.github.io/maplayer/reference/layer_markers.md))

- "numbers" or "numbered"
  ([layer_numbers](https://elipousson.github.io/maplayer/reference/layer_markers.md))

- "mark" or "marked"
  ([layer_marked](https://elipousson.github.io/maplayer/reference/layer_marked.md))

Options for the geom parameter from other suggested packages include:

- "textsf"
  ([geomtextpath::geom_textsf](https://allancameron.github.io/geomtextpath/reference/geom_textsf.html))

- "labelsf"
  ([geomtextpath::geom_labelsf](https://allancameron.github.io/geomtextpath/reference/geom_textsf.html))

- "text_repel"
  ([ggrepel::geom_text_repel](https://ggrepel.slowkow.com/reference/geom_text_repel.html))

- "label_repel"
  ([ggrepel::geom_label_repel](https://ggrepel.slowkow.com/reference/geom_text_repel.html))

- "sf_pattern" or "pattern"
  ([ggpattern::geom_sf_pattern](https://trevorldavis.com/R/ggpattern/reference/geom-docs.html))

`stat = "sf_coordinates"` is automatically added to the parameters for
both ggrepel functions. `label = .data[[name_col]]` is automatically
added to all provided geoms where label is a required parameter.

## Using the layer_fn parameter

layer_fn can be a purrr-style lamba function (converted with
[`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html))
or a function.

## See also

Other layer:
[`layer_frame()`](https://elipousson.github.io/maplayer/reference/layer_frame.md),
[`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md),
[`layer_scaled()`](https://elipousson.github.io/maplayer/reference/layer_scaled.md)
