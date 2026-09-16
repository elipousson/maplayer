# Make a map using layer_mapbox

Wraps
[`make_layer_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md)
and passes layer created with
[`layer_mapbox()`](https://elipousson.github.io/maplayer/reference/layer_mapbox.md)
to basemap and the neatline parameters to
[`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md)
(using the same data as the Mapbox background layer). The neatline
parameters are only used if neatline is `NULL`.

## Usage

``` r
make_mapbox_map(
  data = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter",
  asp = NULL,
  style_url = "mapbox://styles/mapbox/satellite-streets-v11",
  style_id = NULL,
  username = NULL,
  basemap = TRUE,
  scale = 0.75,
  scaling_factor = "1x",
  attribution = TRUE,
  logo = TRUE,
  access_token = NULL,
  neatline = NULL,
  color = "black",
  bgcolor = "white",
  linewidth = 0.5,
  linetype = "solid",
  expand = TRUE,
  hide_grid = TRUE,
  label_axes = "----",
  width = NULL,
  height = NULL,
  units = NULL,
  orientation = NULL,
  location = NULL,
  ...
)
```

## Arguments

- data:

  A `sf`, `sfc`, or `bbox` object.

- dist:

  buffer distance in units. Optional.

- diag_ratio:

  ratio of diagonal distance of area's bounding box used as buffer
  distance. e.g. if the diagonal distance is 3000 meters and the
  "diag_ratio = 0.1" a 300 meter will be used. Ignored when `dist` is
  provided.

- unit:

  Units for buffer. Supported options include "meter", "foot",
  "kilometer", and "mile", "nautical mile" Common abbreviations (e.g.
  "km" instead of "kilometer") are also supported. Distance in units is
  converted to units matching GDAL units for x; defaults to "meter"

- asp:

  Aspect ratio of width to height as a numeric value (e.g. 0.33) or
  character (e.g. "1:3"). If numeric,
  [`get_asp()`](https://elipousson.github.io/sfext/reference/get_asp.html)
  returns the same value without modification.

- style_url:

  Map style url used to fill style_id and username parameters, Default:
  "mapbox://styles/mapbox/satellite-streets-v11"

- style_id:

  A style ID (required if style_url is `NULL`).

- username:

  A Mapbox username (required if `style_url = NULL`).

- basemap:

  If FALSE, create a standalone layer; if `TRUE`, the layer is preceded
  by
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  to allow use as a basemap, Default: `TRUE`

- scale:

  ratio to scale the output image; `scale = 1` will return the largest
  possible image. defaults to 0.5

- scaling_factor:

  The scaling factor of the tiles; either `"1x"` (the default) or `"2x"`

- attribution:

  Controls whether there is attribution on the image. Defaults to
  `TRUE`. If `FALSE`, the watermarked attribution is removed from the
  image. You still have a legal responsibility to attribute maps that
  use OpenStreetMap data, which includes most maps from Mapbox. If you
  specify `attribution = FALSE`, you are legally required to include
  proper attribution elsewhere on the webpage or document.

- logo:

  Controls whether there is a Mapbox logo on the image. Defaults to
  `TRUE`.

- access_token:

  A Mapbox access token; which can be set with
  [mb_access_token](https://walker-data.com/mapboxapi/reference/mb_access_token.html).

- neatline:

  If `TRUE`, add a neatline matching the provided data, Default: `TRUE`

- color:

  Color of panel border, Default: 'black'

- bgcolor:

  Fill color of panel background; defaults to "white". If "none", panel
  background is set to
  [`ggplot2::element_blank()`](https://ggplot2.tidyverse.org/reference/element.html)

- linewidth:

  Line width of panel border, Default: 0.5

- linetype:

  Line type of panel border, Default: 'solid'

- expand:

  If `TRUE`, the default, adds a small expansion factor to the limits to
  ensure that data and axes don't overlap. If `FALSE`, limits are taken
  exactly from the data or `xlim`/`ylim`. Giving a logical vector will
  separately control the expansion for the four directions (top, left,
  bottom and right). The `expand` argument will be recycled to length 4
  if necessary. Alternatively, can be a named logical vector to control
  a single direction, e.g. `expand = c(bottom = FALSE)`.

- hide_grid:

  If `TRUE`, hide grid lines. Default: `TRUE`

- label_axes:

  A description of which axes to label passed to
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html);
  defaults to '—-' which hides all axes labels.

- width, height:

  Page width and height. Both are required, if asp is `NULL`. Default to
  `NULL`.

- units:

  Units for width and height. Required unless units is included in dims
  or `require_units = FALSE`. Passed to
  [`as_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.html)
  to validate.

- orientation:

  Page orientation, Default: `NULL`. Supported options are "portrait",
  "landscape", or "square". If width and height suggest a portrait
  orientation when orientation = "landscape", the dimensions are
  reversed so the page dimensions match the provided orientation.

- location:

  If `location` is provided and `data` is `NULL`, `location` is used in
  place of `data`.

- ...:

  Arguments passed on to
  [`make_layer_map`](https://elipousson.github.io/maplayer/reference/make_location_map.md)

  `image_path`

  :   path to location of images for
      [`make_image_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md)

  `image_geom`

  :   For
      [`make_image_map()`](https://elipousson.github.io/maplayer/reference/make_location_map.md),
      geom to use with layer_markers to mark the location of images
      (based on EXIF metadata).

  `labs_ext_params`

  :   Optional parameters passed to
      [`labs_ext()`](https://elipousson.github.io/maplayer/reference/labs_ext.md).

  `ggsave_params`

  :   List of parameters passed to
      [`papersize::ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.html).

  `layer`

  :   A ggplot2 layer or a list of ggproto objects. If layer is
      provided, all parameters passed to
      [`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md)
      (including data, location, dist, diag_ratio, unit, asp, crs, and
      geom) will be ignored. In this case, the function simply stacks
      the bg_layer, layer, and fg_layer objects then applies the basemap
      and neatline (using the
      [`set_basemap()`](https://elipousson.github.io/maplayer/reference/set_basemap.md)
      and
      [`set_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md)
      helper functions.)

  `bg_layer,fg_layer,addon`

  :   A ggplot2 layer or a list of ggproto objects (e.g. scales, labels,
      etc.) to add to the background or foreground of the primary map
      layer defined by `"geom"` and other parameters. If the geom
      creates an opaque layer or layer is an opaque layer (e.g. a layer
      produced by
      [`layer_mapbox()`](https://elipousson.github.io/maplayer/reference/layer_mapbox.md))
      that covers the full map extent, the bg_layer will not be visible.

  `save`

  :   If `TRUE`, save file with
      [ggsave_ext](https://elipousson.github.io/papersize/reference/ggsave_ext.html)
      using `ggsave_params`. Defaults to `FALSE`.

  `env`

  :   Environment for evaluation of
      [`labs_ext()`](https://elipousson.github.io/maplayer/reference/labs_ext.md)
      if labs_ext_params is supplied.

  `crs`

  :   Coordinate reference system of bounding box to return; defaults to
      `NULL` which maintains the crs of the input object.

  `geom`

  :   A character string indicating which ggplot2 geom to use, Default:
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

  `paper`

  :   Paper matching name from `paper_sizes` (e.g. "letter"). Not case
      sensitive.

  `image`

  :   Image name passed to name parameter of
      [`get_social_size()`](https://elipousson.github.io/papersize/reference/get_social_size.html).

  `platform`

  :   Social media platform, "Instagram", "Facebook", or "Twitter",
      Default: `NULL`

  `format`

  :   Image format, "post", "story", or "cover", Default: `NULL`

  `number`

  :   If `TRUE`, number markers using
      [`layer_markers()`](https://elipousson.github.io/maplayer/reference/layer_markers.md)
      (not currently supported)

  `num_by_group`

  :   If `TRUE`, numbers are added by group based on groupname_col.

  `groupname_col`

  :   Group column name, used to join group metadata if group_meta is a
      non-spatial data frame; Default: `NULL`

  `group_meta`

  :   Group metadata as a data frame or sf object that intersect with
      markers (using join function); Default: `NULL`

  `sort`

  :   Sort column name, Default: "dist_xmin_ymax".

  `desc`

  :   If `TRUE`, sort descending; default `FALSE`.

  `num_style`

  :   Style of enumeration, either "arabic", "alph", "Alph", "roman",
      "Roman".

  `num_start`

  :   Starting number; defaults to 1.

  `suffix`

  :   Character to appended to "number" column. (e.g. "." for "1." or
      ":" for "1:"). Can also be a character vector with the same length
      as the number column.

  `call`

  :   The execution environment of a currently running function, e.g.
      `caller_env()`. The function will be mentioned in error messages
      as the source of the error. See the `call` argument of
      [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
      information.
