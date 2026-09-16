# Use mapboxapi to make a Mapbox static map layer

Use mapboxapi to make a Mapbox static map layer

## Usage

``` r
layer_mapbox(
  data = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter",
  asp = NULL,
  style_url = "mapbox://styles/mapbox/satellite-streets-v11",
  style_id = NULL,
  username = NULL,
  basemap = FALSE,
  scale = 0.75,
  scaling_factor = "1x",
  attribution = TRUE,
  logo = TRUE,
  access_token = NULL,
  neatline = TRUE,
  color = "black",
  bgcolor = "white",
  linewidth = 0.5,
  linetype = "solid",
  expand = TRUE,
  hide_grid = TRUE,
  label_axes = "----",
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

- ...:

  Additional parameter passed to
  [mapboxapi::layer_static_mapbox](https://walker-data.com/mapboxapi/reference/layer_static_mapbox.html)

## See also

[`mapboxapi::layer_static_mapbox()`](https://walker-data.com/mapboxapi/reference/layer_static_mapbox.html)
