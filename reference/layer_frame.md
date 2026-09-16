# Create a frame layer around a simple feature object

Create a circle or square that can be used as a frame around a simple
feature object using fixed aesthetics for fill, color, size, and
linetype. This function is helpful for the background of an inset map
intended for use with
[`layer_inset()`](https://elipousson.github.io/maplayer/reference/layer_inset.md).

## Usage

``` r
layer_frame(
  data = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter",
  asp = NULL,
  style = "circle",
  scale = 1,
  rotate = 0,
  inscribed = FALSE,
  color = "black",
  linewidth = 0.5,
  linetype = "solid",
  fill = "white",
  neatline = TRUE,
  expand = TRUE,
  basemap = FALSE,
  union = TRUE,
  by_feature = FALSE,
  ...
)

make_frame(
  x,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter",
  asp = NULL,
  style = "circle",
  scale = 1,
  rotate = 0,
  inscribed = FALSE,
  dTolerance = 0,
  union = TRUE,
  by_feature = FALSE
)
```

## Arguments

- data, x:

  A `sf`, `sfc`, or `bbox` object to create the frame around.

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

- style:

  Style of framing shape to add, "circle", "square", "rect", "buffer",
  or "none". If style is "buffer", the asp parameter is ignored. If
  style is "none", the dist, diag_ratio, and asp parameters are ignored
  and the input data is used as the frame.

- scale:

  numeric; scale factor, Default: 1

- rotate:

  numeric; degrees to rotate (-360 to 360), Default: 0

- inscribed:

  If `TRUE`, the returned geometry is inscribed within x, if `FALSE`
  (default), the geometry is circumscribed.

- fill, color, linewidth, linetype:

  Fixed aesthetics for frame, passed to
  [layer_location_data](https://elipousson.github.io/maplayer/reference/layer_location_data.md).

- neatline:

  If `TRUE`, return a list of layers that includes a
  [layer_neatline](https://elipousson.github.io/maplayer/reference/layer_neatline.md)

- expand:

  If `TRUE`, the default, adds a small expansion factor to the limits to
  ensure that data and axes don't overlap. If `FALSE`, limits are taken
  exactly from the data or `xlim`/`ylim`. Giving a logical vector will
  separately control the expansion for the four directions (top, left,
  bottom and right). The `expand` argument will be recycled to length 4
  if necessary. Alternatively, can be a named logical vector to control
  a single direction, e.g. `expand = c(bottom = FALSE)`.

- basemap:

  Either a logical vector or ggplot object.

  If **logical** and `TRUE`, add x to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
  If `FALSE`, return x as is.

  If a **ggplot**, add x to basemap object.

  If a **ggproto** object (or list that contains a **ggproto** object),
  add x and basemap object to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- union:

  If `TRUE`, pass data to
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  before buffering and creating frame; defaults to `TRUE`.

- by_feature:

  If `TRUE`, create a frame around each feature. If `FALSE` (default),
  union the provided features before creating a frame.

- ...:

  Arguments passed on to
  [`layer_location_data`](https://elipousson.github.io/maplayer/reference/layer_location_data.md)

  `layer_fn`

  :   ggplot2 geom or custom function using lambda syntax. Use for
      passing custom mapping functions to layer_location_data beyond the
      supported geom options.

  `label_col`

  :   Column name or id for a column with the text or labels to pass to
      any text geom.

  `smooth_params`

  :   Optional. Logical or a list of parameters passed to
      [`smoothr::smooth()`](https://strimas.com/smoothr/reference/smooth.html).
      If `TRUE`, apply
      [`smoothr::smooth()`](https://strimas.com/smoothr/reference/smooth.html)
      to location data using default parameters. smooth_params is
      ignored if data is `NULL` (inheriting data from ggplot).

  `shadow_params`

  :   Optional. Logical or a list of parameters passed to
      [`ggfx::with_shadow()`](https://ggfx.data-imaginist.com/reference/with_shadow.html).
      If `TRUE`, apply
      [`ggfx::with_shadow()`](https://ggfx.data-imaginist.com/reference/with_shadow.html)
      to the layer using default parameters. shadow_params is ignored if
      layer_fn is provided.

  `location`

  :   sf object. If multiple areas are provided, they are unioned into a
      single sf object using
      [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)

  `fileext,filetype`

  :   File extension or type to use if passing parameters to
      [`sfext::read_sf_download()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
      or
      [`sfext::read_sf_pkg()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
      (required for extdata and cached data).

  `fn`

  :   Function to apply to data after filtering by location but before
      returning from function.

  `crop`

  :   If `TRUE`, x is cropped to y using
      [`sf::st_crop()`](https://r-spatial.github.io/sf/reference/st_crop.html).

  `trim`

  :   If `TRUE`, x is trimmed to y with
      [`st_trim()`](https://elipousson.github.io/sfext/reference/st_erase.html).

  `crs`

  :   Coordinate reference system to return.

  `mapping`

  :   Set of aesthetic mappings created by
      [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
      specified and `inherit.aes = TRUE` (the default), it is combined
      with the default mapping at the top level of the plot. You must
      supply `mapping` if there is no plot mapping.

- dTolerance:

  numeric; tolerance parameter, specified for all or for each feature
  geometry. If you run `st_simplify`, the input data is specified with
  long-lat coordinates and
  [`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.html)
  returns `TRUE`, then the value of `dTolerance` must be specified in
  meters.

## Details

The `make_frame()` helper function calls
[`sfext::st_circle()`](https://elipousson.github.io/sfext/reference/st_misc.html)
(if `style = "circle"`),
[`sfext::st_square()`](https://elipousson.github.io/sfext/reference/st_square.html)
(if `style = "square"`),
[`sfext::st_bbox_ext()`](https://elipousson.github.io/sfext/reference/st_bbox_ext.html)
(if `style = "rect"`), or
[`sfext::st_buffer_ext()`](https://elipousson.github.io/sfext/reference/st_buffer_ext.html)
(if `style = "none"`).

If neatline is `TRUE`, `layer_frame()` returns a list of two geoms, the
second a
[`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md)
layer created using the frame object as the data and the parameters
`bgcolor = "none"` and `color = "none"`. asp is set to 1 if style is
"circle" or "square" or the provided asp value otherwise.

Additional parameters passed through ... can include additional fixed
aesthetics (e.g. alpha). If using the fn parameter, the function is
applied to the frame simple feature object created by `make_frame()`
(not to the original input data).

## See also

Other layer:
[`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md),
[`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md),
[`layer_scaled()`](https://elipousson.github.io/maplayer/reference/layer_scaled.md)

## Examples

``` r
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

raleigh_msa <-
  getdata::get_location(
    type = nc,
    name_col = "NAME",
    name = c("Franklin", "Johnston", "Wake"),
    crs = 3857
  )

ggplot() +
  layer_frame(
    data = raleigh_msa,
    frame = "circle",
    fill = "lightyellow",
    inscribed = FALSE
  ) +
  layer_location_data(
    data = raleigh_msa,
    mapping = aes(fill = NAME),
    alpha = 0.5
  ) +
  ggplot2::guides(
    fill = "none"
  )
#> Warning: Ignoring unknown parameters: `frame`
```
