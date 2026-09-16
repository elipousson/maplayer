# Create a mask layer based on a simple feature object

Returns a mask for an area or areas as a simple feature object.
`neatline = TRUE` only works for this layer if data is passed directly;
not inherited.

## Usage

``` r
layer_mask(
  data = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  crs = getOption("maplayer.crs", default = 3857),
  fill = "white",
  color = NA,
  alpha = 0.5,
  mask = NULL,
  neatline = FALSE,
  expand = TRUE,
  ...
)

set_mask(x = NULL, mask = TRUE, data = NULL, crs = NULL, ...)
```

## Arguments

- data:

  `sf`, `sfc`, or `bbox` object. If dist, diag_ratio, and/or asp are
  provided, data is adjusted to set the boundaries of the mask. If data
  is not provided, `mask` is required. If data is `NA`, mask is
  continuous otherwise the data is erased from the mask area.

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

- crs:

  Coordinate reference system of bounding box to return; defaults to
  `NULL` which maintains the crs of the input object.

- fill:

  mask fill color; defaults to "white"

- color:

  mask edge color; defaults to `NA`

- alpha:

  mask alpha/transparency; defaults to 0.5

- mask:

  A `sf`, `sfc`, or `bbox` object to define the mask area. `diag_ratio`,
  `dist`, and `asp` parameters are ignored if a `mask` is provided.
  defaults to `NULL`

- neatline:

  A logical object, `CoordSf` object, or a list containing a `CoordSf`
  object (typically from
  [`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md))
  added to layer by
  [`set_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md).

  - If logical and `TRUE`, add a neatline layer using data, crs and any
    additional parameters passed to ... If logical and `FALSE`, return x
    as is.

  - If object from
    [`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md),
    add it as is.

- expand:

  If `TRUE`, the default, adds a small expansion factor to the limits to
  ensure that data and axes don't overlap. If `FALSE`, limits are taken
  exactly from the data or `xlim`/`ylim`. Giving a logical vector will
  separately control the expansion for the four directions (top, left,
  bottom and right). The `expand` argument will be recycled to length 4
  if necessary. Alternatively, can be a named logical vector to control
  a single direction, e.g. `expand = c(bottom = FALSE)`.

- ...:

  Additional parameters to pass to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)

- x:

  A `sf`, `sfc`, `bbox`, `sfg`, `Raster`, `Spatial`, `Extent`,
  `numeric`, or `character` object (a place name passed to
  `osmdata::getbb()`). See
  [`as_bbox()`](https://elipousson.github.io/sfext/reference/as_sf.html)
  for more details.

## Value

[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
function.
