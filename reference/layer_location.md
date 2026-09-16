# Layer a location border into a ggplot2 map

Helper function to make a ggplot2 layer from data returned by
`get_location`

## Usage

``` r
layer_location(
  mapping = ggplot2::aes(),
  data = NULL,
  type = NULL,
  name = NULL,
  id = NULL,
  location = NULL,
  name_col = "name",
  id_col = "id",
  index = NULL,
  label = NULL,
  label_geom = NULL,
  label_col = name_col,
  union = FALSE,
  crs = getOption("maplayer.crs", default = 3857),
  color = "gray40",
  linewidth = 0.5,
  linetype = "dashed",
  fill = NA,
  alpha = 1,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  mask = FALSE,
  neatline = FALSE,
  smooth_params = NULL,
  shadow_params = NULL,
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

  Data for location to show.

- type:

  Type of location to return. Type can be an sf object, e.g. a data
  frame with multiple neighborhoods or a character string that can be
  passed to
  [`get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.html).
  If index is provided, character can also be a character string to
  match the name of a list.

- name:

  Location name to return.

- id:

  Location id to return. id is coerced to character or numeric to match
  the class of the id_col for type.

- location:

  An address, bounding box (`bbox`), or simple feature (`sf`) object
  passed to
  [`sf::st_filter()`](https://r-spatial.github.io/sf/reference/st_join.html).
  Any valid address or addresses are geocoded with
  `tidygeocoder::geo()`, converted to a simple feature object, and then
  used as a spatial filter. `bbox` objects are converted using
  [`sfext::sf_bbox_to_sf()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.html).
  Multiple addresses are supported.

- name_col:

  Column name in type with name values, Default: 'name' Required if name
  provided.

- id_col:

  Column name in type with id values, Default: 'id'. Required if id is
  provided.

- index:

  Optional list used to match type to data, Default: `NULL`

- label:

  label type (e.g. "text", "label")

- label_geom:

  Optional character string or function with geom to use for labelling
  location layer. Passed to geom parameter of
  [`layer_labelled()`](https://elipousson.github.io/maplayer/reference/layer_labelled.md)

- label_col:

  Column name or id for a column with the text or labels to pass to any
  text geom.

- union:

  If `TRUE`, the location geometry is unioned with
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  and the names are combined into a single value. Default: `FALSE`.

- crs:

  Coordinate reference system to return; defaults to `NULL` which
  returns data using the same coordinate reference system as the
  provided type of location.

- color:

  Color for location; defaults to "black".

- linewidth:

  Line width for location; defaults to 0.5.

- linetype:

  Line type for location; defaults to "dashed".

- fill:

  Fill for location; defaults to "NA".

- alpha:

  mask alpha/transparency; defaults to 0.5

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

- ...:

  Additional parameters passed to get_location if data is `NULL`.

## Value

list of ggplot2 geoms

## See also

[`ggplot2::CoordSf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
