# Create a layer showing a location and related context

Create a ggplot2 layer for a location in a provided context. This
function is useful for making inset locator maps in combination with the
[make_inset_map](https://elipousson.github.io/maplayer/reference/layer_inset.md)
function.

## Usage

``` r
layer_location_context(
  data = NULL,
  location = NULL,
  fill = "gray70",
  color = "black",
  context = NULL,
  context_params = list(fill = "white", color = "black", alpha = 1, ...),
  crs = getOption("maplayer.crs", default = 3857),
  mid_layer = NULL,
  neatline = TRUE,
  basemap = FALSE,
  ...
)
```

## Arguments

- data, location:

  A data and location can be a either `sf` object or a ggplot layer.
  Either data or location is required and, if data and location are both
  provided, location is ignored. If data is `NULL` and location is a
  formula or function, the context data is passed to the location
  function and the results used as the data for the location layer.

- fill, color:

  Fill and color fixed aesthetics for location data.

- context:

  A `sf` object for context area or a ggplot layer representing the
  context.

- context_params:

  A list with parameters for context layer; defaults to
  `list(fill = "white", color = "black", alpha = 1, ...)`.

- crs:

  Coordinate reference system to return.

- mid_layer:

  A ggplot2 layer to insert between the context layer and the location
  layer. Optional.

- neatline:

  A logical object, `CoordSf` object, or a list containing a `CoordSf`
  object (typically from
  [`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md))
  added to layer.

  - If logical and `TRUE`, add a neatline layer using data from the
    context layer with `color = NA` and `bgcolor = "none"`.

  - If object from
    [`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md),
    add it as is.

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

  Additional parameters passed to
  [`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md)
  if data or location is an `sf` object.
