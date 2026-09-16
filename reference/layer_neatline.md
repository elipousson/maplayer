# Set map limits to a bounding box with a buffer and set aspect ratio

Set limits for a map to the bounding box of a feature using
[`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).
Optionally, adjust the x size by applying a buffer and/or adjust the
aspect ratio of the limiting bounding box to match a set aspect ratio.

## Usage

``` r
layer_neatline(
  data = NULL,
  dist = getOption("maplayer.dist"),
  diag_ratio = getOption("maplayer.diag_ratio"),
  unit = getOption("maplayer.unit", default = "meter"),
  asp = getOption("maplayer.asp"),
  crs = getOption("maplayer.crs"),
  nudge = getOption("maplayer.nudge"),
  color = "black",
  linewidth = 0.5,
  linetype = "solid",
  bgcolor = "white",
  expand = TRUE,
  hide_grid = TRUE,
  label_axes = "----",
  axis.title = NULL,
  axis.text = NULL,
  axis.ticks = NULL,
  axis.ticks.length = ggplot2::unit(x = 0, units = "mm"),
  axis.line = NULL,
  panel.grid = NULL,
  panel.grid.major = NULL,
  panel.grid.minor = NULL,
  panel.border = NULL,
  panel.background = NULL,
  plot.background = NULL,
  plot.margin = NULL,
  default_plot_margin = ggplot2::margin(1, 1, 1, 1),
  xlim = NULL,
  ylim = NULL,
  ...
)

theme_grid(
  hide_grid = TRUE,
  grid = FALSE,
  panel.grid = NULL,
  panel.grid.major = NULL,
  panel.grid.minor = NULL
)

theme_sf_axis(
  label_axes = "----",
  axis.title = NULL,
  axis.text = NULL,
  axis.text.x = NULL,
  axis.text.y = NULL,
  axis.ticks = NULL,
  axis.ticks.length = ggplot2::unit(x = 0, units = "mm"),
  axis.line = NULL,
  ...
)

set_neatline(x = NULL, neatline = TRUE, data = NULL, crs = NULL, ...)
```

## Arguments

- data:

  A `sf`, `sfc`, or `bbox` class object.

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

  The coordinate reference system (CRS) into which all data should be
  projected before plotting. If not specified, will use the CRS defined
  in the first sf layer of the plot.

- nudge:

  Passed as to parameter
  [`st_nudge()`](https://elipousson.github.io/sfext/reference/st_nudge.html)
  when not `NULL`. A numeric vector, a `sf` object, or any other object
  that can be converted to a simple feature collection with `as_sfc()`..

- color:

  Color of panel border, Default: 'black'

- linewidth:

  Line width of panel border, Default: 0.5

- linetype:

  Line type of panel border, Default: 'solid'

- bgcolor:

  Fill color of panel background; defaults to "white". If "none", panel
  background is set to
  [`ggplot2::element_blank()`](https://ggplot2.tidyverse.org/reference/element.html)

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

- axis.title, axis.text, axis.ticks, axis.ticks.length, axis.line:

  Theme elements passed as is if label_axes is anything other than "—-".

- panel.grid, panel.grid.major, panel.grid.minor:

  Passed as is if hide_grid is FALSE.

- panel.border, panel.background, plot.background, plot.margin:

  panel.border is used as is if not NULL or
  [`ggplot2::element_blank()`](https://ggplot2.tidyverse.org/reference/element.html)
  if it is NULL unless color is NA or "none". panel.background and
  plot.background are used as is or
  [`ggplot2::element_blank()`](https://ggplot2.tidyverse.org/reference/element.html)
  if bg color is NA or "none". plot.margin is set to
  `ggplot2::margin(1, 1, 1, 1)` if `NULL` or
  `ggplot2::margin(0, 0, 0, 0)` if expand is `FALSE`.

- default_plot_margin:

  Defaults to `ggplot2::margin(1, 1, 1, 1)`. Ignored if `expand = FALSE`

- xlim, ylim:

  Limits for the x and y axes. These limits are specified in the units
  of the default CRS. By default, this means projected coordinates
  (`default_crs = NULL`). How limit specifications translate into the
  exact region shown on the plot can be confusing when non-linear or
  rotated coordinate systems are used as the default crs. First,
  different methods can be preferable under different conditions. See
  parameter `lims_method` for details. Second, specifying limits along
  only one direction can affect the automatically generated limits along
  the other direction. Therefore, it is best to always specify limits
  for both x and y. Third, specifying limits via position scales or
  [`xlim()`](https://ggplot2.tidyverse.org/reference/lims.html)/[`ylim()`](https://ggplot2.tidyverse.org/reference/lims.html)
  is strongly discouraged, as it can result in data points being dropped
  from the plot even though they would be visible in the final plot
  region.

- ...:

  Additional parameters passed to
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

- grid:

  If `grid` is `TRUE` and `hide_grid` is `FALSE`, grid will be included
  in the theme. Otherwise, suppress the grid.

- axis.text.x, axis.text.y:

  Passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

- x:

  For `set_neatline()`, a `ggplot` class object, `ggproto` class object
  or list of `ggproto` objects to combine with neatline layer.

- neatline:

  A logical object, `CoordSf` object, or a list containing a `CoordSf`
  object (typically from `layer_neatline()`) added to layer by
  `set_neatline()`.

  - If logical and `TRUE`, add a neatline layer using data, crs and any
    additional parameters passed to ... If logical and `FALSE`, return x
    as is.

  - If object from `layer_neatline()`, add it as is.

## Value

List of
[ggplot2::coord_sf](https://ggplot2.tidyverse.org/reference/ggsf.html)
and [ggplot2::theme](https://ggplot2.tidyverse.org/reference/theme.html)
calls.

## See also

[`ggplot2::CoordSf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)

Other layer:
[`layer_frame()`](https://elipousson.github.io/maplayer/reference/layer_frame.md),
[`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md),
[`layer_scaled()`](https://elipousson.github.io/maplayer/reference/layer_scaled.md)

## Examples

``` r
library(ggplot2)

nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

ggplot() +
  layer_location_data(data = nc) +
  layer_neatline(
    data = nc[1, ],
    asp = 1,
    color = "red",
    linewidth = 1,
    linetype = "dashed"
  )


ggplot() +
  layer_location_data(data = nc) +
  layer_neatline(data = nc[1, ], dist = 20, unit = "mi", color = "none")
```
