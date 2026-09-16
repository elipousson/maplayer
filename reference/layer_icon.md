# Use ggsvg to add a layer with icons at feature locations

Use the
[`ggsvg::geom_point_svg()`](https://rdrr.io/pkg/ggsvg/man/geom_point_svg.html)
function to plot icons using the centroids from the input simple feature
object to set the icon location.

## Usage

``` r
layer_icon(
  data = NULL,
  iconname_col = "icon",
  icon = NULL,
  px = NULL,
  source = NULL,
  svg = NULL,
  crs = getOption("maplayer.crs", default = 3857),
  ...
)

geom_sf_icon(
  data = NULL,
  iconname_col = "icon",
  icon = NULL,
  px = NULL,
  source = NULL,
  svg = NULL,
  crs = getOption("maplayer.crs", default = 3857),
  ...
)
```

## Arguments

- data:

  A `sf` object. Any objects with polygon geometry are converted to
  points using
  [`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

- iconname_col:

  The column name in the input data to use as the icon name. If the name
  matches multiple icons, the first match from `map_icons` is used. You
  may provide a px or source value to select a different match if needed
  but, in that case, all icons must use the same px or source value.
  Note that the icon column should not be mapped with
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

- icon:

  Icon name. Default `NULL`. If `icon` is provided, `iconname_col` is
  ignored. See `map_icons$name` for supported options.

- px:

  Icon size in pixels. See `map_icons$px` for supported options.
  Optional but may be necessary to differentiate icons with duplicate
  names.

- source:

  Icon source. See `map_icons$repo` for supported options. Optional but
  may be required to differentiate icons with duplicate names.

- svg:

  Optional. Custom file path or URL with SVG to pass to `svg` parameter
  for
  [`ggsvg::geom_point_svg()`](https://rdrr.io/pkg/ggsvg/man/geom_point_svg.html).
  If `icon` is provided, `svg` is ignored.

- crs:

  Coordinate reference system; defaults to `NULL`.

- ...:

  Arguments passed on to
  [`ggsvg::geom_point_svg`](https://rdrr.io/pkg/ggsvg/man/geom_point_svg.html)

  `defaults`

  :   Advanced option. A named list of default values for new
      aesthetics. In general this is not necessary when using `css()`
      aesthetics, as a default value will be determined based upon the
      CSS property e.g. `stroke` property will have a default value of
      "black"

      Set \`options(GGSVG_DEBUG = TRUE)\` for some verbose debugging
      which will cause ggsvg to output (to the console) the final SVG
      for each and every element in the plot.

## See also

[`ggsvg::geom_point_svg()`](https://rdrr.io/pkg/ggsvg/man/geom_point_svg.html)
[map_icons](https://elipousson.github.io/maplayer/reference/map_icons.md)

## Examples

``` r
nc <- getdata::get_location(
  type = system.file("shape/nc.shp", package = "sf"),
  crs = 3857
)

basemap <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  layer_location_data(data = nc)

# icon can be set by name matching a name from map_icons
basemap +
  layer_icon(data = nc, icon = "point-start", size = 8)


# layer_icon can also use a column from the sf object
nc$icon <- rep(c("1", "2", "3", "4"), nrow(nc) / 4)

basemap +
  layer_icon(data = nc, iconname_col = "icon", size = 6)
```
