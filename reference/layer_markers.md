# Create a ggplot2 layer with map markers or numbered markers

If make is `TRUE`, groupname_col, group_meta, crs, and fn is all passed
on to make_markers.

## Usage

``` r
layer_markers(
  data,
  mapping = NULL,
  geom = "sf",
  make = FALSE,
  groupname_col = NULL,
  group_meta = NULL,
  crs = getOption("maplayer.crs", default = 3857),
  number = FALSE,
  num_by_group = FALSE,
  num_style = NULL,
  num_start = 1,
  suffix = NULL,
  sort = "dist_xmin_ymax",
  desc = FALSE,
  fn = NULL,
  ...
)

layer_numbers(
  data,
  mapping = NULL,
  geom = "label",
  make = FALSE,
  groupname_col = NULL,
  style = "roundrect",
  size = 5,
  sort = "dist_xmin_ymax",
  num_by_group = FALSE,
  num_style = NULL,
  num_start = 1,
  suffix = NULL,
  desc = FALSE,
  fn = NULL,
  crs = getOption("maplayer.crs", default = 3857),
  linewidth = 0,
  label.padding = ggplot2::unit(size/10, "lines"),
  label.r = label.padding * 1.5,
  hjust = 0.5,
  vjust = 0.5,
  ...
)

make_markers(
  data,
  groupname_col = NULL,
  group_meta = NULL,
  join = sf::st_intersects,
  geo = FALSE,
  coords = c("lon", "lat"),
  address = "address",
  point = TRUE,
  crs = NULL,
  fn = NULL,
  ...
)
```

## Arguments

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

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- geom:

  A character string indicating which ggplot2 geom to use, Default:
  'sf'. Options include "sf"
  ([`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)),
  "icon"
  ([`layer_icon()`](https://elipousson.github.io/maplayer/reference/layer_icon.md)),
  "markers" (`layer_markers()`), "sf_text"
  ([`ggplot2::geom_sf_text()`](https://ggplot2.tidyverse.org/reference/ggsf.html)),
  and "sf_label"
  ([`ggplot2::geom_sf_label()`](https://ggplot2.tidyverse.org/reference/ggsf.html)).
  See details for a full list.

- make:

  If `TRUE`, pass data to make_markers.

- groupname_col:

  Group column name, used to join group metadata if group_meta is a
  non-spatial data frame; Default: `NULL`

- group_meta:

  Group metadata as a data frame or sf object that intersect with
  markers (using join function); Default: `NULL`

- crs:

  Coordinate reference system for markers, Default: `NULL`

- number:

  If `TRUE`, number markers using `layer_markers()` (not currently
  supported)

- num_by_group:

  If `TRUE`, numbers are added by group based on groupname_col.

- num_style:

  Style of enumeration, either "arabic", "alph", "Alph", "roman",
  "Roman".

- num_start:

  Starting number; defaults to 1.

- suffix:

  Character to appended to "number" column. (e.g. "." for "1." or ":"
  for "1:"). Can also be a character vector with the same length as the
  number column.

- sort:

  Sort column name, Default: "dist_xmin_ymax".

- desc:

  If `TRUE`, sort descending; default `FALSE`.

- fn:

  Function to apply to data before results; gives warning if data is
  grouped; Default: `NULL`

- ...:

  Additional parameters passed to
  [`get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.html)
  when using `make = TRUE` to pass data to make_markers

- style:

  Marker style; defaults to `NULL` for `layer_markers()` (supports
  "facet"); defaults to "roundrect" for `layer_markers()` when numbered
  = `TRUE` (default is only supported option at present).

- size:

  Marker size, Default: 5

- linewidth:

  Width of the label border, Default: 0.0

- label.padding:

  Amount of padding around label. Defaults to 0.25 lines.

- label.r:

  Radius of rounded corners. Defaults to 0.15 lines.

- hjust, vjust:

  Horizontal and vertical justification.

- join:

  Spatial relation function to combine data with group_meta, passed to
  [`sf::st_join()`](https://r-spatial.github.io/sf/reference/st_join.html).
  Defaults to
  [`sf::st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html).

- geo:

  If `FALSE`, pass data to
  [`getdata::get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.html)
  with `geo = TRUE` parameter.

- coords:

  Coordinate columns for input data.frame or output sf object (if
  geometry is 'centroid' or 'point') Default: c("lon", "lat").

- address:

  Address column name passed to `tidygeocoder::geocode()` or
  tidygeocoder::geo

- point:

  If `TRUE` and data does not have POINT or MULTIPOINT geometry, convert
  to POINT data using
  [`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

## Value

ggplot2 layers

## Examples

``` r
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
nc <- sf::st_transform(nc, 3857)

basemap <-
  ggplot() +
  layer_location_data(
    data = nc,
    fill = NA
  )

basemap +
  layer_markers(
    data = nc[1:10],
    mapping = aes(size = AREA),
    make = TRUE
  )


large_nc <-
  getdata::get_location_data(
    data = nc,
    fn = ~ dplyr::filter(.x, AREA > 0.2)
  )

large_nc$number <- 1
large_nc$dist <- 2

basemap +
  layer_numbers(
    data = large_nc,
    mapping = aes(fill = NAME),
    sort = "dist_xmax_ymin",
    num_style = "Roman",
    geom = "label",
    size = 3
  ) +
  guides(fill = "none")
```
