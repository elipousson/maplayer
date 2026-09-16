# Create a layer with an arrow or segment from and to specified locations

A wrapper for
[ggplot2::geom_segment](https://ggplot2.tidyverse.org/reference/geom_segment.html),
[`ggplot2::geom_curve()`](https://ggplot2.tidyverse.org/reference/geom_segment.html),
[`ggarchery::geom_arrowsegment()`](https://rdrr.io/pkg/ggarchery/man/geom_arrowsegment.html),
[`ggforce::geom_diagonal0()`](https://ggforce.data-imaginist.com/reference/geom_diagonal.html),
[`ggforce::geom_link()`](https://ggforce.data-imaginist.com/reference/geom_link.html)
that makes it easier to specify the start and end of the segment using
any object supported by the
[`sfext::as_xy()`](https://elipousson.github.io/sfext/reference/as_xy.html)
function.

## Usage

``` r
layer_arrow(
  mapping = NULL,
  data = NULL,
  crs = NULL,
  from,
  to,
  geom = "segment",
  ...
)
```

## Arguments

- mapping:

  aesthetic mapping overwritten with x, y, xend, and yend values based
  on provided from and to parameters.

- data:

  Required if from or to are character vectors to
  [`sfext::as_xy()`](https://elipousson.github.io/sfext/reference/as_xy.html)

- crs:

  A character or numeric reference to a coordinate reference system
  supported by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  or another `sf`, `sfc`, or `bbox` object that is used to provide crs.

- from, to:

  Required. Passed to x parameter of
  [`sfext::as_xy()`](https://elipousson.github.io/sfext/reference/as_xy.html)
  (using `nm = c("xend", "yend")`) for the to parameter.

- geom:

  Character string for geom to use c("segment", "curve", "arrowsegment",
  "diagonal0", "link") or a geom function.

- ...:

  Additional parameters passed to function specified by geom paramter.

## See also

[`ggplot2::reexports()`](https://ggplot2.tidyverse.org/reference/reexports.html),
[`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html),
[`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)

## Examples

``` r
library(ggplot2)

nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

nc_map <- ggplot(data = nc) +
  geom_sf()

nc_map +
  layer_arrow(
    data = nc,
    from = c("xmin", "ymin"),
    to = c("xmid", "ymax"),
  )


nc_map +
  layer_arrow(
    data = nc,
    from = c("xmin", "ymin"),
    to = c("xmid", "ymax"),
    geom = "curve",
    curvature = 0.25
  )


nc_map +
  layer_arrow(
    data = nc,
    from = c("xmax", "ymin"),
    to = c("xmid", "ymax"),
    geom = "arrowsegment"
  )
```
