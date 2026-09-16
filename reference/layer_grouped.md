# Make group layers

Can be used to make multiple layers or multiple maps based on a grouping
variable.

## Usage

``` r
layer_grouped(
  data,
  mapping = NULL,
  groupname_col = "group",
  label_col = "name",
  geom = "sf",
  basemap = FALSE,
  palette = NULL,
  aesthetics = "fill",
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

- groupname_col:

  Group column name. Defaults to "group".

- label_col:

  Column name or id for a column with the text or labels to pass to any
  text geom.

- geom:

  A character string indicating which ggplot2 geom to use, Default:
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

- basemap:

  Either a logical vector or ggplot object.

  If **logical** and `TRUE`, add x to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
  If `FALSE`, return x as is.

  If a **ggplot**, add x to basemap object.

  If a **ggproto** object (or list that contains a **ggproto** object),
  add x and basemap object to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- palette:

  Name of palette as a string. Must be on the form
  packagename::palettename.

- aesthetics:

  Aesthetic to map to groupname_col. Defaults to "fill"; also supports
  "color" or c("fill", "color").

- ...:

  Additional parameters passed to
  [`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md)

## Details

Scales are applied a palette and aesthetic are provided and basemap is
set to `TRUE`.
