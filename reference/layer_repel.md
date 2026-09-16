# Use ggrepel to create text annotations based on simple features

Use
[ggrepel::geom_label_repel](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
or
[ggrepel::geom_text_repel](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
with
[`ggplot2::stat_sf_coordinates()`](https://ggplot2.tidyverse.org/reference/stat_sf_coordinates.html)
to create a layer of textual annotations repelled from simple feature
locations.

## Usage

``` r
layer_repel(
  mapping = aes(),
  data = NULL,
  label_col = "name",
  geom = c("text", "label"),
  location_lims = NULL,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  ...
)

geom_sf_label_repel(mapping = aes(), data = NULL, label_col = "name", ...)

geom_sf_text_repel(mapping = aes(), data = NULL, label_col = "name", ...)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes`](https://ggplot2.tidyverse.org/reference/aes.html) or
  [`aes_`](https://ggplot2.tidyverse.org/reference/aes_.html). If
  specified and `inherit.aes = TRUE` (the default), is combined with the
  default mapping at the top level of the plot. You only need to supply
  `mapping` if there isn't a mapping defined for the plot.

- data:

  A data frame. If specified, overrides the default data frame defined
  at the top level of the plot.

- label_col:

  Column name to use for label aesthetic mapping. Optional if label is
  provided to mapping; required otherwise.

- geom:

  Character vector with geom to use, "text" for
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
  or "label" for
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

- location_lims:

  A `sf`, `sfc`, or `bbox` object to use in setting xlim and ylim values
  if no xlim and ylim value area provided. Using this parameter
  constrains labels to the bounding box of `location_lims`.

- xlim, ylim:

  Limits for the x and y axes. Text labels will be constrained to these
  limits. By default, text labels are constrained to the entire plot
  area.

- ...:

  Arguments passed on to
  [`ggrepel::geom_label_repel`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)

  `position`

  :   Position adjustment, either as a string, or the result of a call
      to a position adjustment function.

  `parse`

  :   If TRUE, the labels will be parsed into expressions and displayed
      as described in ?plotmath

  `box.padding`

  :   Amount of padding around bounding box, as unit or number. Defaults
      to 0.25. (Default unit is lines, but other units can be specified
      by passing `unit(x, "units")`).

  `label.padding`

  :   Amount of padding around label, as unit or number. Defaults to
      0.25. (Default unit is lines, but other units can be specified by
      passing `unit(x, "units")`).

  `point.padding`

  :   Amount of padding around labeled point, as unit or number.
      Defaults to 0. (Default unit is lines, but other units can be
      specified by passing `unit(x, "units")`).

  `label.r`

  :   Radius of rounded corners, as unit or number. Defaults to 0.15.
      (Default unit is lines, but other units can be specified by
      passing `unit(x, "units")`).

  `label.size`

  :   Size of label border, in mm.

  `min.segment.length`

  :   Skip drawing segments shorter than this, as unit or number.
      Defaults to 0.5. (Default unit is lines, but other units can be
      specified by passing `unit(x, "units")`).

  `arrow`

  :   specification for arrow heads, as created by
      [`arrow`](https://rdrr.io/r/grid/arrow.html)

  `force`

  :   Force of repulsion between overlapping text labels. Defaults to 1.

  `force_pull`

  :   Force of attraction between a text label and its corresponding
      data point. Defaults to 1.

  `max.time`

  :   Maximum number of seconds to try to resolve overlaps. Defaults to
      0.5.

  `max.iter`

  :   Maximum number of iterations to try to resolve overlaps. Defaults
      to 10000.

  `max.overlaps`

  :   Exclude text labels when they overlap too many other things. For
      each text label, we count how many other text labels or other data
      points it overlaps, and exclude the text label if it has too many
      overlaps. Defaults to 10.

  `nudge_x,nudge_y`

  :   Horizontal and vertical adjustments to nudge the starting position
      of each text label. The units for `nudge_x` and `nudge_y` are the
      same as for the data units on the x-axis and y-axis.

  `xlim,ylim`

  :   Limits for the x and y axes. Text labels will be constrained to
      these limits. By default, text labels are constrained to the
      entire plot area.

  `na.rm`

  :   If `FALSE` (the default), removes missing values with a warning.
      If `TRUE` silently removes missing values.

  `show.legend`

  :   logical. Should this layer be included in the legends? `NA`, the
      default, includes if any aesthetics are mapped. `FALSE` never
      includes, and `TRUE` always includes.

  `direction`

  :   "both", "x", or "y" – direction in which to adjust position of
      labels

  `seed`

  :   Random seed passed to
      [`set.seed`](https://rdrr.io/r/base/Random.html). Defaults to
      `NA`, which means that `set.seed` will not be called.

  `verbose`

  :   If `TRUE`, some diagnostics of the repel algorithm are printed

  `inherit.aes`

  :   If `FALSE`, overrides the default aesthetics, rather than
      combining with them. This is most useful for helper functions that
      define both data and aesthetics and shouldn't inherit behaviour
      from the default plot specification, e.g.
      [`borders`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).
