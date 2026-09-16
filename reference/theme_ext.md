# Modify the text, margins, or legend for a ggplot theme

Helper functions for modifying a ggplot theme. The "replace" and
"update" options for the method parameter are not currently working;
keeping method = NULL or method = "set" is recommended.

## Usage

``` r
theme_text(
  font_family = NULL,
  color = "black",
  geom_text = TRUE,
  hjust = NULL,
  vjust = NULL,
  method = NULL,
  ...
)

theme_margin(
  margin = "standard",
  paper = NULL,
  orientation = NULL,
  dist = NULL,
  unit = "in",
  block_width = NULL,
  header = 0,
  footer = 0,
  fill = NA,
  color = NA,
  linewidth = 0,
  size = NULL,
  method = NULL,
  ...
)

theme_legend(
  position = NULL,
  justification = NULL,
  margin = 8,
  unit = "pt",
  inset = TRUE,
  nudge_inset = 0.05,
  bgcolor = "white",
  title = list(face = "bold", align = 0),
  method = NULL,
  ...
)
```

## Arguments

- font_family:

  Font family, Default: 'Helvetica' If `NULL`, font_family is pulled
  from current set theme which is helpful for resetting all text
  families to the theme.

- color:

  Color for text elements (passed to
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  by theme_text), `plot.background` (passed to
  [`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
  by theme_margin). Default: `NA`.

- geom_text:

  If `TRUE`, update text family for
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html),
  [`ggplot2::geom_sf_text()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
  [`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html),
  and
  [`ggplot2::geom_sf_label()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  to match `font_family` and color. If `FALSE`, make no changes to the
  theme. Default: `TRUE`.

- hjust, vjust:

  Horizontal and vertical justification.

- method:

  Method with name of the ggplot2 geom function to use for modifying
  theme ("set", "update", or "replace"); defaults to `NULL`.

- ...:

  Additional parameters passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- margin:

  Margin distance, a margin style supported by `get_margin()` or a
  margin object; defaults to 10.

- paper:

  Paper, Default: 'letter'.

- orientation:

  Page orientation, Default: `NULL`. Supported options are "portrait",
  "landscape", or "square".

- dist:

  Margin distance (single value used to all sides), Default: `NULL`

- unit:

  Legend margin units; defaults to 'pt'.

- block_width:

  Plot or map width in units. If `paper` and `block_width` are provided,
  margins are half the distance between the two evenly distributed. This
  sets the margin distance for height as well as width so does not work
  well with header and footers and should be improved in the future.

- header, footer:

  Header and footer height in units; defaults to 0. Please note: headers
  and footers are not currently supported for "px" units.

- fill:

  Fill for `plot.background` theme element passed to
  [`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
  Default: `NA`.

- linewidth, size:

  Passed to linewidth of
  [`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
  to define the plot.background theme element. linewidth is ignored if
  size is provided.

- position:

  Legend position (“left”,“top”, “right”, “bottom”) or a two-element
  numeric vector to set position using Normalized Parent Coordinates
  ("npc"); defaults NULL

- justification:

  If `NULL`, justification is set to "center"; defaults to `NULL`. Use
  justification to set legend position if `inset = FALSE`. Supports
  "topleft", "bottomleft", "topright", or "bottomright" values.

- inset:

  If `TRUE` and position is "topleft", "bottomleft", "topright", or
  "bottomright", place the legend in an inset position; defaults to
  `TRUE`.

- nudge_inset:

  Position adjustment in "npc" units to use for inset legends. Defaults
  to 0.05.

- bgcolor:

  Fill color for legend background; defaults to 'white'.

- title:

  Attributes to use for legend.title text (e.g. face and align).

## See also

- [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

- [`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html)

- [`ggplot2::theme_get()`](https://ggplot2.tidyverse.org/reference/get_theme.html)

- [`ggplot2::update_geom_defaults()`](https://ggplot2.tidyverse.org/reference/update_defaults.html)
