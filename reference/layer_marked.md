# Use ggforce to create an annotation layer using simple feature data

Use
[`ggforce::geom_mark_rect()`](https://ggforce.data-imaginist.com/reference/geom_mark_rect.html),
[`ggforce::geom_mark_circle()`](https://ggforce.data-imaginist.com/reference/geom_mark_circle.html),
[`ggforce::geom_mark_ellipse()`](https://ggforce.data-imaginist.com/reference/geom_mark_ellipse.html),
or
[`ggforce::geom_mark_hull()`](https://ggforce.data-imaginist.com/reference/geom_mark_hull.html)
to annotate simple feature objects. This function modifies the geometry
and sets the stat to "st_coordinates" to make it easy to use these
annotation geoms with simple feature objects.

## Usage

``` r
layer_marked(
  data,
  fn = NULL,
  mapping = NULL,
  label_col = NULL,
  desc_col = NULL,
  geom = NULL,
  center = FALSE,
  font_family = NULL,
  font_face = c("bold", "plain"),
  font_color = NULL,
  expand = ggplot2::unit(5, "mm"),
  radius = expand,
  stat = "sf_coordinates",
  drop_shadow = FALSE,
  shadow_params = list(x_offset = 5, y_offset = 5, sigma = 0.5, ...),
  ...
)
```

## Arguments

- data:

  A `sf`, `sfc`, or `bbox` object that can be converted with
  [sfext::as_sf](https://elipousson.github.io/sfext/reference/as_sf.html)

- fn:

  Function to apply to data before passing to geom, typically a
  predicate or filter to define area for annotation. A filter can also
  be passed to any of the {ggforce} functions using the filter
  aesthetic. Default: NULL

- mapping:

  Aesthetic mapping to pass to geom, Default: `NULL`

- label_col:

  Label column name

- desc_col:

  Column name to use for description. Defaults to `NULL`.

- geom:

  geom to use for layer (options include "rect", "circle", "ellipse", or
  "hull"), Default: `NULL`

- center:

  If `FALSE`, use
  [sfext::st_cast_ext](https://elipousson.github.io/sfext/reference/st_cast_ext.html)
  MULTIPOLYGON and POLYGON data to POINT; If `TRUE`, use
  [sfext::st_center](https://elipousson.github.io/sfext/reference/st_misc.html)
  use centroid as the feature geometry. Defaults to FALSE.

- font_family, font_face, font_color:

  Parameters passed to label.family, label.fontface, and label.colour.
  If `NULL`, values are set to match
  [ggplot2::geom_label](https://ggplot2.tidyverse.org/reference/geom_text.html)
  defaults. Defaults to `NULL`.

- expand:

  A numeric or unit vector of length one, specifying the expansion
  amount. Negative values will result in contraction instead. If the
  value is given as a numeric it will be understood as a proportion of
  the plot area width.

- radius:

  As `expand` but specifying the corner radius.

- stat:

  stat to pass to ggforce function; defaults to "sf_coordinates"

- drop_shadow:

  If `TRUE`, use
  [`ggfx::with_shadow()`](https://ggfx.data-imaginist.com/reference/with_shadow.html)
  to add a drop shadow to the label layer with shadow_params. Defaults
  to `FALSE`.

- shadow_params:

  Parameters passed to
  [`ggfx::with_shadow()`](https://ggfx.data-imaginist.com/reference/with_shadow.html)
  if `drop_shadow = TRUE`. Defaults to
  `list(x_offset = 5, y_offset = 5, sigma = 0.5)`.

- ...:

  Additional parameters passed to
  [`ggforce::geom_mark_rect()`](https://ggforce.data-imaginist.com/reference/geom_mark_rect.html),
  [`ggforce::geom_mark_circle()`](https://ggforce.data-imaginist.com/reference/geom_mark_circle.html),
  [`ggforce::geom_mark_ellipse()`](https://ggforce.data-imaginist.com/reference/geom_mark_ellipse.html),
  or
  [`ggforce::geom_mark_hull()`](https://ggforce.data-imaginist.com/reference/geom_mark_hull.html)
  or on to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)

## Geometry conversion for MULTIPOLYGON or POLYGON data

If cast is `FALSE` and the data geometry type is MULTIPOLYGON or
POLYGON, the annotation uses a centroid for the annotation geometry. If
cast is `TRUE`, the data is converted to POINT geometry using
[`sfext::st_cast_ext()`](https://elipousson.github.io/sfext/reference/st_cast_ext.html)
and the modified geometry passed on to selected geom.

## Setting label and description

Labels and descriptions can be included in the aesthetic mapping for the
layer consistent with the standard documented approaches for all four
functions.

Labels and descriptions also can be set in two non-standard ways:

- Setting label_col and/or or desc_col to character strings with the
  column names for labels and/or descriptions

- Setting label_col and/or desc_col with the value of the
  label/description

If the first approach is used, label_col and desc_col can also can be
created or modified by a function provided to the fn parameter.
Otherwise, the columns must be present in the data to work. If the
second approach is used, the length and order of vector provided to
label_col and/or desc_col must match that length and order of the data
(after the fn is applied).

## See also

- [`ggforce::geom_mark_rect()`](https://ggforce.data-imaginist.com/reference/geom_mark_rect.html)

- [`ggforce::geom_mark_circle()`](https://ggforce.data-imaginist.com/reference/geom_mark_circle.html)

- [`ggforce::geom_mark_ellipse()`](https://ggforce.data-imaginist.com/reference/geom_mark_ellipse.html)

- [`ggforce::geom_mark_hull()`](https://ggforce.data-imaginist.com/reference/geom_mark_hull.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {
  # Mark the 12th Council District on a Baltimore neighborhood basemap
  ggplot::ggplot() +
    layer_location_data(
      data = "neighborhoods",
      package = "mapbaltimore"
    ) +
    layer_marked(
      data = getdata::get_location(
        type = "council district",
        name = "District 12",
        package = "mapbaltimore"
      ),
      geom = "hull",
      color = "red",
      size = 1.5
    )
}
} # }
```
