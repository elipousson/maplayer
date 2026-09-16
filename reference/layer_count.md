# Layer for counting occurrences of data in spatial relation a location or other sf object

Wraps
[`sfext::count_sf_ext()`](https://elipousson.github.io/sfext/reference/count_sf_ext.html).
Specification of parameters for this function may be too complex and may
be changed in the future.

## Usage

``` r
layer_count(
  data,
  location = NULL,
  y = NULL,
  join = sf::st_intersects,
  largest = TRUE,
  replace_na = FALSE,
  lims = NULL,
  .id = "id",
  grid_params = list(alpha = 1, color = NA),
  show_data = FALSE,
  data_params = list(mapping = aes(), alpha = 0.75, size = 1),
  show_label = FALSE,
  label_params = NULL,
  scale_fn = ggplot2::scale_fill_continuous,
  scale_params = list(type = "viridis", breaks = scales::breaks_pretty(n = 4)),
  ...
)
```

## Arguments

- data:

  Data to count in relationship to y

- location:

  Passed to x parameter of
  [`sfext::count_sf_ext()`](https://elipousson.github.io/sfext/reference/count_sf_ext.html).

- y:

  If `NULL` (default), y defaults to an `sf` object created by
  [`st_make_grid_ext()`](https://elipousson.github.io/sfext/reference/st_make_grid_ext.html)
  using x or data (if x is `NULL`) as the x parameter for
  [`st_make_grid_ext()`](https://elipousson.github.io/sfext/reference/st_make_grid_ext.html).
  If not `NULL`, y must be an `sf` object that has a column with the
  same name as .id (defaults to "id").

- join:

  geometry predicate function with the same profile as
  [st_intersects](https://r-spatial.github.io/sf/reference/geos_binary_pred.html);
  see details

- largest:

  logical; if `TRUE`, return `x` features augmented with the fields of
  `y` that have the largest overlap with each of the features of `x`;
  see https://github.com/r-spatial/sf/issues/578

- replace_na:

  If `TRUE`, replace NA values from count with 0.

- lims:

  Optional numeric vector with minimum or both minimum and maximum count
  values. If provided, any values below the minimum are set to that
  minimum and any values above the maximum as set to the maximum. If
  only one value is provided, it is assumed to be a minimum limit.

- .id:

  A name to use for the cell id column. Defaults to "id".

- grid_params:

  Passed to
  [`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md)
  to style foreground grid with fill based on count.

- show_data:

  If `TRUE`, add background layer with data to stack returned by
  function. If `TRUE` and grid_params includes a fixed aesthetic for
  alpha, divide alpha in half to ensure background data is visible below
  the filled grid.

- data_params:

  Passed to
  [`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md)
  to style background layer based on data.

- show_label:

  If `TRUE`, add layer with labels to stack returned by function.

- label_params:

  Passed to
  [`layer_labelled()`](https://elipousson.github.io/maplayer/reference/layer_labelled.md)
  for foreground labels with fill based on count.

- scale_fn, scale_params:

  Scale function and parameters. Defaults to
  [`ggplot2::scale_fill_continuous()`](https://ggplot2.tidyverse.org/reference/scale_colour_continuous.html).

- ...:

  Arguments passed on to
  [`sfext::count_sf_ext`](https://elipousson.github.io/sfext/reference/count_sf_ext.html)

  `wt`

  :   \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
      Frequency weights. Can be `NULL` or a variable:

      - If `NULL` (the default), counts the number of rows in each
        group.

      - If a variable, computes `sum(wt)` for each group.

  `sort`

  :   If `TRUE`, will show the largest groups at the top.

  `keep_na`

  :   If `TRUE`, filter NA values from count. Ignored if replace_na is
      `TRUE`.

  `geometry`

  :   If `TRUE` (default) return a `sf` object. If `FALSE`, return a
      data frame.

  `name`

  :   The name of the new column in the output.

      If omitted, it will default to `n`. If there's already a column
      called `n`, it will use `nn`. If there's a column called `n` and
      `nn`, it'll use `nnn`, and so on, adding `n`s until it gets a new
      name.

## Examples

``` r

nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
data <- sf::st_sample(nc, 75)

ggplot() +
  layer_count(data = data, location = nc)


ggplot() +
  layer_count(data = data, y = nc, .id = "FIPS")

```
