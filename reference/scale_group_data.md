# Create discrete fill and color scales for grouped data

Designed for use with layer_group_data. group_data_pal generates
palettes that are passed to
[ggplot2::scale_fill_manual](https://ggplot2.tidyverse.org/reference/scale_manual.html)
and
[ggplot2::scale_color_manual](https://ggplot2.tidyverse.org/reference/scale_manual.html).

## Usage

``` r
scale_group_data(
  ...,
  data,
  col = NULL,
  palette = NULL,
  n = NULL,
  direction = 1,
  na.value = "grey50",
  drop = FALSE,
  limits = NULL,
  aesthetics = "fill"
)

group_data_pal(
  data,
  palette = NULL,
  col = NULL,
  n = NULL,
  direction = 1,
  pkg = NULL
)

get_group_data_pal_scale(data, col = NULL, palette = NULL, ...)
```

## Arguments

- ...:

  Arguments passed on to
  [`discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

  `limits`

  :   One of:

      - `NULL` to use the default scale values

      - A character vector that defines possible values of the scale and
        their order

      - A function that accepts the existing (automatic) values and
        returns new ones. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `drop`

  :   Should unused factor levels be omitted from the scale? The
      default, `TRUE`, uses the levels that appear in the data; `FALSE`
      includes the levels in the factor. Please note that to display
      every level in a legend, the layer should use
      `show.legend = TRUE`.

  `na.translate`

  :   Unlike continuous scales, discrete scales can easily show missing
      values, and do so by default. If you want to remove missing values
      from a discrete scale, specify `na.translate = FALSE`.

  `name`

  :   The name of the scale. Used as the axis or legend title. If
      [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html),
      the default, the name of the scale is taken from the first mapping
      used for that aesthetic. If `NULL`, the legend title will be
      omitted.

  `minor_breaks`

  :   One of:

      - `NULL` for no minor breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default breaks (none for discrete, one minor break
        between each major break for continuous)

      - A numeric vector of positions

      - A function that given the limits returns a vector of minor
        breaks. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. When the function has two arguments, it will
        be given the limits and major break positions.

  `labels`

  :   One of the options below. Please note that when `labels` is a
      vector, it is highly recommended to also set the `breaks` argument
      as a vector to protect against unintended mismatches.

      - `NULL` for no labels

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default labels computed by the transformation object

      - A character vector giving labels (must be same length as
        `breaks`)

      - An expression vector (must be the same length as breaks). See
        ?plotmath for details.

      - A function that takes the breaks as input and returns labels as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `guide`

  :   A function used to create a guide or its name. See
      [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html)
      for more information.

  `call`

  :   The `call` used to construct the scale for reporting messages.

  `super`

  :   The super class to use for the constructed scale

- data:

  Data to use when generating scale or palette.

- col:

  Grouping column found in data to use in generating scale or palette;
  defaults to `NULL`.

- palette:

  Name of palette as a string. Must be on the form
  packagename::palettename.

- n:

  Number of colors desired. If omitted, returns complete palette.

- direction:

  Either `1` or `-1`. If `-1` the palette will be reversed.

- na.value:

  The aesthetic value to use for missing (`NA`) values

- drop:

  Should unused factor levels be omitted from the scale? The default,
  `TRUE`, uses the levels that appear in the data; `FALSE` includes the
  levels in the factor. Please note that to display every level in a
  legend, the layer should use `show.legend = TRUE`.

- limits:

  One of:

  - `NULL` to use the default scale values

  - A character vector that defines possible values of the scale and
    their order

  - A function that accepts the existing (automatic) values and returns
    new ones. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- aesthetics:

  Character string or vector of character strings listing the name(s) of
  the aesthetic(s) that this scale works with. This can be useful, for
  example, to apply colour settings to the `colour` and `fill`
  aesthetics at the same time, via `aesthetics = c("colour", "fill")`.

- pkg:

  Package name to use as prefix for palette name when selecting a
  palette with
  [`paletteer::paletteer_d()`](https://emilhvitfeldt.github.io/paletteer/reference/paletteer_d.html)

## See also

[`scales::viridis_pal()`](https://scales.r-lib.org/reference/pal_viridis.html)
[`paletteer::paletteer_d()`](https://emilhvitfeldt.github.io/paletteer/reference/paletteer_d.html)
