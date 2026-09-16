# Use patchwork to create a map with an inset context map or figpatch to stamp an inset image

`layer_inset()` is useful when you want to add an inset to a plot.

## Usage

``` r
layer_inset(
  map = NULL,
  inset = NULL,
  position = "bottomright",
  scale = 1,
  nudge_x = 0,
  nudge_y = 0,
  align_to = "full",
  ...
)

make_inset_map(
  map = NULL,
  inset = NULL,
  location = NULL,
  context = NULL,
  position = "bottomright",
  scale = 1,
  nudge_x = 0,
  nudge_y = 0,
  align_to = "full",
  ...
)

stamp_inset_img(
  path,
  plot = NULL,
  img_margin = ggplot2::margin(0, 0, 0, 0),
  position = "bottomright",
  scale = 1,
  nudge_x = 0,
  nudge_y = 0,
  align_to = "full",
  ...
)
```

## Arguments

- inset:

  plot or map created with
  [`ggplot2()`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
  passed to p argument of
  [`patchwork::inset_element()`](https://patchwork.data-imaginist.com/reference/inset_element.html).
  If both location and context are provided to `make_inset_map()`, inset
  is optional and any provided value is replaced with a new layer
  created by
  [`layer_location_context()`](https://elipousson.github.io/maplayer/reference/layer_location_context.md).

- position:

  inset map position, Default: 'bottomright'. position, nudge_x, and
  nudge_y are used to set the left, bottom, top, and right parameters
  for
  [`patchwork::inset_element()`](https://patchwork.data-imaginist.com/reference/inset_element.html).

- scale:

  scale of inset map, defaults to 1.

- nudge_x, nudge_y:

  nudge x and/or y position of inset map, Default: 0.

- align_to:

  Specifies what `left`, `bottom`, etc should be relative to. Either
  `'panel'` (default), `'plot'`, or `'full'`.

- ...:

  Arguments passed on to
  [`patchwork::inset_element`](https://patchwork.data-imaginist.com/reference/inset_element.html)

  `p`

  :   A grob, ggplot, patchwork, formula, raster, nativeRaster, or gt
      object to add as an inset

  `left,bottom,right,top`

  :   numerics or units giving the location of the outer bounds. If
      given as numerics they will be converted to `npc` units.

  `on_top`

  :   Logical. Should the inset be placed on top of the other plot or
      below (but above the background)?

  `clip`

  :   Logical. Should clipping be performed on the inset?

  `ignore_tag`

  :   Logical. Should autotagging ignore the inset?

- location:

  A location passed to
  [`layer_location_context()`](https://elipousson.github.io/maplayer/reference/layer_location_context.md).
  This can be a sf object, a ggplot layer, or a formula or function. If
  it is a formula or function, it is applied to the context data is
  passed to the location function and the results used as the data for
  the location layer.

- context:

  A `sf` object for context area or a ggplot layer representing the
  context.

- path:

  image path passed to
  [`figpatch::fig()`](https://BradyAJohnston.github.io/figpatch/reference/fig.html)
  for `stamp_inset_img()`

- plot, map:

  plot or map created with
  [`ggplot2()`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)

- img_margin:

  margin around image for `stamp_inset_img()` created by
  [`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html).
  Defaults to no margin.

## Value

ggplot2 map with inset map added using patchwork

## Details

`make_inset_map()` is useful for creating an inset map just using the
location with fewer options for customization. In that case, the ...
parameters are passed to
[`layer_location_context()`](https://elipousson.github.io/maplayer/reference/layer_location_context.md)
instead of
[`patchwork::inset_element()`](https://patchwork.data-imaginist.com/reference/inset_element.html)

`stamp_inset_img()` is useful for applying a logo to a map. The ...
parameters are passed to
[`figpatch::fig()`](https://BradyAJohnston.github.io/figpatch/reference/fig.html)

Note, currently, plots created with `layer_inset()` do not work with
[`map_ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.html)
using the `single_file = TRUE` parameter.
