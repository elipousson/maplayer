# Label simple feature objects in a location

Label markers, streets, areas, or other simple feature objects using any
of the following geoms: "text", "sf_text", "label", "sf_label",
"textsf", "labelsf", "text_repel", or "label_repel".

## Usage

``` r
layer_labelled(
  data,
  location = NULL,
  geom = "text",
  fn = NULL,
  label_col = NULL,
  mapping = NULL,
  union = FALSE,
  clip = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  drop_shadow = FALSE,
  shadow_params = NULL,
  ...
)
```

## Arguments

- data:

  Data to use for labels (must be an sf object or a data frame that can
  be converted to an sf object using
  [`sfext::as_sf()`](https://elipousson.github.io/sfext/reference/as_sf.html))

- location:

  Location to label (if not specified the data is assumed to conver the
  whole location);

- geom:

  A geom to use "text", "label", "textsf", "labelsf", "text_repel", or
  "label_repel" or a geom function (passed to layer_fn).

- fn:

  Function to apply to data before creating labels; can be used in the
  creation of the label_col.

- label_col:

  Label column name

- mapping:

  Aesthetic mapping, Default: `NULL`

- union:

  If TRUE, group by label_col and union geometry, Default: `FALSE`

- clip:

  Character string describing the part of the area to clip or remove.
  Options include c("top", "right", "bottom", "left", "topright",
  "bottomright", "bottomleft", "topleft"). If NULL, the area is not
  clipped and a full edge can be returned.

- dist:

  Numeric. Distance to use for the edge. Default NULL meters. Use
  negative values for an inside edge or positive numbers for an outside
  edge.

- diag_ratio:

  Alternate way to define edge distance.

- unit:

  Units for buffer. Supported options include "meter", "foot",
  "kilometer", and "mile", "nautical mile" Common abbreviations (e.g.
  "km" instead of "kilometer") are also supported. Distance in units is
  converted to units matching GDAL units for x; defaults to "meter"

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

  Arguments passed on to
  [`layer_location_data`](https://elipousson.github.io/maplayer/reference/layer_location_data.md)

  `smooth_params`

  :   Optional. Logical or a list of parameters passed to
      [`smoothr::smooth()`](https://strimas.com/smoothr/reference/smooth.html).
      If `TRUE`, apply
      [`smoothr::smooth()`](https://strimas.com/smoothr/reference/smooth.html)
      to location data using default parameters. smooth_params is
      ignored if data is `NULL` (inheriting data from ggplot).

  `asp`

  :   Aspect ratio of width to height as a numeric value (e.g. 0.33) or
      character (e.g. "1:3"). If numeric,
      [`get_asp()`](https://elipousson.github.io/sfext/reference/get_asp.html)
      returns the same value without modification.

  `pkg,package`

  :   Name of the package to search for data.

  `fileext,filetype`

  :   File extension or type to use if passing parameters to
      [`sfext::read_sf_download()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
      or
      [`sfext::read_sf_pkg()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
      (required for extdata and cached data).

  `crop`

  :   If `TRUE`, x is cropped to y using
      [`sf::st_crop()`](https://r-spatial.github.io/sf/reference/st_crop.html).

  `trim`

  :   If `TRUE`, x is trimmed to y with
      [`st_trim()`](https://elipousson.github.io/sfext/reference/st_erase.html).

  `crs`

  :   Coordinate reference system to return.

  `basemap`

  :   Either a logical vector or ggplot object.

      If **logical** and `TRUE`, add x to
      [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
      If `FALSE`, return x as is.

      If a **ggplot**, add x to basemap object.

      If a **ggproto** object (or list that contains a **ggproto**
      object), add x and basemap object to
      [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Details

Note: Unlike in some getdata package functions, fn is applied to all of
the data; not a subset of the data based on location. For this function,
dist and unit are both used by
[`sfext::st_clip()`](https://elipousson.github.io/sfext/reference/st_clip.html)
(not by
[`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md))

The function also overrides the label aesthetics to hide the colored
letters that would otherwise appear when using a theme legend.

## See also

[`sfext::st_clip()`](https://elipousson.github.io/sfext/reference/st_clip.html),[`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md)
