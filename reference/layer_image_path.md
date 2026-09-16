# Use ggpath to create a layer with images at locations

Use the
[`ggpath::geom_from_path()`](https://mrcaseb.github.io/ggpath/reference/geom_from_path.html)
function with
[`sfext::read_sf_exif()`](https://elipousson.github.io/sfext/reference/read_sf_exif.html)
and `stat = "sf_coordinates"` to create a layer showing images at
locations.

## Usage

``` r
layer_image_path(
  data = NULL,
  path = NULL,
  path_col = "path",
  width = 0.1,
  crs = getOption("maplayer.crs", 3857),
  segment_params = NULL,
  neatline = FALSE,
  basemap = FALSE,
  ...
)
```

## Arguments

- data:

  A `sf` object with a column containing file paths that has the same
  name as the path_col argument. Optional if "path" is provided for
  [`sfext::read_sf_exif()`](https://elipousson.github.io/sfext/reference/read_sf_exif.html).
  Required if path is `NULL`. If path is provided, data is ignored.

- path:

  A path to folder or file.

- path_col:

  Column name with file paths from data. Defaults to "path" (path name
  used by data returned from
  [`sfext::read_sf_exif()`](https://elipousson.github.io/sfext/reference/read_sf_exif.html))

- width:

  Width of the image in npc (Normalised Parent Coordinates) passed to
  [`ggpath::geom_from_path()`](https://mrcaseb.github.io/ggpath/reference/geom_from_path.html);
  defaults to 0.1.

- crs:

  The coordinate reference system (CRS) into which all data should be
  projected before plotting. If not specified, will use the CRS defined
  in the first sf layer of the plot.

- segment_params:

  Not implemented: parameters to define segments connecting images to
  the location.

- neatline:

  A logical object, `CoordSf` object, or a list containing a `CoordSf`
  object (typically from
  [`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md))
  added to layer by
  [`set_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md).

  - If logical and `TRUE`, add a neatline layer using data, crs and any
    additional parameters passed to ... If logical and `FALSE`, return x
    as is.

  - If object from
    [`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md),
    add it as is.

- basemap:

  Either a logical vector or ggplot object.

  If **logical** and `TRUE`, add x to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
  If `FALSE`, return x as is.

  If a **ggplot**, add x to basemap object.

  If a **ggproto** object (or list that contains a **ggproto** object),
  add x and basemap object to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- ...:

  Additional parameters to pass to
  [`exiftoolr::exif_read()`](https://joshobrien.github.io/exiftoolr/reference/exif_read.html)
