# Convert a set of ggplot2 maps to an atlas

`make_atlas()` is a wrapper for
[`papersize::page_layout()`](https://elipousson.github.io/papersize/reference/page_layout.html)
and
[`papersize::map_ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.html)
with the intent for taking a list of maps into a set of patchwork plots
and optionally save plots to file. The function is similar to
[`papersize::make_contact_sheets()`](https://elipousson.github.io/papersize/reference/make_contact_sheets.html).

## Usage

``` r
make_atlas(
  plots,
  dims = NULL,
  ncol = NULL,
  nrow = NULL,
  page = "letter",
  orientation = "portrait",
  save = FALSE,
  filename = NULL,
  ...
)
```

## Arguments

- plots:

  A list of ggplot2 maps to assemble into a set of sheet maps in an
  atlas format.

- dims:

  Optional. Plot dimensions. Ignored if ncol and nrow are supplied.
  Otherwise, if `NULL` (default), dims are inferred based on the
  dimensions of the first plot in plots.

- ncol, nrow:

  The dimensions of the grid to create. If both are `NULL`, dims will be
  used or dims will be determined based on the plot dimensions.

- page:

  Used by
  [`get_page_dims()`](https://elipousson.github.io/papersize/reference/get_page_size.html),
  page is either a character vector passed to the name parameter of
  [`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.html),
  a data.frame with column names matching the cols parameter, or a
  length 2 numeric vector with the page width and height.

- orientation:

  Page orientation, Default: `NULL`. Supported options are "portrait",
  "landscape", or "square".

- save:

  If `TRUE`, save atlas plots to files using
  [`papersize::map_ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.html)
  Default: FALSE

- filename:

  File name to create on disk.

- ...:

  Arguments passed on to
  [`papersize::map_ggsave_ext`](https://elipousson.github.io/papersize/reference/ggsave_ext.html)

  `name`

  :   Plot name, used to create filename (if filename is `NULL`) using
      [`filenamr::make_filename()`](https://elipousson.github.io/filenamr/reference/make_filename.html)

  `label`

  :   Label to combine with name converted to snake case with
      [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html).
      The label is designed to identify the area or other shared
      characteristics across multiple data files, maps, or plots. label
      is ignored if name is NULL or if name includes a file extension.

  `prefix`

  :   File name prefix. "date" adds a date prefix, "time" adds a
      date/time prefix; defaults to `NULL`.

  `postfix`

  :   File name postfix; defaults to `NULL`.

  `increment`

  :   If `TRUE`, increment digits in string by 1. If numeric, increment
      digits in string by value. If `NULL`, 0, or if no digits are
      present in string, return string as is.

  `device`

  :   Device to use. Can either be a device function (e.g.
      [png](https://rdrr.io/r/grDevices/png.html)), or one of "eps",
      "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg"
      or "wmf" (windows only). If `NULL` (default), the device is
      guessed based on the `filename` extension.

  `fileext`

  :   File type or extension. Optional if filename or path include a
      file extension.

  `filetype`

  :   File type (used if fileext is `NULL`).

  `path`

  :   Path of the directory to save plot to: `path` and `filename` are
      combined to create the fully qualified file name. Defaults to the
      working directory.

  `paper`

  :   Paper matching name from `paper_sizes` (e.g. "letter"). Not case
      sensitive.

  `width,height`

  :   Plot size in units expressed by the `units` argument. If not
      supplied, uses the size of the current graphics device.

  `asp`

  :   Numeric aspect ratio used to determine width or height if only one
      of the two arguments is provided; defaults to `NULL`.

  `units`

  :   One of the following units in which the `width` and `height`
      arguments are expressed: `"in"`, `"cm"`, `"mm"` or `"px"`.

  `scale`

  :   Multiplicative scaling factor.

  `dpi`

  :   Plot resolution. Also accepts a string input: "retina" (320),
      "print" (300), or "screen" (72). Only applies when converting
      pixel units, as is typical for raster output types.

  `bgcolor`

  :   Background color to optionally override `plot.background` theme
      element.

  `exif`

  :   If `TRUE`, the EXIF metadata for the exported file is updated with
      the exifr package; defaults to `FALSE`.

  `title`

  :   Title to add to file metadata with exiftoolr, Default: `NULL`.

  `author`

  :   Author to add to file metadata to the "Author" and
      "XMP-dc:creator" tags. Default: `NULL`.

  `keywords`

  :   Keyword(s) added to file metadata to "IPTC:Keywords" and
      "XMP-dc:Subject" tags. Defaults to `NULL`.

  `args`

  :   Alternate arguments passed to
      [`exiftoolr::exif_call()`](https://joshobrien.github.io/exiftoolr/reference/exif_call.html).
      Other tag parameters are appended to args if they are not `NULL`.

  `overwrite`

  :   If `TRUE` (default), overwrite any existing file with the same
      name or ask to overwrite if `ask = TRUE`. Passed to
      [`filenamr::check_file_overwrite()`](https://elipousson.github.io/filenamr/reference/check_file_overwrite.html).

  `ask`

  :   If `TRUE`, ask before overwriting file with the same name.
      Defaults to `FALSE`. Passed to
      [`filenamr::check_file_overwrite()`](https://elipousson.github.io/filenamr/reference/check_file_overwrite.html).

  `preview`

  :   If `TRUE`, open saved file in default system application. Based on
      ggpreview from tjmisc package.

  `limitsize`

  :   When `TRUE` (the default),
      [`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
      will not save images larger than 50x50 inches, to prevent the
      common error of specifying dimensions in pixels.

  `quiet`

  :   If `TRUE` (default), suppress function messages.

  `image`

  :   Image name passed to name parameter of
      [`get_social_size()`](https://elipousson.github.io/papersize/reference/get_social_size.html).

  `platform`

  :   Social media platform, "Instagram", "Facebook", or "Twitter",
      Default: `NULL`

  `format`

  :   Image format, "post", "story", or "cover", Default: `NULL`

  `single_file,onefile`

  :   If `TRUE`, use
      [`gridExtra::arrangeGrob()`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)
      to create an arrangelist class object that
      [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
      can save as a single multi-page file. Note: this does not work
      with plots modified with patchwork including inset maps created
      with the
      [`maplayer::layer_inset()`](https://elipousson.github.io/maplayer/reference/layer_inset.md)
      function.

## Value

OUTPUT_DESCRIPTION

## Details

DETAILS

## Examples

``` r
nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

plots <- lapply(
  dplyr::nest_by(nc, .by = NAME)[["data"]][1:4],
  function(x) {
    make_location_map(
      basemap = ggplot(),
      layer = layer_location(
        data = x,
        fill = "yellow",
        alpha = 0.5
      ),
      bg_layer = layer_location_data(
        data = nc,
        location = x,
        asp = 8.5 / 5.5,
        crop = FALSE
      ),
      neatline = layer_neatline(data = x, asp = 8.5 / 5.5),
      addon = labs_ext(caption = x$NAME)
    )
  }
)

make_atlas(
  plots = plots,
  page = "letter",
  nrow = 2,
  ncol = 1,
  save = FALSE
)
#> ℹ Creating sheet map plots
#> ✔ Creating sheet map plots [17ms]
#> 
#> $`1`

#> 
#> $`2`

#> 
```
