# Create a ggplot2 layer scaled to a paper and orientation for a location

Uses
[layer_neatline](https://elipousson.github.io/maplayer/reference/layer_neatline.md),
[sfext::standard_scales](https://elipousson.github.io/sfext/reference/standard_scales.html),
and
[sfext::convert_dist_scale](https://elipousson.github.io/sfext/reference/convert_dist_scale.html).

## Usage

``` r
layer_scaled(
  data = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  crs = getOption("maplayer.crs", default = 3857),
  scale = NULL,
  paper = NULL,
  orientation = NULL,
  clip = FALSE
)
```

## Arguments

- data:

  A `sf`, `sfc`, or `bbox` class object.

- dist:

  distance to convert. If paper is provided, dist is optional and paper
  width and height are used as dist.

- diag_ratio:

  ratio of diagonal distance of area's bounding box used as buffer
  distance. e.g. if the diagonal distance is 3000 meters and the
  "diag_ratio = 0.1" a 300 meter will be used. Ignored when `dist` is
  provided.

- unit:

  Units for buffer. Supported options include "meter", "foot",
  "kilometer", and "mile", "nautical mile" Common abbreviations (e.g.
  "km" instead of "kilometer") are also supported. Distance in units is
  converted to units matching GDAL units for x; defaults to "meter"

- asp:

  Aspect ratio of width to height as a numeric value (e.g. 0.33) or
  character (e.g. "1:3"). If numeric,
  [`get_asp()`](https://elipousson.github.io/sfext/reference/get_asp.html)
  returns the same value without modification.

- crs:

  Coordinate reference system of bounding box to return; defaults to
  `NULL` which maintains the crs of the input object.

- scale:

  Scale name from `standard_scales[["scale"]]`.

- paper:

  Paper, Default: 'letter'.

- orientation:

  Orientation "portrait", "landscape", or "square", Default: 'portrait'.

- clip:

  If `TRUE`, create scaled layer even if the data is cut off; defaults
  to `FALSE`.

## See also

Other layer:
[`layer_frame()`](https://elipousson.github.io/maplayer/reference/layer_frame.md),
[`layer_location_data()`](https://elipousson.github.io/maplayer/reference/layer_location_data.md),
[`layer_neatline()`](https://elipousson.github.io/maplayer/reference/layer_neatline.md)
