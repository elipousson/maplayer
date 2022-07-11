# data can be a file path or url passed to getdata::get_location_data
nc_path <- system.file("shape/nc.shp", package = "sf")

ggplot() +
  layer_location_data(
    data = nc_path,
    crs = 3857,
    aes(fill = AREA)
  )

# Typically data will be loaded separately to avoid the need to re-load multiple layers separately
nc <-
  getdata::get_location_data(
    data = nc_path,
    crs = 3857
  )

# geom_fn can be a function with additional parameters passed through the ... parameters
ggplot() +
  layer_location_data(
    data = nc,
    geom_fn = ggplot2::geom_sf_text,
    color = "red",
    aes(label = NAME)
  )

# geom_fn can also be a purr-style lambda function
ggplot() +
  layer_location_data(
    data = nc,
    geom_fn = ~ ggplot2::geom_sf_text(data = .x, color = "red", aes(label = NAME))
  )

# FIXME: This is a non-functioning draft example for the layer_location_data function
layers <-
  map_location_data(
    data = list(
      "streets",
      "neighborhoods",
      "council_districts"
    ),
    location = get_location(
      type = "council_districts",
      id = 1,
      package = "mapbaltimore"),
    diag_ratio = 0.2,
    package = "mapbaltimore",
    from_crs = 2804
  )
