# data can be a file path or url passed to getdata::get_location_data
path <- system.file("shape/nc.shp", package = "sf")

ggplot2::ggplot() +
  layer_location_data(
    data = path,
    crs = 3857,
    aes(fill = AREA)
  )

data <- getdata::get_location(type = system.file("shape/nc.shp", package = "sf"), crs = 3857)

location <- getdata::get_location(type = nc, name = "Ashe", name_col = "NAME", crs = 3857)

# Using the geom parameter to select a geom
ggplot2::ggplot() +
  layer_location_data(
    data = data,
    location = location,
    geom = "sf",
    crop = FALSE,
    color = "red"
  ) +
  layer_location_data(
    data = data,
    location = location,
    geom = "label",
    label_col = "NAME",
    crop = FALSE,
    color = "red"
  )

# Using the geom_fn to pass a function
ggplot2::ggplot() +
  layer_location_data(
    data = data,
    location = location,
    geom_fn = ggplot2::geom_sf,
    crop = FALSE,
    color = "red"
  )

# Using the geom_fn to pass a lamda-style function
ggplot2::ggplot() +
  layer_location_data(
    data = data,
    location = location,
    geom_fn = ~ ggplot2::geom_sf_text(data = .x, aes(label = NAME), size = 8, color = "red"),
    crop = FALSE
  )
