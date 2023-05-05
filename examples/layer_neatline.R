library(ggplot2)

nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

ggplot() +
  layer_location_data(data = nc) +
  layer_neatline(data = nc[1, ], asp = 1, color = "red", linewidth = 1, linetype = "dashed")

ggplot() +
  layer_location_data(data = nc) +
  layer_neatline(data = nc[1, ], dist = 20, unit = "mi", color = "none")
