library(ggplot2)

nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

nc_map <- ggplot(data = nc) +
  geom_sf()

nc_map +
  layer_arrow(
    data = nc,
    from = c("xmin", "ymin"),
    to = c("xmid", "ymax"),
  )

nc_map +
  layer_arrow(
    data = nc,
    from = c("xmin", "ymin"),
    to = c("xmid", "ymax"),
    geom = "curve",
    curvature = 0.25
  )

nc_map +
  layer_arrow(
    data = nc,
    from = c("xmax", "ymin"),
    to = c("xmid", "ymax"),
    geom = "arrowsegment"
  )
