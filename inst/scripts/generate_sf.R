# Create waterways data set

library(sf)
library(geojsonsf)

river_names <- c("River Clyde", "River Forth", "Forth and Clyde Canal")
rivers <- st_read("~/shp")
rivers <- rivers[rivers$name %in% river_names, -c(1, 4)]

UK <- geojson_sf(paste0("http://geoportal1-ons.opendata.arcgis.com",
                        "/datasets/48b6b85bb7ea43699ee85f4ecd12fd36_2.",
                        "geojson?outSR={%22latestWkid%22:27700,",
                        "%22wkid%22:27700}"))

Scotland <- UK[grep("(Scotland)|(Highlands)", UK$nuts218nm), ]

Scotland <- st_union(Scotland)

land <- st_sf(geometry = Scotland, name = "land", type = "land")
land <- st_transform(land, st_crs(rivers))

waterways <- rbind(land, rivers)

waterways <- st_crop(waterways, xmin = -7, xmax = -2,
                     ymin = 54.5, ymax = 56.5)
