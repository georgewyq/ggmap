# driving time to company
library(RCurl)
library(RJSONIO)
library(XML)
library(jsonlite)
library(zipcode)
library(dplyr)
install.packages('tigris')
library(tigris)
library(sp)
# test

get_dur <- function(orig, dest) {
  key <- "AIzaSyCBM982RgKbEl0tzBy-EokBap1X9D5AgJE"
  format <- "xml"
  api <- "https://maps.googleapis.com/maps/api/directions/"
  url <- paste0(api, format, "?origin=", origin, "&destination=", dest, "&key=", key)
  
  data <- getURL(url, .mapUnicode = F)
  doc <- xmlToList(data)
  dur <- as.integer(doc$route$leg$duration$value)
  return(dur)
}

orig <- "729%20El%20Camino%20Real%20Burlingame"
dest <- "500%20Airport%20Blvd%20Burlingame"
get_dur(orig, dest)

library(ggmap)
hdf <- get_map("burlingame, ca", maptype = "roadmap", zoom = 13)
ggmap(hdf, extent = "normal")

tl <- c(37.406766, -122.065711)
tr <- c(37.423500, -121.918925)
bl <- c(37.259240, -122.032080)
br <- c(37.259537, -121.796302)

lat_lim <- c(min(bl[1], br[1]), max(tl[1], tr[1]))
lon_lim <- c(min(bl[2], tl[2]), max(br[2], tr[2]))
center <- c(lon = mean(lon_lim), lat = mean(lat_lim))

hdf <- get_map(center, maptype = "roadmap", zoom = 11)
ggmap(hdf, extent = "normal")

coor_to_addr <- function(coor) return(paste0(round(coor["lat"], 5), ",", round(coor["lon"], 5)))
  
get_dur(dest, coor_to_addr(center))

lat_list <- seq(lat_lim[1], lat_lim[2], len = 40)
lon_list <- seq(lon_lim[1], lon_lim[2], len = 40)

points <- expand.grid(lat = lat_list, lon = lon_list)
system.time(query <- apply(points, 1, function(coor) get_dur(dest, coor_to_addr(coor))))

plot_data <- data.frame(points, time = query / 60)

ggmap(hdf, extent = "normal") + geom_point(data = plot_data, aes(x = lon, y = lat, color = time))

fit <- loess( time ~ lat + lon, data = plot_data, span = 0.05)

lat_list <- seq(lat_lim[1], lat_lim[2], len = 100)
lon_list <- seq(lon_lim[1], lon_lim[2], len = 100)

new_points <- expand.grid(lat = lat_list, lon = lon_list)

pred <- predict(fit, new_points)
pred_long <- as.data.frame(melt(pred))
pred_long$lat <- as.numeric(gsub("lat=", "", as.character(pred_long$lat)))
pred_long$lon <- as.numeric(gsub("lon=", "", as.character(pred_long$lon)))
pred_long$time <- pred_long$value
pred_long$value <- NULL

shapefile <- zctas(cb = TRUE, starts_with = zip_list)
plot(shapefile)
shapefile_df <- fortify(shapefile)
shapefile_df$id <- as.factor(shapefile_df$id)
levels(shapefile_df$id) <- shapefile$ZCTA5CE10

ggmap(hdf) + 
  geom_contour(data = pred_long, aes(z = time), binwidth = 2) +
  coord_cartesian(xlim = lon_lim, ylim = lat_lim) +
  geom_path(data = shapefile_df, aes(x = long, y = lat, group = id), color = "pink") +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = id, fill = id), alpha = 0.5)

ggmap(hdf) + 
  geom_contour(data = plot_data, aes(z = time), binwidth = 5) +
  ylim(lat_lim[1], lat_lim[2]) +
  xlim(lon_lim[1], lon_lim[2])

zip_list <-
zipcode %>%
  filter(state == "CA") %>%
  filter(city %in% c("San Jose", "Sunnyvale", "Santa Clara"))

