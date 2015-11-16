library(png)
library(ggmap)
library(ggplot2)
library(dplyr)
library(lubridate)

load("nyBoroughs.Rdata")

# Creat a new column wday to represent the weekday of Created.Date #
nyBoroughs$Created.Date = mdy_hms(nyBoroughs$Created.Date)
nyBoroughs <- nyBoroughs %>% dplyr::select(1:5) %>%
  mutate(wday  = lubridate::wday(Created.Date,label=TRUE))
# Subset latLongs including Complaint.Type, x, y, wday #
complaint <- dplyr::select(nyBoroughs, Complaint.Type, longitude, latitude, wday)
# Get map data for New York City #
# Set center to be the medoid of the lon and lat in complaint #
center <- c(mean(complaint$longitude), mean(complaint$latitude))
ny <- get_map(location = center, zoom = 11, color = "bw", source = "google")
nyMap <- ggmap(ny, base_layer = ggplot(aes(x = longitude, y = latitude), extent = "device",
                                        data = complaint))
# Draw 7 heat maps from Sun to Sat show distributions of complaint#
heatMap <- nyMap +
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 bins = 5, geom = "polygon",
                 data = complaint) +scale_fill_gradient(name = "Complaint", low = "yellow",
                                                             high= "red") +
  facet_wrap(~ wday) + scale_alpha(guide = 'none') + 
  ggtitle("Heat Map of Complaints in New York City by Day")

