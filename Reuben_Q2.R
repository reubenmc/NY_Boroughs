# Required libraries for analysis #

library(rgdal)
library(rgeos)
library(e1071)  # for svm()
library(sp)
library(raster)
library(maptools)
library(plyr)
library(dplyr)
gpclibPermit()
library(nnet)

# load in geocoded data, dataframe called nyBoroughs #
load("nyBoroughs.Rdata")

# remove duplicate coordinates #
nyBoroughs<-nyBoroughs[!(duplicated(nyBoroughs[c("longitude","latitude")])),]

# used to rename polygon data at bottom #
short_to_long = c("BK"="Brooklyn", 
                  "BX"="Bronx",
                  "MN"="Manhattan",
                  "QN"="Queens",
                  "SI"="Staten Island")

# Covering function adapted from stackexchange #
# http://stackoverflow.com/questions/30585924/spatial-data-in-r-plot-decision-regions-of-multi-class-svm #
 
# randomly sample 10000 data points from each borough, call this sampNY #
set.seed(98192895)
sampNY = nyBoroughs %>%
  group_by(Borough) %>%
  do(sample_n(., 10000))

# select only the borough, long, and lat columns #
sampNY = sampNY[, c(3,4,5)]

#change boroughs to factors#
sampNY$Borough <- as.factor(sampNY$Borough)

# rename our data frame #
names(sampNY) = c("Borough", "long", "lat")

## Function to create raster just covering sample area ##
## Create a mask of the data region, as a data frame of x/y points. ##
covering <- function(data, xlen=150, ylen=150) {
  # Convex hulls of each class's data points:
  polys <- dlply(data, .(Borough), function(x) Polygon(x[chull(x[-1]), -1]))
  # Union of the hulls:
  bbs <- unionSpatialPolygons(SpatialPolygons(list(Polygons(polys, 1))), 1)
  
  # Pixels that are inside the union polygon:
  grid <- expand.grid(x=seq(min(data$long), max(data$long), length.out=xlen),
                      y=seq(min(data$lat), max(data$lat), length.out=ylen))
  grid[!is.na(over(SpatialPoints(grid), bbs)), ]
}

# does regression, using svm #
m <- svm(Borough ~ long+lat, sampNY)
# creates raster grid using covering function for the mask above #
# load pluto data to create grid #
load("/home/vis/cr173/Sta523/data/nyc/pluto/pluto.Rdata")
pluto = pluto[, c(5,1,2)]

# converts boroughs to factor variables #
pluto$Borough <- as.factor(pluto$Borough)

# rename our data frame #
names(pluto) = c( "Borough","long", "lat")

# Create grid for prediction with pluto data #
grid <- covering(pluto)


# renames raster grid to match data frame #
names(grid) = c("long","lat")


# Predict locations of points in grid using svm regression #
pred = predict(m, grid)

# Plot geocoded ponts on top of predicted polygons #
plot(lat ~ long, grid, col=pred, pch=20, asp =1)
points(nyBoroughs$longitude, nyBoroughs$latitude, pch=16, 
       cex = 0.01, col = adjustcolor("yellow", 0.01))
legend("topleft", c("Brooklyn", "Bronx", "Manhattan", "Queens", "Staten Island"), 
       col=c("black", "red", "green", "blue", "cyan"),
       pch = 15)

# Conversion into Spatial Polygons #
sp.grid <- cbind(grid, pred)
coordinates(sp.grid) <- ~ long + lat
gridded(sp.grid) <- TRUE
sp.grid <- raster(sp.grid)

poly <- rasterToPolygons(sp.grid, n = 16, dissolve = TRUE)

# Renaming spatial polygon #
names(poly@data) = "Name"

poly@data$Name = short_to_long[levels(pred)]

# writes out results to geoJson file #
source("write_geojson.R")
write_geojson(poly,"boroughs.json")



