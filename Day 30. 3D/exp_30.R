library(maptools)
library(raster)

# Load your point shapefile (with IP values in an IP field):
pts <- readShapePoints("pts.shp")

# Create a raster, give it the same extent as the points
# and define rows and columns:

rast <- raster()
extent(rast) <- extent(pts) # this might be unnecessary
ncol(rast) <- 20 # this is one way of assigning cell size / resolution
nrow(rast) <- 20

# And then ... rasterize it! This creates a grid version
# of your points using the cells of rast, values from the IP field:
rast2 <- rasterize(pts, rast, pts$IP, fun=mean)
