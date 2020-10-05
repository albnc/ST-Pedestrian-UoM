library(sf)     # Simple Feature -> Edzer Pebesma
library(gstat)  # -> Edzer Pebesma, University of Munster, Germany
library(geoR)   # -> Paulo Ribeiro Jr, Federal University of Parana (UFPR), Brazil
library(leaflet)


# LOAD DATASET ----------------------------------------------------------------------
source("R/01-datafilter.R")

# Is it necessary to transform (Long,Lat) in (X,Y)? ---------------------------------
## It is necessary to understand CRS (Coordinate Reference System). There are a lot of different CRS, for each region in the world. 
## In [1], the region with Melbourne is CRS=28355. 
## In [2], the authors suggest WGS84 (CRS=4326) as the default for the world.

## [1] [Spatial Reference](https://spatialreference.org/ref/epsg/gda94-mga-zone-55/)
## [2] [Geocomputing with R](https://geocompr.robinlovelace.net/reproj-geo-data.html)

ped.sf <- st_as_sf(ped.summary, coords = c("long", "lat"), crs=4326)
## Check if it came as LongLat
st_is_longlat(ped.sf)
ped.sf$geometry
# Plot standard
plot(st_geometry(ped.sf), border='grey', axes=TRUE)
# Plot GGPLOT2
ggplot() + geom_sf(data=ped.sf)
# Plot LEAFLET - just works with WGS84 (CRS=4326)
leaflet(data=ped.sf) %>% 
  # addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircles(radius=~sqrt(count.avg)*2, color='green')
  

# Change to the Australian CRS (projected points)
ped.sf <- st_transform(ped.sf, crs=28355)
st_is_longlat(ped.sf)
ped.sf$geometry
plot(st_geometry(ped.sf), border='grey', axes=TRUE)
ggplot() + geom_sf(data=ped.sf, 
                   aes(col=cut(ped.sf$count.avg, quantile(ped.sf$count.avg, 
                                                          probs=seq(0,1,.25)))),
                   size=sqrt(ped.sf$count.avg/50))

## There is a difference plotting both points with different CRS


# Add X, Y coordinates to the dataframe ---------------------------------------------
ped.summary %>% mutate(X=st_coordinates(ped.sf)[,1], Y=st_coordinates(ped.sf)[,2])


# PACKAGE GSTAT ---------------------------------------------------------------------
## The variogram uses the classes sp or sf, that contains 'geometry' as the positions
## Constant model ~1
lzn.vgm <- variogram(log(zinc)~1, meuse)
lzn.vgm

lzn.fit = fit.variogram(lzn.vgm, model=vgm(1, "Sph", 900,1))
lzn.fit
plot(lzn.vgm, lzn.fit)









# PACKAGE GEOR (UFPR) ---------------------------------------------------------------
## The variogram run using the class 'geodata' that contains two lists: coords and data
ped <- as.geodata(ped.summary, coords.col = 2:3, data.col = 4 )
summary(ped)
plot.geodata(ped, col.data = 1)
points(ped, col.data = 1, xlab="Longitude", ylab="Latitude", pt.divide = "quartiles")

cloud1 <- variog(ped, option="cloud")
plot(cloud1)
