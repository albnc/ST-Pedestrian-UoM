library(sp)
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

# SIMPLE FEATURE (SF) ---------------------------------------------------------------
## Created by Pebesma: [Simple Feature for R](https://r-spatial.github.io/sf/articles/sf1.html)
## Used in most of packages to run variogram or spatial-temporal analysis
ped.sf <- st_as_sf(ped.summary, coords = c("long", "lat"), crs=4326)
## Check if the data are in LongLat
st_is_longlat(ped.sf)
ped.sf$geometry
## Plot standard
plot(st_geometry(ped.sf), border='grey', axes=TRUE)
## Plot GGPLOT2
ggplot() + geom_sf(data=ped.sf)
## Plot LEAFLET - just works with WGS84 (CRS=4326)
leaflet(data=ped.sf) %>% 
  # addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircles(radius=~sqrt(count.avg)*2, color='green')
  

## Change to the Australian CRS (projected points)
ped.sf <- st_transform(ped.sf, crs=28355)
st_is_longlat(ped.sf)
ped.sf$geometry
plot(st_geometry(ped.sf), border='grey', axes=TRUE)
ggplot() + geom_sf(data=ped.sf, 
                   aes(col=cut(ped.sf$count.avg, quantile(ped.sf$count.avg, 
                                                          probs=seq(0,1,.25)))),
                   size=sqrt(ped.sf$count.avg/50))

## There is a difference plotting both points with different CRS


# SPATIAL (SP) ----------------------------------------------------------------------
wgs84 <- CRS("+proj=longlat +datum=WGS84")
crs.vic <- CRS("+proj=tmerc +lat_0=0 +lon_0=145 +k=1 +x_0=500000 +y_0=10000000 +ellps=WGS84 +units=m +no_defs") 
crs.aus <- CRS("+proj=tmerc +lat_0=0 +lon_0=145.9965556 +k=1.000006 +x_0=365616.076 +y_0=4495192.464 +ellps=aust_SA +units=m +no_defs")

## Completely Manual
ped.mat <- cbind(ped.summary$long, ped.summary$lat)
row.names(ped.mat) <- 1:nrow(ped.mat)
ped.sp0 <- SpatialPoints(ped.mat, wgs84)
ped.sp0

## Manual way
ped.sp1 <- ped.summary
coordinates(ped.sp1) <- ~long+lat
proj4string(ped.sp1) <- wgs84
summary(ped.sp1)

## Easiest way
ped.sp <- as_Spatial(ped.sf)
summary(ped.sp)


# PACKAGE GSTAT ---------------------------------------------------------------------
## The variogram uses the classes SP, that contains 'coords' with the positions
## Constant model ~1
ped.vgm <- variogram(log(count.tot)~coords.x1+coords.x2, ped.sp)
ped.vgm

ped.fit = fit.variogram(ped.vgm, model=vgm(1, "Exp", 900,1))
ped.fit
plot(ped.vgm, ped.fit)









# PACKAGE GEOR (UFPR) ---------------------------------------------------------------
## The variogram run using the class 'geodata' that contains two lists: coords and data
ped <- as.geodata(ped.summary, coords.col = 2:3, data.col = 4 )
summary(ped)
plot.geodata(ped, col.data = 1)
points(ped, col.data = 1, xlab="Longitude", ylab="Latitude", pt.divide = "quartiles")

cloud1 <- variog(ped, option="cloud")
plot(cloud1)
