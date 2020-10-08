library(sp)
#library(rgdal)
library(sf)     # Simple Feature -> Edzer Pebesma
library(gstat)  # -> Edzer Pebesma, University of Munster, Germany
library(geoR)   # -> Paulo Ribeiro Jr, Federal University of Parana (UFPR), Brazil
library(leaflet)
library(RColorBrewer) # color palettes


# LOAD DATASET ----------------------------------------------------------------------
source("R/01-datafilter.R")

# Is it necessary to transform (Long,Lat) in (X,Y)? ---------------------------------
## It is necessary to understand CRS (Coordinate Reference System). There are a lot of different CRS, for each region in the world. 
## In [1], the region with Melbourne is CRS=28355. 
## In [2], the authors suggest WGS84 (CRS=4326) as the default for the world.
## In [3], some suggestion to Victoria CRS (epsg:7899, epsg:3111 - VicRoads)

## [1] [Spatial Reference](https://spatialreference.org/ref/epsg/gda94-mga-zone-55/)
## [2] [Geocomputing with R](https://geocompr.robinlovelace.net/reproj-geo-data.html)
## [3] [EPSG.io](https://epsg.io/?q=Australia)

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
ggplot() + geom_sf(data=ped.sf) 
                   # aes(col=cut(ped.sf$count.avg, quantile(ped.sf$count.avg, 
                   #                                        probs=seq(0,1,.25)))),
                   # size=sqrt(ped.sf$count.avg/50))

## There is a difference plotting both points with different CRS


# SPATIAL (SP) ----------------------------------------------------------------------
wgs84 <- CRS("+proj=longlat +datum=WGS84")
crs.vicroads <- CRS("+proj=tmerc +lat_0=0 +lon_0=145 +k=1 +x_0=500000 +y_0=10000000 +ellps=WGS84 +units=m +no_defs") 
crs.vic <- CRS("+init=epsg:7899")
crs.aus <- CRS("+proj=tmerc +lat_0=0 +lon_0=145.9965556 +k=1.000006 +x_0=365616.076 +y_0=4495192.464 +ellps=aust_SA +units=m +no_defs")

## Easiest way
ped.sp <- as_Spatial(ped.sf)
summary(ped.sp)

## Manually. References:
# [Youtube](https://youtu.be/93_JSqQ3aG4)
# [Introduction to Visualising spatial data in R](https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf)

ped.sp <- data.frame(ped.summary %>% rename(X=long, Y=lat))
coordinates(ped.sp) <- ~X+Y
proj4string(ped.sp) <- wgs84
ped.sp <- spTransform(ped.sp, crs.vic)
summary(ped.sp)


as(ped.sp, "data.frame") %>%
ggplot(aes(X, Y, size=count.tot, color=count.tot)) +
 geom_point() + coord_equal()

# PACKAGE GSTAT --------------------------------------------------------------------
## The variogram uses the classes SP, that contains 'coords' with the positions

# Spatial Grid to feed models -------------------------------------------------------
## Create the grid to feed other points. Extract any intersection from OSMDATA
X <- seq(from=min(ped.sp$X),to=max(ped.sp$X), by=100)
Y <- seq(from=min(ped.sp$Y),to=max(ped.sp$Y), by=100)
coords.df <- crossing(X,Y)
## Transform in SP class
coordinates(coords.df) <- ~X+Y
proj4string(coords.df) <- crs.vic

# Inverse distance
ped.idw <- idw(count.tot~1, idp=2.0, ped.sp, coords.df)

ggplot() +
  geom_tile(data=as.data.frame(ped.idw), aes(x=X,y=Y, fill=var1.pred)) +
  geom_point(data=as.data.frame(ped.sp), aes(x=X, y=Y)) +
  coord_equal() +
  scale_fill_gradientn(colors=terrain.colors(10)) +
  theme_bw()

# Variogram gstat -------------------------------------------------------------------
func.variogram <- function(data, model="Sph", alpha=0, tol=45) {
  ped.vgm <<- variogram(log(count.tot)~X+Y, data, cutoff = 2000, width=100, alpha=alpha, tol.hor=tol, cressie=TRUE)
  print(ped.vgm)
  
  ## There are four models to calibrate the variogram: Sph, Exp, Gau, Mat
  ## The parameters psill, range and nugget will be estimated. It is a good practice to set up a guess. If there are no guesses, use NA in each parameter.
  ped.fit <<-fit.variogram(ped.vgm, model=vgm(psill = 1, model = model, range = 100))
  print(ped.fit)
  return(plot(ped.vgm, ped.fit))
}
# Models for one direction and angle
func.variogram(data=ped.sp, model="Sph", alpha=45, tol=45)

# Models for more than one direction and angle
func.variogram(data=ped.sp, model="Sph", alpha=c(0,45,90,135), tol=22.5)

  
# Kriging gstat ---------------------------------------------------------------------
ped.krige <- krige(formula = count.tot~X+Y,
                   data = ped.sp,
                   newdata = coords.df,
                   model = ped.fit)












# PACKAGE GEOR (UFPR) ---------------------------------------------------------------
## The variogram run using the class 'geodata' that contains two lists: coords and data
ped <- as.geodata(ped.summary, coords.col = 2:3, data.col = 4 )
summary(ped)
plot.geodata(ped, col.data = 1)
points(ped, col.data = 1, xlab="Longitude", ylab="Latitude", pt.divide = "quartiles")

cloud1 <- variog(ped, option="cloud")
plot(cloud1)
