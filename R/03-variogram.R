library(sp)
#library(rgdal)
library(sf)     # Simple Feature -> Edzer Pebesma
library(gstat)  # -> Edzer Pebesma, University of Munster, Germany
library(geoR)   # -> Paulo Ribeiro Jr, Federal University of Parana (UFPR), Brazil
library(leaflet)
library(RColorBrewer) # color palettes


# LOAD DATASET ----------------------------------------------------------------------
# source("R/01-datafilter.R")
ped.year <- readRDS("data/pedyear.RDS")
ped.summary <- readRDS("data/pedsummary.RDS")

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
ped.sf <- st_transform(ped.sf, crs=st_crs(7899))
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
proj4string(ped.sp) <- as.character(NA)

## Transfor with no projection *potencially* cause of errors in Kriging functio
ped.sp <- as.data.frame(as_Spatial(ped.sf))
ped.sp <- ped.sp %>% rename(X=coords.x1, Y=coords.x2)
coordinates(ped.sp) <- ~X+Y
# No projections
proj4string(ped.sp)

as(ped.sp, "data.frame") %>%
ggplot(aes(X, Y, size=count.tot, color=count.tot)) +
 geom_point() + coord_equal()

# PACKAGE GSTAT --------------------------------------------------------------------
## The variogram uses the classes SP, that contains 'coords' with the positions

# Spatial Grid to feed models -------------------------------------------------------
## Create the grid to feed other points. Extract any intersection from OSMDATA
x.range <- as.integer(range(ped.sp$X))
y.range <- as.integer(range(ped.sp$Y))
X <- seq(from=x.range[1],to=x.range[2], by=100)
Y <- seq(from=y.range[1],to=y.range[2], by=100)
#coords.df <- expand.grid(X,Y)
coords.df <- crossing(X,Y)
## Transform in SP class
coordinates(coords.df) <- ~X+Y
proj4string(coords.df) <- proj4string(ped.sp)
# gridded(coords.df) <- TRUE

## Plot the grid and known points
ggplot() +
  geom_point(data = as.data.frame(coords.df), aes(X,Y), shape=3) +
  geom_point(data = as.data.frame(ped.sp), aes(X,Y, color = "red", size = .5)) + coord_equal()

## Inverse distance
ped.idw <- idw(formula = count.tot~1, 
               idp = 2.0, 
               locations = ped.sp, 
               newdata = coords.df)

ggplot() +
  geom_tile(data=as.data.frame(ped.idw), aes(x=X,y=Y, fill=var1.pred)) +
  geom_point(data=as.data.frame(ped.sp), aes(x=X, y=Y)) +
  coord_equal() +
  scale_fill_gradientn(colors=terrain.colors(10)) +
  theme_bw()

# Variogram gstat -------------------------------------------------------------------
func.variogram <- function(data, model="Sph", alpha=0, tol=45) {
  ped.vgm <<- variogram(log(count.tot)~X+Y, data, cutoff = 2000, width=100, alpha=alpha, tol.hor=tol, cressie=FALSE)
  print(ped.vgm)
  
  ## There are four models to calibrate the variogram: Sph, Exp, Gau, Mat
  ## The parameters psill, range and nugget will be estimated. It is a good practice to set up a guess. If there are no guesses, use NA in each parameter.
  ped.fit <<-fit.variogram(ped.vgm, model=vgm(psill = 1, model = model, range = 100))
  print(ped.fit)
  return(plot(ped.vgm, ped.fit))
}
# Models for one direction and angle
func.variogram(data=ped.sp, model="Gau", alpha=0, tol=22.5)

# Models for more than one direction and angle
func.variogram(data=ped.sp, model="Gau", alpha=c(0,45,90,135), tol=22.5)

  
# Kriging gstat ---------------------------------------------------------------------
# Using anisotropic model
ped.vm.ani <- vgm(ped.fit$psill, ped.fit$model, ped.fit$range, nugget = 0, anis = c(90,100))
ped.krige <- krige(formula = count.tot~1,
                   locations = ped.sp,
                   newdata = coords.df,
                   model = ped.vm.ani)

ggplot() +
  geom_tile(data=as.data.frame(ped.krige), aes(x=X,y=Y, fill=var1.pred)) +
  geom_point(data=as.data.frame(ped.sp), aes(x=X, y=Y)) +
  coord_equal() +
  scale_fill_gradientn(colors=terrain.colors(10)) +
  theme_bw()










# PACKAGE GEOR (UFPR) ---------------------------------------------------------------
## The variogram run using the class 'geodata' that contains two lists: coords and data
ped.geo <- as.geodata(as.data.frame(ped.sp), coords.col = 6:7, data.col = 2 )
summary(ped.geo)
plot.geodata(ped.geo, col.data = 1)
points(ped.geo, col.data = 1, xlab="X", ylab="Y", pt.divide = "quartiles")

cloud1 <- variog(ped.geo, option="cloud", max.dist = 2400)
plot(cloud1)

bin1 <- variog(ped.geo, uvec = seq(0,2400,100), direction = 3*pi/4,
               estimator.type = "modulus")#, bin.cloud = T)
plot(bin1)#, bin.cloud=T)

vario.4 <- variog4(ped.geo, max.dist = 2400)
plot(vario.4, omni = TRUE)

xv.ml <- xvalid(ped.geo, model = ml)
