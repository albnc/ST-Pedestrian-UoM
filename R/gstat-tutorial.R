## This tutorial is based on the vignettes of GSTAT:
## https://cran.r-project.org/web/packages/gstat/vignettes/gstat.pdf

library(sp)
data(meuse)


# Meuse Data --------------------------------------------------------------
class(meuse)
names(meuse)
meuse

# Attribute the coordinates ------------------------------------------------
coordinates(meuse) <- ~x+y
class(meuse)
summary(meuse)

coordinates(meuse)[1:5,]

## Plot the zinc properties
bubble(meuse, "zinc", 
       col=c("#00ff0088", "#00ff0088"), main = "zinc concentrations (ppm)")

# Meuse data grid ---------------------------------------------------------
data("meuse.grid")
summary(meuse.grid)

coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
image(meuse.grid["dist"])
title("Distance to river (red = 0)")


# GSTAT package -----------------------------------------------------------
library(gstat)
zinc.idw <- idw(zinc~1, meuse, meuse.grid)
spplot(zinc.idw["var1.pred"], main="zinc inverse distance weighted interpolations")

plot(log(zinc)~sqrt(dist), meuse)
abline(lm(log(zinc)~sqrt(dist), meuse))


# VARIOGRAMS --------------------------------------------------------------
## Constant model ~1
lzn.vgm <- variogram(log(zinc)~1, meuse)
lzn.vgm

lzn.fit = fit.variogram(lzn.vgm, model=vgm(1, "Sph", 900,1))
lzn.fit
plot(lzn.vgm, lzn.fit)

## Using a mean function ~sqrt(dist) as a predictor value
lznr.vgm <- variogram(log(zinc)~sqrt(dist), meuse)
lznr.fit <- fit.variogram(lznr.vgm, model=vgm(1, "Exp", 300, 1))
lznr.fit
plot(lznr.vgm, lznr.fit)


# KRIGING -----------------------------------------------------------------
lzn.kriged <- krige(log(zinc)~1, meuse, meuse.grid, model=lzn.fit)
spplot(lzn.kriged["var1.pred"])


# CONDITIONAL SIMULATION --------------------------------------------------
lzn.condsim <- krige(log(zinc)~1, meuse, meuse.grid, model=lzn.fit, 
                     nmax=30, nsim=4)
spplot(lzn.condsim, main="four conditional simulations")

lzn.condsim2 <- krige(log(zinc)~sqrt(dist), meuse, meuse.grid, model=lznr.fit, 
                     nmax=30, nsim=4)
spplot(lzn.condsim2, main="four conditional simulations")


# DIRECTIONAL VARIOGRAM ---------------------------------------------------
lzn.dir <- variogram(log(zinc)~1, meuse, alpha=c(0,45,90,135))
lzndir.fit <- vgm(.59, "Sph", 1200, .05, anis=c(45, .4))
plot(lzn.dir, lzndir.fit, as.table=TRUE)

lznr.dir <- variogram(log(zinc)~sqrt(dist), meuse, alpha=c(0,45,90,135))
plot(lznr.dir, lznr.fit, as.table=TRUE)

# VARIOGRAM MAPS ----------------------------------------------------------
vgm.map <- variogram(log(zinc)~sqrt(dist), meuse, cutoff = 1500, width = 100,
                     map = TRUE)
plot(vgm.map, threshold=5)
