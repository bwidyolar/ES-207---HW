#CLEAR VARIABLES
rm(list=ls())
#RESET PLOT
par(mfrow=c(1,1))

#install.packages("maptools")
#install(akima)
library(maptools)
library(akima)

pt.shp <- readShapePoints("ahearn_allobs/allobs")

pt.shp.18 <- subset(pt.shp, DateTime_ == "2005-02-18")
pt.shp.23 <- subset(pt.shp, DateTime_ == "2005-02-23")

pt.shp.18.temp <- interp(pt.shp.18$X, pt.shp.18$Y, pt.shp.18$Temp, duplicate="mean")
pt.shp.18.chlo <- interp(pt.shp.18$X, pt.shp.18$Y, pt.shp.18$Chlorophyl, duplicate="mean")
pt.shp.18.do <- interp(pt.shp.18$X, pt.shp.18$Y, pt.shp.18$DO__, duplicate="mean")

par(mfrow=c(2,3))

image(pt.shp.18.temp, col=cm.colors(24))
contour(pt.shp.18.temp, add=T)
image(pt.shp.18.chlo, col=topo.colors(24))
contour(pt.shp.18.chlo, add=T)
image(pt.shp.18.do, col=heat.colors(24))
contour(pt.shp.18.do, add=T)

persp(pt.shp.18.temp, theta=-50, phi=45, col="lightblue",zlab="Temp C", xlab="UTM X (m)", ylab="UTM Y (m)")
persp(pt.shp.18.chlo, theta=-50, phi=45, col="lightblue",zlab="Chlorophyll mg/L", xlab="UTM X (m)", ylab="UTM Y (m)")
persp(pt.shp.18.do, theta=-50, phi=45, col="lightblue",zlab="Dissolved Oxygen mg/L", xlab="UTM X (m)", ylab="UTM Y (m)")

