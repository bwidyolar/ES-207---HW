#CLEAR VARIABLES
rm(list=ls())
frame()

#install.packages("foreign")
library(foreign)
#install.packages("rgdal")
library(rgdal)
#install.packages("raster")
library(raster)

#LOAD DBF FILE
meadows <- read.dbf('Sierra_Nevada_MultiSource_Meadow_Polygons_Compilation_v1.dbf')

#SUBSET TO RECORDS HAVING HGM VALUES
meadows <- subset(meadows, !is.na(HGM_TYPE))

#FORMATE DATA FRAME COLUMNS
meadows$area.sqkm = meadows[,"Shape_Area"]/1000000 # m^2 to km^2
meadows$catch.sqkm = meadows[,"CATCHMENT_"]/1000000# m^2 to km^2
meadows$elev_m = meadows[,"ELEV_MEAN"]
meadows$elev_r = meadows[,"ELEV_RANGE"]
meadows$lat_dd = meadows[,"LAT_DD"]
meadows$lon_dd = meadows[,"LONG_DD"]
meadows$slope.pct = meadows[,"FLOW_SLOPE"]
meadows$edge.comp = meadows[,"EDGE_COMPL"]
meadows$clay = meadows[,"ClayTot_r"]
meadows$soil.kf = meadows[,"Kf"]

#NOTE RELEVANT COLUMNS
rel_cols = c("area.sqkm", "catch.sqkm", "elev_m", "elev_r", "lat_dd", "lon_dd", "slope.pct", "edge.comp", "clay", "soil.kf")

pairs(~area.sqkm + catch.sqkm + elev_m + elev_r + lat_dd + lon_dd + slope.pct + edge.comp + clay + soil.kf,data=meadows)
#pairs(meadows[rel_cols],data=meadows) -> produces 50+ warnings!


#dist using euclidean
meadows.dist <- dist(x = meadows[,rel_cols],method = "euclidean")
#hclust using ward.D
meadows.hc <- hclust(meadows.dist,method="ward.D")

####################################### HIERARCHICAL CLUSTERING

#PLOT WITH RECTANGLES AROUND CLUSTERS
plot(meadows.hc)
rect.hclust(meadows.hc, k=6)

#STORE CLUSTER GROUP NUMBER IN DATA FRAME
meadows$hc6 <- cutree(meadows.hc, k=6)

######################################### K MEANS CLUSTERING

#K MEANS CLUSTERING
meadows.km6 <- kmeans(meadows[,rel_cols],centers =6)
#STORE GROUP #
meadows$km6 <- meadows.km6$cluster

#COMPARE CLUSTERING METHODS
table(meadows$hc6, meadows$km6)

gdal_grid = readGDAL("DEM.tif")
DEM = raster(gdal_grid) #use data as a projected raster
plot(DEM, col=gray.colors(10, start=0.9, end=0.3),main="Hierarchical Cluster Locations")

#PLOT HC CLUSTERS
codeColors = c("red","blue","green","black","orange","yellow")
points(meadows$lon_dd,meadows$lat_dd,col=codeColors[meadows$hc6],pch=20)
#DOUBLE CHECK BELOW
#clusters <- aggregate(cbind(lat_dd,lon_dd) ~ hc6, data=meadows, mean)
#points(clusters$lon_dd,clusters$lat_dd,col=codeColors,pch=15)

#PLOT KMEANS CLUSTERS
plot(DEM, col=gray.colors(10, start=0.9, end=0.3),main="K-Means Cluster Locations")
points(meadows$lon_dd,meadows$lat_dd,col=codeColors[meadows$km6],pch=20)
#DOUBLE CHECK BELOW
#clusters <- aggregate(cbind(lat_dd,lon_dd) ~ km6, data=meadows, mean)
#points(clusters$lon_dd,clusters$lat_dd,col=codeColors,pch=15)


#########################       PRINCIPAL COMPONENT ANALYSIS - PCA

meadows.pca <- prcomp(x = meadows[,rel_cols], scale=TRUE, retx = TRUE, center = TRUE, scores=TRUE)
print("Principal Component Analylsis")
print(summary(meadows.pca))

plot(meadows.pca,type="lines")

print(meadows.pca$rotation)

biplot(meadows.pca, choices=1:2, cex=0.5, xlim=c(-0.1,0.2), ylim=c(-0.1,0.2))
biplot(meadows.pca, choices=2:3, cex=0.5, xlim=c(-0.1,0.3), ylim=c(-0.12,0.2))
biplot(meadows.pca, choices=3:4, cex=0.5, xlim=c(-0.12,0.25), ylim=c(-0.12,0.2))
biplot(meadows.pca, choices=4:5, cex=0.5, xlim=c(-0.2,0.25), ylim=c(-0.12,0.35))

pairs(meadows.pca$x[,1:5],col=meadows$km6)
pairs(meadows.pca$x[,1:5],col=meadows$hc6)

#CONTINGENCY ANALYSIS
print(table(meadows$HGM_TYPE,meadows$km6))
print(table(meadows$HGM_TYPE,meadows$hc6))

chisq.test(meadows$HGM_TYPE,meadows$km6)
chisq.test(meadows$HGM_TYPE,meadows$hc6)

print(table(meadows$DOM_ROCKTY,meadows$km6))
print(table(meadows$VEG_MAJORI,meadows$km6))

chisq.test(meadows$DOM_ROCKTY,meadows$km6)
chisq.test(meadows$VEG_MAJORI,meadows$km6)

#SUMMARIZE DATA BY NATIONAL FOREST AND CLUSTER TYPE
ownership <- aggregate(AREA_ACRE ~ km6+OWNERSHIP, data=meadows, sum)
#write.csv(ownership)
