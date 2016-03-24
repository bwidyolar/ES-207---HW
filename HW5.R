#CLEAR VARIABLES
rm(list=ls())

#install.packages("rgdal")
library(rgdal)
#install.packages("raster")
library(raster)

#LOAD DATA
rip <- read.csv("riparian_cleaned.csv")

#SORT OUT ONLY TOP 5 SPECIES
top5 <- rip[c(which(rip$Genus == "Acer")),]
top5 <- rbind(top5,rip[c(which(rip$Genus == "Quercus")),])
top5 <- rbind(top5,rip[c(which(rip$Genus == "Populus")),])
top5 <- rbind(top5,rip[c(which(rip$Genus == "Salix")),])
top5 <- rbind(top5,rip[c(which(rip$Genus == "Fraxinus")),])
top5 <- droplevels(top5)
rip <- top5
rm(top5)

#ADD HEIGHT AS CM
rip$htcm <- rip$Woody_Height_m * 100  #CONVERT TO CM
#CLEAN DATA OF NULL VALUES
rip <- rip[-c(which(rip$htcm > 6000)), ]

#PLOT DEM RASTER DATA
filename = "DEM.tif"
gdal_grid = readGDAL(filename)
DEM = raster(gdal_grid) #use data as a projected raster
plot(DEM)
#OVERLAY PROJECT LOCATIONS
ProjLoc <- aggregate(cbind(Latitude,Longitude) ~ ProjCode, data=rip, mean)
codeColors = c("red","blue","green","black")
symbols = c(17,19,15,18)
points(ProjLoc$Longitude,ProjLoc$Latitude,col=codeColors,pch=symbols)
legend("topright",inset=0.05,title="Project Site",levels(ProjLoc$ProjCode),pch=symbols,col=codeColors)

#EXTRACT ELEVATION RASTER DATA
rip$Elevation <- extract(DEM,cbind(rip$Longitude,rip$Latitude))
#EXTRACT TEMPERATURE RASTER DATA
filename = "tmean_8.tif"
gdal_grid = readGDAL(filename)
temperature = raster(gdal_grid) #use data as a projected raster
rip$Temp_aug <- extract(temperature,cbind(rip$Longitude,rip$Latitude))
#EXTRACT PRECIPITATION RASTER DATA
#EXTRACT TEMPERATURE RASTER DATA
filename = "precip_8.tif"
gdal_grid = readGDAL(filename)
precipitation = raster(gdal_grid) #use data as a projected raster
rip$Precp_aug <- extract(precipitation,cbind(rip$Longitude,rip$Latitude))

#CLEAN DATA
rip <- rip[-c(which(rip$Elevation > 200)), ]

#TEST DISTRIBUTIONS
#hist(rip$Elevation)
#hist(rip$Precp_aug)
#hist(rip$Temp_aug)
#TEMPERATURE IS SOMEWHAT NORMALLY DISTRIBUTED
#OTHERS ARE NOT

#TEST CORRELATIONS
#cor(rip$Elevation, rip$Precp_aug) - 0.9661259
#cor(rip$Elevation, rip$Temp_aug) - 0.5831392
#cor(rip$Precp_aug, rip$Temp_aug) - 0.6183203
#ELEVATION AND PRECIPITATION IS HIGHLY CORRELATED
#OTHERS NOT SO MUCH

#LINEAR MODELS
lm.pred <- lm(htcm~Woody_DBH_cm*Genus, data=rip) #BASE PREDICTION
lm.pred1 <- lm(htcm~Woody_DBH_cm*Genus + Latitude, data=rip) #WITH LATITUDE
lm.pred2 <- lm(htcm~Woody_DBH_cm*Genus + Elevation, data=rip) #WITH ELEVATION
lm.pred3 <- lm(htcm~Woody_DBH_cm*Genus + Precp_aug, data=rip) #WITH PRECIPITATION
lm.pred4 <- lm(htcm~Woody_DBH_cm*Genus + Temp_aug, data=rip) #WITH TEMPERATURE

AIC(lm.pred,lm.pred1,lm.pred2,lm.pred3,lm.pred4)
#BASED ON THE LINEAR MODELS r SQUARED AND THE AIC VALUE, THE MODEL WITH ELEVATION IS THE BEST, BUT NOT BY MUCH

#temp = subset(rip, Genus == "Quercus")
#temp2 <- lm(temp$htcm ~ temp$Woody_DBH_cm)
#plot(rip$Woody_DBH_cm,rip$htcm)
#abline(coefficients(temp2))

#USING MODEL WITH NEW DATA, USING BASE MODEL SO WE DONT NEED TO ADD ELEVATION DATA TO THE NEW DATA
newData <- read.csv("new_data.csv")
newData$htcm <- predict(lm.pred,newData)
newData$Biomass = 705 * (0.0000334750*(newData$Woody_DBH_cm^2.33631)*(newData$htcm^0.74872))
newData$Carbon = newData$Biomass/2

#Mg of carbon per hectare
print(sum(newData$Carbon/1000*100))
#THIS WILL PROBABLY UNDERESTIMATE - THE  LINEAR MODEL FOR ALL TOP 5 SPECIES HAS A HIGHER INTERCEPT (HEIGHT) THAN THE LINEAR MODEL FOR QUERCUS ALONE.
