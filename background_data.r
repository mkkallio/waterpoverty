#######################################################################
#################### create the village voronoi dataset and other helping things
#######################################################################

# voronoi polygons and a sample grid
voronoi <- voronoipolygons(WPIdata)
proj4string(voronoi) <- CRS("+init=epsg:32648")
s.grid <- spsample(voronoi, type='regular', n=1000) # create sample grd
voronoi <- gIntersection(LaoAdmin0,voronoi, byid=T)

#create fortified voronoi for plotting with ggplot2
pid <- sapply(slot(voronoi, "polygons"), function(x) slot(x, "ID"))
p.df <- data.frame( ID=WPIdata$wid, dryWPIgpca=WPIdata$dryWPIgpca, row.names = pid)
voronoi <- SpatialPolygonsDataFrame(voronoi, data=as.data.frame(p.df))
voronoi.f <- fortify(voronoi, region='ID')
rm('pid','p.df')

mapBG.f <- fortify(mapBG)


# fortify admin borders
LaoAdmin0.f <- fortify(LaoAdmin0)
LaoAdmin1.f <- fortify(LaoAdmin1)
LaoAdmin2.f <- fortify(LaoAdmin2)


sample <- sample(8215,2000)
# create a sample
WPIdata_sample <- WPIdata_orig[sample,]
WPIdata_sample$dryWPIgpca <- WPIdata$dryWPIgpca[sample]
WPIdata_sample$wetWPIgpca <- WPIdata$wetWPIgpca[sample]

# load precalculated distMatrices, or calculate them with the following commands. Warning: May take a long time to finish.
load("distMat.RData")
#distMat <- gw.dist(dp.locat = cbind(as.numeric(WPIdata_orig@coords[,1]), as.numeric(WPIdata_orig@coords[,2])))
#distMat.grid <- gw.dist(dp.locat = cbind(as.numeric(WPIdata_orig@coords[,1]), as.numeric(WPIdata_orig@coords[,2])), rp.locat = s.grid@coords)
#distMat.sample <-  gw.dist(dp.locat = cbind(as.numeric(WPIdata_sample@coords[,1]), as.numeric(WPIdata_sample@coords[,2])))


# collect components
dryRESvar <- cbind(WPIdata$DryAvail, WPIdata$DryPrec, WPIdata$AvMaxDDay)
colnames(dryRESvar) <- c('DryAvail','DryPrec','AvMaxDDay')
wetRESvar <- cbind(WPIdata$WetAvail, WPIdata$WetPrec)
colnames(wetRESvar) <- c('WetAvail','WetPrec')
ACCvar <- cbind(WPIdata$Irrigation, WPIdata$DrinkNS, WPIdata$ToiletType)
colnames(ACCvar) <- c('Irrigation','WaterSource','ToiletType')
dryCAPvar <- cbind(WPIdata$TimeCap, WPIdata$DryRoad, WPIdata$LitPopSh, WPIdata$IncPov)
colnames(dryCAPvar) <- c('TravelTimeToCap','DryRoadAccess','LiteracyRate','IncidenceOfPoverty')
wetCAPvar <- cbind(WPIdata$TimeCap, WPIdata$WetRoad, WPIdata$LitPopSh, WPIdata$IncPov)
colnames(wetCAPvar) <- c('TravelTimeToCap','WetRoadAccess','LiteracyRate','IncidenceOfPoverty')
dryUSEvar <- cbind(WPIdata$ShDryIrr, WPIdata$AgAreaPerC, WPIdata$AgAqDepend)
colnames(dryUSEvar) <- c('DryIrrigation','AgrAreaPerCapita','PopDependingWater')
wetUSEvar <- cbind(WPIdata$IrrAreaSh, WPIdata$AgAreaPerC, WPIdata$AgAqDepend)
colnames(wetUSEvar) <- c('WetIrrigation','AgrAreaPerCapita','PopDependingWater')
dryENVvar <- cbind(WPIdata$ECO_V_cat, WPIdata$dryDisast, WPIdata$SoilDeg, WPIdata$HumanFP)
colnames(dryENVvar) <- c('ThreatenedAmphibians','DryDisasters','SoilDegradation','HumanFootprint')
wetENVvar <- cbind(WPIdata$ECO_V_cat, WPIdata$wetDisast, WPIdata$SoilDeg, WPIdata$HumanFP)
colnames(wetENVvar) <- c('ThreatenedAmphibians','WetDisasters','SoilDegradation','HumanFootprint')

dryVAR <- data.frame(WPIdata$dryWPIgpca, dryRESvar, ACCvar, dryCAPvar, dryUSEvar, dryENVvar)
colnames(dryVAR) <- c('DryWPI','DryAvail','DryPrec','AvMaxDDay', 'Irrigation','DrinkingWaterSource','ToiletType', 'TravelTimeToCap','DryRoadAccess','LiteracyRate','IncidenceOfPoverty', 'DryIrrigation','AgrAreaPerCapita','PopDependingWater', 'ThreatenedAmphibians','DryDisasters','SoilDegradation','HumanFootprint')
wetVAR <- data.frame(WPIdata$wetWPIgpca, wetRESvar, ACCvar, wetCAPvar, wetUSEvar, wetENVvar)
colnames(wetVAR) <- c('WetWPI','WetAvail','WetPrec', 'Irrigation','DrinkingWaterSource','ToiletType', 'TravelTimeToCap','WetRoadAccess','LiteracyRate','IncidenceOfPoverty', 'WetIrrigation','AgrAreaPerCapita','PopDependingWater', 'ThreatenedAmphibians','WetDisasters','SoilDegradation','HumanFootprint')



# province and district names
tempnames <- data.frame(WPIdata$bcne, WPIdata$tcne, WPIdata$wcne)
Provinces <- as.character(tempnames[,1])
Districts <- as.character(tempnames[,2])
Villages <- as.character(tempnames[,3])
rm('tempnames')


# create a listw object to use with spdep package
voronoi.nb.listw <- nb2listw(poly2nb(voronoi) )


