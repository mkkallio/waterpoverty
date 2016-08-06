###################################
# Poverty in Laos
#################################


########################## GWSS for the entire Census dataset
VillageData_gwss <- gwss(VillageData, vars=c("vrppoac26"), bw=50000, quantile=T )

########################## GWSS with quantiles for every province
for (i in 1:17) {
	provName <- provNameList[i]
	CorrectName <- eval(as.name(paste("villages.prov.",as.character(provName),sep="") ))
	assign(paste("province.gwss.",provName, sep=""), gwss(CorrectName, vars=c("vrppoac26"), bw=15000, quantile=T)) #BW = 15km
}

############ GWSS with quantiles for every district
for (i in 1:length(distrCode) {
	dCode <- distrCode[i]
	CorrectName <- eval(as.name(paste("villages.distr.",as.character(dCode),sep="") ))
	assign(paste("district.gwss.",dCode, sep=""), gwss(CorrectName, vars=c("vrppoac26"), bw=5000, quantile=T)) #BW = 5km
}




################## Do interpolation with IDW
villageVoronoi <- voronoipolygons(villages.prov.Savannakhet) #create voronoi polygons from ppoint data
proj4string(villageVoronoi) <- CRS("+init=epsg:32648") # correct coordinate system
s.grid <- spsample(villageVoronoi, type='regular', n=10000) # create sample grd
idw.est <- gstat::idw(vrppoac26~1,villages.prov.Savannakhet, newdata=s.grid, idp=2 ) #interpolate

################### do this prior to plotting
ux <- unique(coordinates(idw.est) [,1] )
uy <- unique(coordinates(idw.est) [,2] )
predmat <- matrix(idw.est$var1.pred, length(ux), length(uy) )

gwr.res <- gwr.basic(vrppoac26~ Slopeclass + Elev5minme + A_wcdenja0, data=VillageData, bw=25000, kernel='gaussian')



##################### VillageData factors -> num

for (i in 1:length(ncol(VillageData@data))) {
	colname <- colnames(VillageData@data[,i])
	if (is.factor(VillageData$colname)) {
		assign(VillageData$colname, as.numeric(VillageData$colname))
	}
}
bw.pca <- bw.gwpca(VillageData, vars=colnames(VillageData@data), k = 5)
VillageData_gwpca <- gwpca(VillageData, vars=colnames(VillageData@data), k = 5, bw=50000)



################### GW stats 

# first get the voronoi polygons
villageVoronoi <- voronoipolygons(WPIdata) #create voronoi polygons from ppoint data
proj4string(villageVoronoi) <- CRS("+init=epsg:32648") # correct coordinate system
s.grid <- spsample(villageVoronoi, type='regular', n=1000) # create sample grd

# Calculate GWSS for two bandwidths 200km and 50km
WPI_gwss_200km <- gwss(WPIdata, vars=c("WPI"), bw=200000, quantile=T )


#Calculate GWSS surface
WPI_gwss_200km_surf <- gwss(WPIdata, summary.locat=s.grid, vars=c("WPI"), bw=200000, quantile=T )
# why does it not work??

#calculate GWSS surface for adaptive bandwidth 
WPI_gwss_200NN_surf <- gwss(WPIdata, s.grid, vars=c("WPI"), bw=200, adaptive=T, quantile=T)



##################### GW regression
distanceMatrix <- gw.dist(dp.locat = cbind(as.numeric(WPIdata_orig@coords[,1]), as.numeric(WPIdata_orig@coords[,2])))
WPI_gwr_200km_reg <- gwr.basic(WPI~ Disaster+Flood+Drought+LandType+SoilDeg+IncPov+TotalPop+PopElec+LitPopSh+NormRF3a+StartRF3a+AmountRF3a, data=WPIdata_orig, bw=200000, kernel='bisquare', dMat=distanceMatrix)
WPI_gwr_300NN_reg <- gwr.basic(WPI~ Disaster+Flood+Drought+LandType+SoilDeg+IncPov+TotalPop+PopElec+LitPopSh+NormRF3a+StartRF3a+AmountRF3a, data=WPIdata_orig, bw=300, adaptive=T, kernel='bisquare', dMat=distanceMatrix)

# Get optimal GWR bandwidth
WPI_gwr_optBW <- bw.ggwr(WPI~ DrySurf+WetSurf+DrySoil+WetSoil+DryPrec+WetPrec+Irrigation+DrinkNS+ToiletType+TimeDisCap+TimeProCap+RoadAcc+LitPopSh+IncPov+IrrAreaSh+AgAreaPerC+AgAqDepend+DomWatUse+ECO_V_cat+Disaster+SoilDeg, data=WPIdata_orig, family='poisson', kernel='bisquare', dMat=distanceMatrix) # does not finish because of multicollinearity (?)


WPI_gwr_200km_reg <- gwr.basic(WPI~ DrySurf+WetSurf+DrySoil+WetSoil+DryPrec+WetPrec+Irrigation+DrinkNS+ToiletType+TimeDisCap+TimeProCap+RoadAcc+LitPopSh+IncPov+IrrAreaSh+AgAreaPerC+AgAqDepend+DomWatUse+ECO_V_cat+Disaster+SoilDeg, data=WPIdata_scaled, bw=200000, kernel='bisquare', dMat=distanceMatrix)

##################### GW PCA

#WPIdata_scaled <- WPIdata_orig
#WPIdata_scaled@data <- scale(WPIdata_orig@data[,c("Disaster","Flood","Drought","LandType","SoilDeg","IncPov","TotalPop","PopElec","LitPopSh","NormRF3a","StartRF3a","AmountRF3a")])

# Scale data
scaled_data <- scale(WPIdata@data[,c("DryAvail", "WetAvail", "DrySoil", "WetSoil", "Irrigation", "DrinkNS", "ToiletType", "TimeCap", "RoadAcc", "LitPopSh", "IncPov", "IrrAreaSh", "AgAreaPerC", "AgAqDepend", "ECO_V_cat", "Disaster", "SoilDeg")])
WPIdata_scaled <- WPIdata
WPIdata_scaled@data <- data.frame(scaled_data)


scaled_data2 <- scale(WPIdata@data[,c("RES", "ACC", "CAP", "USE", "ENV")])
WPIdata_scaled@data <- data.frame(scaled_data2)




dryWPI_gwpca_400NN <- gwpca(WPIdata, vars=c("DryAvail", "DrySoil","AvMaxDDay", "Irrigation", "DrinkNS", "ToiletType", "TimeCap", "DryRoad", "LitPopSh", "IncPov", "IrrAreaSh", "AgAreaPerC", "AgAqDepend", "ECO_V_cat", "dryDisast", "SoilDeg"), bw=400, adaptive=T, k=10, dMat=distMat)

wetWPI_gwpca_400NN <- gwpca(WPIdata, vars=c("WetAvail", "WetSoil", "Irrigation", "DrinkNS", "ToiletType", "TimeCap", "WetRoad", "LitPopSh", "IncPov", "IrrAreaSh", "AgAreaPerC", "AgAqDepend", "ECO_V_cat", "wetDisast", "SoilDeg"), bw=400, adaptive=T, k=10, dMat=distMat)


# plot lead item
local.loadings1 <- WPI_gwpca_200km$loadings[,,1] 
local.loadings2 <- WPI_gwpca_200km$loadings[,,2]
local.loadings3 <- WPI_gwpca_200km$loadings[,,3]
lead.item1 <- colnames(local.loadings1)[max.col(abs(local.loadings1))]
lead.item2 <- colnames(local.loadings2)[max.col(abs(local.loadings2))]
lead.item3 <- colnames(local.loadings3)[max.col(abs(local.loadings3))]
df <- SpatialPolygonsDataFrame(villageVoronoi, data=data.frame(lead.item1, lead.item2, lead.item3))

par(mfrow=c(2,3))
# lead item in PC1
#colour1 <- brewer.pal(5,"Set1")[match(df$lead.item1,unique(df$lead.item1))]
plot(df, col=colour1, border=NA)
plotBG()
#legend('bottomleft',as.character(unique(lead.item2)),pch=15, cex=1,col=brewer.pal(5,"Set2"))
title("'Winning' variable, PC1")

# lead item in PC2
#colour2 <- brewer.pal(5,"Set2")[match(df$lead.item2,unique(df$lead.item2))]
plot(df, col=colour2, border=NA)
plotBG()
#legend('bottomleft',as.character(unique(lead.item2)),pch=15, cex=1,col=brewer.pal(5,"Set2"))
title("'Winning' variable, PC2")

# lead item in PC3
#colour3 <- brewer.pal(5,"Set3")[match(df$lead.item3,unique(df$lead.item3))]
plot(df, col=colour3, border=NA)
plotBG()
#legend('bottomleft',as.character(unique(lead.item2)),pch=15, cex=1,col=brewer.pal(5,"Set2"))
title("'Winning' variable, PC3")

barplot(table(df@data[,1]),ylab="Freqency",las=2, col=c("#FC8D62", "#A6D854", "#E78AC3", "#8DA0CB", "#66C2A5"))
barplot(table(df@data[,2]),ylab="Freqency",las=2, col=c("#FC8D62", "#A6D854", "#E78AC3", "#8DA0CB", "#66C2A5"))
barplot(table(df@data[,3]),ylab="Freqency",las=2, col=c("#FC8D62", "#A6D854", "#E78AC3", "#8DA0CB", "#66C2A5"))

barplot(table(df@data[,1]),main="'Winning' variable",ylab="Freqency",las=2, col=c("#E6AB02", "#D95F02", "#7570B3", "#666666", "#66A61E","#E7298A" , "#A6761D","#1B9E77"))

# plot variation
props <- prop.var(WPI_gwpca_200km,2)
summary(props)
interval <- cut(props,c(0.5,0.54,0.58,0.62),labels=FALSE) # note first two component only - would need to explore all components..
df2 = SpatialPolygonsDataFrame(villageVoronoi, data.frame(int=interval))
colour <- brewer.pal(4,"Blues")[-1][interval]
plot(df2,col=colour, border=NA)
plotBG()
interval.labels <- c("Below 0.54","0.54 up to 0.58","0.58 and above")
legend('bottomleft',interval.labels,pch=18,col=brewer.pal(4,"Blues")[-1])
title('Variability Explained by PC1 and PC2')


# Glyph plot
subsample <- sample(8100, 200)
coords <- coordinates(df)
plot(LaoAdmin0)
glyph.plot( local.loadings1[subsample,], coords[subsample,], add=T, r1=25)
plotBG()
title('PC1 Variable Loadings')





########################### GWR NEW
names <- colnames(WPIdata_orig@data)[9:75]
newnames <- vector()
for (i in 1:length(names)) {
	newnames[i] <- paste('WPIdata_orig$',names[i], sep='')
}

# Select model variables automatically. using only a sample size of 2000 (out of 8215) because of memory problems. 

sample <- sample(8215, 2000)
WPIdata_sample <- WPIdata_orig[sample,]
distanceMatrix_sample <-  gw.dist(dp.locat = cbind(as.numeric(WPIdata_sample@coords[,1]), as.numeric(WPIdata_sample@coords[,2])))

names <- colnames(WPIdata_sample@data)[9:75]
for (i in 1:length(names)) {
	newnames[i] <- paste("WPIdata_sample$",names[i],sep="")
}
# newnames <- newnames[-c(53,56,54,12,13,14,15,16,17,18,19,34,35,36,37,38,39,33)]
# names <- names[-c(53,56,54,12,13,14,15,16,17,18,19,34,35,36,37,38,39,33)]

modelSelection1 <- model.selection.gwr(DeVar="WPIpca",InDeVars=newnames, data=WPIdata_sample,bw=gwr.bw,approach="CV", adaptive=F,kernel="gaussian",dMat=distanceMatrix_sample,p=2, theta=0, longlat=F)


WPI.gwr <- gwr.basic(WPIpca ~ IncPov + PopDepCrop + TimeProCap + SoilDeg + LitPopSh + WetSurf + ECO_V_cat + TotalPop + Pests + TotAgrArea + IrrAreaSh + Landslide + WetPrec + RoadAcc + Drought + AvMaxDDay + DrySurf + OtherDis + TimeDisCap + Flood + ShDryIrr + IrrPermWei + WatSupp + Irrigation + IrrOther + ToiletType + TotIrrArea + TWalkHC + AmountRF3a + PopElec + DryPrec + PopDepAqua + IrrGabion, data = WPIdata_orig, dMat = distanceMatrix, bw=gwr.bw)





# Model selection doesnt finish because too many things. Therefore we need to do it with a script or a *function*
selectModel <- function(container, variable, list, bandwidth, distmatrix) {
	#variables <- as.list(colnames(data@data))
		AICs <- vector()
	for (i in 1:length(list)) {
		value <- gwr.basic(variable ~ as.character(list[i]), data = container, bw = bandwidth, dMat = distmatrix)
		
		AICs[i] <- value$GW.diagnostic$AICc
	}
	
	df <- data.frame(cbind(list, AICs))as.character
	maxAIC <- which(df$AICs == max(AICs))
	print(df)
	return(df)
}

