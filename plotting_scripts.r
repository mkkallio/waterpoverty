
########################## Plotting provincial means
shades <- auto.shading(CensusData$vrppoac26, n=9)
for (i in 1:17) {
	prov <- provBorderList[i]
	provName <- provNameList[i]
	correctNameForMean <- eval(as.name(paste("villages.in.",as.character(provName),sep="") ))
	correctNameForProv <- eval(as.name(paste(prov)))
	if (i == 1) {
		plot(LaoAdmin0)
		choropleth(eval(as.name(prov)), median(correctNameForMean$vrppoac26), shading=shades, add=T)
		choro.legend(px='bottomleft', sh=shades, bg='white')
	} else {
		choropleth(eval(as.name(prov)), median(correctNameForMean$vrppoac26), shading=shades, add=T)
	}
}





######################### plot IDW (after getting ux, uy, predmat)
par(mfrow=c(1,2))
choropleth(idw.est, idw.est$var1.pred, shading=shades, pch=1)
choro.legend(px='bottomleft', sh=sh, bg='white')
plot(VientianeBorder, border=NA, col=NA) #to set the plot window extent
quick.map(vientianestats$SDF,"vrppoac26_LM", "incidence of poverty %", "GW Mean")
plot(LaoAdmin2, add=T, color=NA)




########################### quickly plot gwss variables
par(mfrow=c(2,3))
quick.map(villagestats$SDF,"vrppoac26_LM", "incidence of poverty %", "GW Mean")
quick.map(villagestats$SDF,"vrppoac26_LSD", "incidence of poverty %", "GW StDev")
quick.map(villagestats$SDF,"vrppoac26_LSKe", "incidence of poverty %", "GW Skewness")
quick.map(villagestats$SDF,"vrppoac26_LVar", "incidence of poverty %", "GW Variance")
quick.map(villagestats$SDF,"vrppoac26_LCV", "incidence of poverty %", "GW Covariance")

par(mfrow=c(2,3))
quick.map(vientianestats$SDF,"vrppoac26_LM", "incidence of poverty %", "GW Mean")
plot(LaoAdmin2, add=T, color=NA)
#quick.map(vientianestats$SDF,"vrppoac26_LSD", "incidence of poverty %", "GW StDev")
#plot(LaoAdmin2, add=T, color=NA)
#quick.map(vientianestats$SDF,"vrppoac26_LSKe", "incidence of poverty %", "GW Skewness")
#plot(LaoAdmin2, add=T, color=NA)
#quick.map(vientianestats$SDF,"vrppoac26_LVar", "incidence of poverty %", "GW Variance")
#plot(LaoAdmin2, add=T, color=NA)
#quick.map(vientianestats$SDF,"vrppoac26_LCV", "incidence of poverty %", "GW Covariance")
#plot(LaoAdmin2, add=T, color=NA)








########################## Seminar course plots
background <- readShapePoly("mapBG", proj4string = CRS ("+init=epsg:4326"))
background <- spTransform(background, CRS=CRS("+init=epsg:32648"))
gridBG <- readShapePoly("gridBG", proj4string = CRS ("+init=epsg:4326"))
gridBG <- spTransform(gridBG, CRS=CRS("+init=epsg:32648"))

VillageData_gwss <- gwss(VillageData, vars=c("vrppoac26"), bw=50000, quantile=T )
VillageData_gwss_mat <- gwss(VillageData, gridBG, vars=c("vrppoac26"), bw=50000, quantile=T )

########### Plot GWSS examples with fixed bandwidth
par(mfrow=c(1,2))
shading <- auto.shading(WPI_gwss_200km$SDF$WPI_LM, cols=rev(brewer.pal(7,"Purples")))
plot(LaoAdmin0, border=NA, col=NA)
choropleth(WPI_gwss_200km$SDF, WPI_gwss_200km$SDF$WPI_LM, shading=shading, pch=1, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading, bg='white', title="Water Poverty Index")
title("GW Mean at observation points. BW=200km")

shading <- auto.shading(WPI_gwss_200km$SDF$WPI_LVar, cols=brewer.pal(7,"Reds"))
plot(LaoAdmin0, border=NA, col=NA)
choropleth(WPI_gwss_200km$SDF, WPI_gwss_200km$SDF$WPI_LVar, shading=shading, pch=1, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading, bg='white', title="Water Poverty Index")
title("Local variance at observation points. BW=200km")

shading <- auto.shading(WPI_gwss_200km$SDF$WPI_LSD, cols=brewer.pal(7,"Reds"))
plot(LaoAdmin0, border=NA, col=NA)
choropleth(WPI_gwss_200km$SDF, WPI_gwss_200km$SDF$WPI_LSD, shading=shading, pch=1, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading, bg='white', title="Water Poverty Index")
title("GW StDev at observation points. BW=200km")

########## GWSS Surface
shading <- auto.shading(WPI_gwss_200NN_surf$SDF$WPI_LM, cols=rev(brewer.pal(7,"Purples")))
plot(LaoAdmin0, border=NA, col=NA)
choropleth(WPI_gwss_200NN_surf$SDF, WPI_gwss_200NN_surf$SDF$WPI_LM, shading=shading, pch=1, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading, bg='white', title="Water Poverty Index")
title("GW Mean at grid points. BW=200NN")

shading <- auto.shading(WPI_gwss_200NN_surf$SDF$WPI_LSD, cols=brewer.pal(7,"Reds"))
plot(LaoAdmin0, border=NA, col=NA)
choropleth(WPI_gwss_200NN_surf$SDF, WPI_gwss_200NN_surf$SDF$WPI_LSD, shading=shading, pch=1, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading, bg='white', title="Water Poverty Index")
title("GW StDev at grid points. BW=200NN")

# with contours
ux <- unique(s.grid@coords[,1])
uy <- unique(s.grid@coords[,2])

predmat <- matrix(WPI_gwss_200NN_surf$SDF$WPI_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=rev(brewer.pal(5, 'Purples')), levels=c(0,39,41,43,46,100))
plotBG()
sh <- shading(breaks=c(39,41,43,46), cols=rev(brewer.pal(5, 'Purples')))
choro.legend(px='bottomleft', sh=sh, bg='white')
title("GW Mean at grid points. BW=200NN")

predmat <- matrix(WPI_gwss_200NN_surf$SDF$WPI_LSD, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=brewer.pal(5, 'Reds'), levels=c(0,3.4,4,4.4,4.8,100))
plotBG()
sh <- shading(breaks=c(3.4,4,4.4,4.8), cols=brewer.pal(5, 'Reds'))
choro.legend(px='bottomleft', sh=sh, bg='white')
title("GW StDev at grid points. BW=200NN")


########### Plot GWSS examples with adaptive bandwidth
par(mfrow=c(1,2))
shading <- auto.shading(VillageData_gwss$SDF$vrppoac26_LM, cols=brewer.pal(7,"Purples"))
choropleth(VillageData_gwss_mat$SDF, VillageData_gwss_mat$SDF$vrppoac26_LM, shading=shading, pch=1)
plot(LaoAdmin1, add=T)
plot(background, col='white', add=T)
choro.legend(px='bottomleft', sh=shading, bg='white', title="Incidence of Poverty (%)")
title("GW Mean at observation points. BW=50NN")

shading <- auto.shading(VillageData_gwss$SDF$vrppoac26_LSD, cols=brewer.pal(7,"Reds"))
choropleth(VillageData_gwss_mat$SDF, VillageData_gwss_mat$SDF$vrppoac26_Median, shading=shading, pch=1)
plot(LaoAdmin1, add=T)
plot(background, col='white', add=T)
choro.legend(px='bottomleft', sh=shading, bg='white', title="Incidence of Poverty (%)")
title("GW StDev at observation points. BW=50NN")

########### Plot GWSS examples with different bandwidth
par(mfrow=c(1,2))
shading <- auto.shading(VillageData_gwss$SDF$vrppoac26_LM, cols=brewer.pal(7,"Purples"))
choropleth(VillageData_gwss_100km$SDF, VillageData_gwss_100km$SDF$vrppoac26_LM, shading=shading, pch=1)
plot(LaoAdmin1, add=T)
plot(background, col='white', add=T)
choro.legend(px='bottomleft', sh=shading, bg='white', title="Incidence of Poverty (%)")
title("GW Mean at observation points. BW=100km")

shading <- auto.shading(VillageData_gwss$SDF$vrppoac26_LM, cols=brewer.pal(7,"Purples"))
choropleth(VillageData_gwss_mat$SDF, VillageData_gwss_mat$SDF$vrppoac26_Median, shading=shading, pch=1)
plot(LaoAdmin1, add=T)
plot(background, col='white', add=T)
choro.legend(px='bottomleft', sh=shading, bg='white', title="Incidence of Poverty (%)")
title("GW Mean at observation points. BW=25km")


########### Plotting positive drought
shading <- auto.shading(WPI_gwr_300NN_reg$SDF$Intercept, cols = brewer.pal(5,"Blues"), n=5)
plot(LaoAdmin0, border=NA, col=NA)
choropleth(villageVoronoi, WPI_gwr_300NN_reg$SDF$Intercept, shading=shading, border=NA, add=T)
plot(mapBG, col='white', border=NA, add=T)
plot(LaoAdmin0, add=T)
choro.legend(px='bottomleft', sh=shading, title="Intercept", border=NA)
title('Intercept Coefficient, BW=300NN')

negdrought <- (WPI_gwr_300NN_reg$SDF$Drought > 0) +0 # get the villages where flood coefficient is positive
# Plot!
shading <- auto.shading(WPI_gwr_300NN_reg$SDF$Drought, cols = rev(brewer.pal(5,"Greys")), n=5)
plot(LaoAdmin0, border=NA, col=NA)
choropleth(villageVoronoi, WPI_gwr_300NN_reg$SDF$Drought, shading=shading, border=NA, add=T)
choropleth(villageVoronoi, negdrought, shading=shading(0.1, c("NA","darkred")), border=NA, add=T)
plot(mapBG, col='white', add=T)
plotBG()
title('Positive Drought Coefficients, BW=300NN')
choro.legend(px='bottomleft', sh=shading, title="Drought Coefficient", border=NA)


negdrought <- (WPI_gwr_200km_reg$SDF$Drought > 0) +0 # get the villages where flood coefficient is positive
# Plot!
shading <- auto.shading(WPI_gwr_200km_reg$SDF$Drought, cols = rev(brewer.pal(5,"Greys")), n=5)
plot(LaoAdmin0, border=NA, col=NA)
choropleth(villageVoronoi, WPI_gwr_200km_reg$SDF$Drought, shading=shading, border=NA, add=T)
choropleth(villageVoronoi, negdrought, shading=shading(0.1, c("NA","darkred")), border=NA, add=T)
plot(mapBG, col='white', add=T)
plotBG()
title('Positive Drought Coefficients, BW=200km')
choro.legend(px='bottomleft', sh=shading, title="Drought Coefficient", border=NA)

negdrought2 <- (WPI_gwr_300NN_reg$SDF$Drought > 0) +0 # get the villages where flood coefficient is positive
# Plot!
shading <- auto.shading(WPI_gwr_300NN_reg$SDF$Drought, cols = rev(brewer.pal(5,"Greys")), n=5)
plot(LaoAdmin0, border=NA, col=NA)
choropleth(villageVoronoi, WPI_gwr_300NN_reg$SDF$Drought, shading=shading, border=NA, add=T)
choropleth(villageVoronoi, negdrought2, shading=shading(0.1, c("NA","darkred")), border=NA, add=T)
plot(mapBG, col='white', add=T)
plotBG()
title('Positive Drought Coefficients, BW=300NN')
choro.legend(px='bottomleft', sh=shading, title="Drought Coefficient", border=NA)

########## Plotting GWR

# Total
shading <- auto.shading(WPI_gwr_200km_reg$SDF$yhat, cols = rev(brewer.pal(5,"Purples")), n=5)
ux <- unique(coordinates(s.grid) [,1] )
uy <- unique(coordinates(s.grid) [,2] )
predmat <- matrix(house.gwr$SDF$yhat, length(ux), length(uy) )


choropleth(villageVoronoi, WPI_gwr_200km_reg$SDF$yhat, shading=shading, border=NA, add=T)
plot(mapBG, col='white', border=NA, add=T)
plot(LaoAdmin0, add=T)
choro.legend(px='bottomleft', sh=shading, title="WPI", border=NA)
title('Predicted Water Poverty Index')

shading <- auto.shading(WPI_gwr_200km_reg$SDF$residual, cols = brewer.pal(5,"RdYlBu"), n=5)
plot(LaoAdmin0, border=NA, col=NA)
choropleth(villageVoronoi, WPI_gwr_200km_reg$SDF$residual, shading=shading, border=NA, add=T)
plot(mapBG, col='white', border=NA, add=T)
plot(LaoAdmin0, add=T)
choro.legend(px='bottomleft', sh=shading, title="WPI", border=NA)
title('Residuals')



#################### PLOTTING WPI
par(mfrow=c(2,3))
# Resources
shading <- auto.shading(WPIdata$RES, cols = brewer.pal(9,"Blues"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(villageVoronoi, WPIdata$RES, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('WPI component Resources')

# Access
shading <- auto.shading(WPIdata$ACC, cols = brewer.pal(9,"Greens"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(villageVoronoi, WPIdata$ACC, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('WPI component Access')

# Capacity
shading <- auto.shading(WPIdata$CAP, cols = brewer.pal(9,"Purples"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(villageVoronoi, WPIdata$CAP, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('WPI component Capacity')

# Use
shading <- auto.shading(WPIdata$USE, cols = brewer.pal(9,"Oranges"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(villageVoronoi, WPIdata$USE, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('WPI component Use')

# Environment
shading <- auto.shading(WPIdata$ENV, cols = brewer.pal(9,"Greys"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(villageVoronoi, WPIdata$ENV, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('WPI component Environment')

# Total
shading <- auto.shading(WPIdata$WPI, cols = brewer.pal(9,"Reds"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(villageVoronoi, WPIdata$WPI, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Water Poverty Index')



#BOXPLOTS


temp <- as.data.frame(wetWPIcomp)
temp <- fortify(temp)
temp <- melt(wetWPIcomp)
ggplot() + geom_boxplot(data = temp, aes(X2, value, fill = factor(X2)))