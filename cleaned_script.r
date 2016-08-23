###################################
# Poverty in Laos
#################################

##################### Load libraries
library(rgdal)
library(sp)
library(GISTools)
library(spdep)
library(ggplot2)
library(raster)
library(GWmodel)
library(deldir)
library(classInt)
library(DescTools)
library(reshape)
library(cluster)
library(plotly)


####################################################################
####################### Load data 
####################################################################

# Admin borders
LaoAdmin0_WGS84 <- readShapePoly("admin areas/LAO_adm0", proj4string = CRS("+init=epsg:4326") ) # country border
LaoAdmin1_WGS84 <- readShapePoly("admin areas/LAO_adm1_correct_names_no_xaisomboun", proj4string = CRS("+init=epsg:4326")) # province border

####################### transform coordinates to UTM 48N
LaoAdmin0 <- spTransform(LaoAdmin0_WGS84, CRS=CRS("+init=epsg:32648"))
LaoAdmin1 <- spTransform(LaoAdmin1_WGS84, CRS=CRS("+init=epsg:32648"))

rm('LaoAdmin0_WGS84','LaoAdmin1_WGS84','LaoAdmin2_WGS84')

# Map background which is used in the contour plots
mapBG <- readShapePoly("data/MapBG", proj4string = CRS("+init=epsg:4326") ) # background
mapBG <- spTransform(mapBG, CRS=CRS("+init=epsg:32648"))


####################### Load village data
WPIdata <- readShapePoints("data/WPIdata2", proj4string=CRS("+init=epsg:32648")) # to process
WPIdata_orig <- readShapePoints("data/WPIdata2", proj4string=CRS("+init=epsg:32648")) # to keep the original values



##########################################
#### SOURCE other .r files
#########################################

# Auxiliary functions
source('aux_functions.r', encoding = 'UTF-8')

# preprocess data in a separate .r file to keep the length of this file shorter
source('preprocess.r', encoding = 'UTF-8')

# calculate WPI
source('calculate_components.r', encoding = 'UTF-8')

# create helping objects
source('background_data.r', encoding = 'UTF-8')











##########################################
# Chapter 2
###########################


# Plot DEM
alt <- getData('alt', country='LAO')
slope <- terrain(alt, opt='slope')
aspect <- terrain(alt,opt='aspect')
hill <- hillShade(slope,aspect,40,270)
hill <- data.frame(rasterToPoints(hill))
alt <- data.frame(rasterToPoints(alt))
ggplot(hill,aes(x=x,y=y))+
    geom_raster(data=alt,aes(fill=LAO_msk_alt),alpha=0.75)+
    geom_raster(aes(alpha=1-layer),fill="gray20")+
    scale_alpha(guide=FALSE,range = c(0,1.00))+
    scale_fill_gradientn(name="Altitude",colours = terrain.colors(100))+
    theme_bw()+coord_equal()+xlab("Longitude")+ylab("Latitude")+ggtitle('Digital Elevation Map of Laos')
rm('alt','slope','aspect','hill')





# Plot provinces and districts
temp <- gCentroid(LaoAdmin1,byid=TRUE)
temp <- data.frame(temp@coords[,1], temp@coords[,2], LaoAdmin1$NAME_1)
colnames(temp) <- c('x','y','name')

bmap <- ggplot(LaoAdmin2.f, aes(x = long, y = lat)) + geom_polygon(aes(group = group), colour = I("grey90"), size=0.2, fill = "white") + coord_equal() 
bmap <- bmap + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = I("grey30"), size=0.2, fill = NA) + coord_equal() 
bmap <- bmap + geom_text(data=temp, aes(x = x,y = y, label=name))+ theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
bmap










##########################################
# Chapter 5 - initial dataset exploration
###########################


# plot village numbers per province (FIG 5.1)
temp <- table(WPIdata$bcne)
temp <- as.data.frame(temp)
ggplot(temp) + geom_bar(aes(Var1,Freq), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Number of Villages')
rm('temp')



# plot dot density map of the villages (FIG 5.2)
bmap <- ggplot(LaoAdmin1.f, aes(x = long, y = lat)) + geom_polygon(aes(group = group), colour = I("grey65"), size=0.2, fill = "white") + coord_equal() 
map <- bmap + geom_point(data=as.data.frame(coordinates(WPIdata)), aes(x=coords.x1,y=coords.x2, fill=factor(Provinces), colour=factor(Provinces)), size=0.8)+ theme_minimal() + theme(axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none")

dtop <- ggplot()+geom_line(aes(as.data.frame(coordinates(WPIdata)[,1])), stat='density')+ theme_minimal() + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

dright <- ggplot()+geom_line(aes(as.data.frame(coordinates(WPIdata)[,1])), stat='density') + coord_flip()+ theme_minimal() + theme(axis.line=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

empty <- ggplot()+geom_point(aes(1,1), colour="white") + theme(axis.ticks=element_blank(), panel.background=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

grid.arrange(dtop, empty, map, dright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

rm('bmap','map', 'dtop','dright','empty')




# Explore Resources
# dry season

# Scatterplot 
temp <- as.data.frame(dryRESvar)
colnames(temp) <- c('Dry Season \n Surface Availability','Dry Season \n Precipitation','Consecutive Drought Days') 
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# Boxplot
temp <- melt(temp)
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('a) Dry Season RES')



# wet season

# Scatterplot
temp <- as.data.frame(cbind(wetRESvar, rep.int(100,8215)))
colnames(temp) <- c('Wet Season \n Surface Availability','Wet Season \n Precipitation','Consecutive Drought Days') 
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# Boxplot
temp <- melt(temp)
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('b) Wet Season RES') 
multiplot(p1,p2, cols=2)


# calculate global Moran's I
#tempsample <- sample(8215,6000)
temp <- componentMoran2(dryRESvar, voronoi.nb.listw)
temp2 <- componentMoran2(wetRESvar, voronoi.nb.listw)
moranTable <- data.frame(rbind(temp, temp2))
#rownames(moranRES) <- c('DryAvail','DryPrec','AvMaxDDay','WetAvail','WetPrec')
rm('temp','temp2','p1','p2')



# plot villages with less than 100 score on surface water availability
selection <- WPIdata$DryAvail < 100
selection.f <- fortify(voronoi[selection,], region='ID')
p1 <- ggplot(LaoAdmin1.f, aes(x = long, y = lat)) + geom_polygon(aes(group = group), colour = I("grey65"), size=0.2, fill = "white") + coord_equal() + geom_polygon(data=selection.f, aes(group=group), fill="red") + theme_bw() + ggtitle('a) Water Scarcity \n in the Dry Season') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())

selection <- WPIdata$WetAvail < 100
selection.f <- fortify(voronoi[selection,], region='ID')
p2 <- ggplot(LaoAdmin1.f, aes(x = long, y = lat)) + geom_polygon(aes(group = group), colour = I("grey65"), size=0.2, fill = "white") + coord_equal() + geom_polygon(data=selection.f, aes(group=group), fill="orange") + theme_bw() + ggtitle('b) Water Scarcity \n in the Wet Season') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())

multiplot(p1,p2,cols=2)

rm('selection','selection.f','p1','p2')






#Explore Access

# Scatterplot
temp <- as.data.frame(ACCvar)
colnames(temp) <- c('Irrigation Type','Drinking Water \n Source','Toilet Type')
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth) 

# Boxplot
temp <- melt(temp)
ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('ACC')


# calculate global Moran's I
temp <- componentMoran2(ACCvar, voronoi.nb.listw)
moranTable <- data.frame(temp)









#Explore Capacity

# Scatterplot
temp <- as.data.frame(dryCAPvar)
colnames(temp) <- c('Travel Time to Capital','Road Access','Literacy Rate','Incidence of Poverty') 
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# Boxplot
temp <- melt(temp)
ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('Dry Season CAP')

# calculate global Moran's I
temp <- componentMoran2(dryCAPvar, voronoi.nb.listw)
temp2 <- componentMoran2(wetCAPvar, voronoi.nb.listw)
moranTable <- data.frame(rbind(temp, temp2))
rm('temp','temp2')

# plot villages with less than 100 score on road access
selection <- WPIdata$DryRoad < 100
selection.f <- fortify(voronoi[selection,], region='ID')
p1 <- ggplot(LaoAdmin1.f, aes(x = long, y = lat)) + geom_polygon(aes(group = group), colour = I("grey65"), size=0.2, fill = "white") + coord_equal() + geom_polygon(data=selection.f, aes(group=group), fill="red") + theme_bw() + ggtitle('a) Villages without road access in the dry season')

selection <- WPIdata$WetRoad < 100
selection.f <- fortify(voronoi[selection,], region='ID')
p2 <- ggplot(LaoAdmin1.f, aes(x = long, y = lat)) + geom_polygon(aes(group = group), colour = I("grey65"), size=0.2, fill = "white") + coord_equal() + geom_polygon(data=selection.f, aes(group=group), fill="orange") + theme_bw() + ggtitle('b) Villages without road access in the wet season')

multiplot(p1,p2,cols=2)

rm('selection','selection.f','p1','p2')









#Explore Use

#Scatterplot
temp <- as.data.frame(dryUSEvar)
colnames(temp) <- c('Irrigation Rate','Agr. Area per Capita','Pop. Rate Depending on Water') 
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# Boxplot
temp <- melt(temp)
ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('Dry Season USE')

# plot dry vs wet irrigation
temp <- as.data.frame(cbind(WPIdata$IrrAreaSh, WPIdata$ShDryIrr))
colnames(temp) <- c('WetIrrigation','DryIrrigation')
p1 <- ggplot(data = temp) + geom_point(aes(WetIrrigation, DryIrrigation)) + geom_abline(color='red') + theme_bw() + ggtitle('Seasonal Irrigation')
temp <- melt(temp)
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('Irrigation Scores')
multiplot(p1,p2, cols=2)


# calculate global Moran's I
temp <- componentMoran2(dryUSEvar, voronoi.nb.listw)
temp2 <- componentMoran2(wetUSEvar, voronoi.nb.listw)
moranTable <- data.frame(rbind(temp, temp2))
rm('temp','p1','p2')








#Explore Environment
# dry season

# Scatterplot
temp <- as.data.frame(dryENVvar)
colnames(temp) <- c('Threatened \n Amphibians','Disasters','Soil \n Degradation','Human \n Footprint') 
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# Boxplot
temp <- melt(temp)
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('a) Dry Season ENV')


# wet season

# Scatterplot
temp <- as.data.frame(cbind(WPIdata$dryDisast, WPIdata$wetDisast))
colnames(temp) <- c('Dry season \n disasters','Wet season \n disasters')
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

#Boxplot
temp <- melt(temp)
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('b) Dry and Wet Season Disasters')
multiplot(p1,p2,cols=2)

# calculate global Moran's I
temp <- componentMoran2(dryENVvar, voronoi.nb.listw)
temp2 <- componentMoran2(wetENVvar, voronoi.nb.listw)
moranTable <- data.frame(rbind(temp, temp2))
rm('temp','temp2', 'p1', 'p2')








###########################################################################
############## Chapter 5 - Explore spatial differences in WPI
###########################################################################

# Local statistics (geographically weighted summary statistics) on a grid of 1000 points

# on components
dryWPI_gwss_400NN_comp <- gwss(WPIdata, s.grid, vars=c("dryWPIgpca", "dryRES", "dryACC", "dryCAP", "dryUSE", "dryENV"), bw=400, adaptive=T, quantile=F, dMat=distMat.grid)
wetWPI_gwss_400NN_comp <- gwss(WPIdata, s.grid, vars=c("wetWPIgpca", "wetRES", "wetACC", "wetCAP", "wetUSE", "wetENV"), bw=400, adaptive=T, quantile=F, dMat=distMat.grid)

# on variables
dryWPI_gwss_400NN_vars <- gwss(WPIdata, s.grid, vars=c("dryWPIgpca", "DryAvail","AvMaxDDay", "Irrigation", "DrinkNS", "ToiletType", "TimeCap", "DryRoad", "LitPopSh", "IncPov", "IrrAreaSh", "AgAreaPerC", "AgAqDepend", "ECO_V_cat", "dryDisast", "SoilDeg","HumanFP"), bw=400, adaptive=T, quantile=F, dMat=distMat.grid)
wetWPI_gwss_400NN_vars <- gwss(WPIdata, s.grid, vars=c("wetWPIgpca", "WetAvail", "Irrigation", "DrinkNS", "ToiletType", "TimeCap", "WetRoad", "LitPopSh", "IncPov", "IrrAreaSh", "AgAreaPerC", "AgAqDepend", "ECO_V_cat", "wetDisast", "SoilDeg", "HumanFP"), bw=400, adaptive=T, quantile=F, dMat=distMat.grid)



# Plot mean components (FIG 5.13)
par(mfrow=c(1,2), mar=c(1,3,1,1))
#RES
breaks <- classIntervals(dryWPI_gwss_400NN_comp$SDF$dryRES_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Blues')
# dry LM
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryRES_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("a) Dry season RES")

#ACC
breaks <- classIntervals(dryWPI_gwss_400NN_comp$SDF$dryACC_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Greens')
# dry LM
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryACC_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("b) Dry season ACC")

#CAP
breaks <- classIntervals(dryWPI_gwss_400NN_comp$SDF$dryCAP_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Purples')
# dry LM
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryCAP_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("c) Dry season CAP")

#USE
breaks <- classIntervals(dryWPI_gwss_400NN_comp$SDF$dryUSE_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Oranges')
# dry LM
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryUSE_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("d) Dry season USE")

#ENV
breaks <- classIntervals(dryWPI_gwss_400NN_comp$SDF$dryENV_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Greys')
# dry LM
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryENV_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("e) Dry season ENV")

#Total
breaks <- classIntervals(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Reds')
# dry LM
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("f) Dry season WPI")

rm(breaks, colours, predmat, sh)





###### Plot dry season ranks (FIG 5.15)
temp <- data.frame(WPIdata$wid, rank(WPIdata$dryWPIgpca))
colnames(temp) <- c('id','Rank')
View(temp)
temp2 <- merge(voronoi.f,temp, by='id')

map <- ggplot(temp2, aes(x = long, y = lat, fill = Rank)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low='red', mid='yellow', high='blue', midpoint=4108) + theme_bw()
map <- map + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Dry season WPI ranks') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
map

rm('temp', 'temp2','map')




# Calculate Moran's I 
temp <- componentMoran2(dryWPIcomp, voronoi.nb.listw)
moranTable <- data.frame(temp)
rm('temp','moranTable')



# Plot WPI density curves for dry season (FIG 5.14)
temp <- table(WPIdata$bcne)
temp <- as.data.frame(temp)
tempnames <- as.character(temp[,"Var1"])
provinceWPIsummary <- vector()
provinceWPIplot <- rep(NA,1000)
for (i in 1:length(tempnames)) {
	temppro <- tempnames[i]
	tempdry <- summary(rank(WPIdata$dryWPIgpca)[WPIdata$bcne==temppro])
	provinceWPIsummary <- rbind(provinceWPIsummary, c(as.character(temppro), tempdry))
	
	tempdry <- c(WPIdata$dryWPIgpca[WPIdata$bcne==temppro], rep(NA, 1000 - length(WPIdata$dryWPIgpca[WPIdata$bcne==temppro])))
	provinceWPIplot <- data.frame(provinceWPIplot, tempdry)
}
provinceWPIsummary <- as.data.frame(provinceWPIsummary)
provinceWPIplot <- provinceWPIplot[,-1]
colnames(provinceWPIplot) <- tempnames
rm('temp','tempnames','temppro','tempdry')

temp <- melt(provinceWPIplot)

p1 <- ggplot(temp, aes(value, fill=variable, colour = variable)) +  geom_density(position='stack', alpha=0.5) + ggtitle('Provincial WPI densities') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + labs(x='WPI')
p2 <- ggplot(temp, aes(value, colour = variable)) +  geom_density() + ggtitle('Provincial WPI densities') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + labs(x='WPI')

multiplot(p1,p2,cols=2)

rm('p1','p2','temp')
#ggplot(provinceWPI2, aes(x = Province, xend=Province, y = DryWPI, yend=WetWPI, fill=Province)) + geom_segment() + geom_point(aes(x=Province, y=DryWPI, color='blue'), size=3) + geom_point(aes(x=Province, y=WetWPI, color='red'), size=3) + ggtitle('Season WPI differences') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y= 'WPI', x='')




# perform an anova test for the mean WPI values
temp <- pairwise.t.test(WPIdata$dryWPIgpca,Provinces,  p.adj = "none")


###### compute and plot local Moran's I
temp <- localmoran(dryWPIcomp[,1], voronoi.nb.listw)
temp2 <- data.frame(WPIdata$wid, temp[,1])
colnames(temp2) <- c('id','LocalMoran')
temp3 <- merge(voronoi.f,temp2, by='id')

map <- ggplot(temp3, aes(x = long, y = lat, fill = LocalMoran)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low='red', mid='white', high='blue', midpoint=0) + theme_bw()
map <- map + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle("Dry season WPI Local Moran's I") + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
map

rm('temp', 'temp2','temp3','map')




# Wet season local mean WPI (FIG 5.16)
par(mfrow=c(1,2), mar=c(1,3,1,1))
#RES
breaks <- classIntervals(wetWPI_gwss_400NN_comp$SDF$wetRES_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Blues')
# wet LM
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetRES_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("a) Wet season RES")

#ACC
breaks <- classIntervals(wetWPI_gwss_400NN_comp$SDF$wetACC_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Greens')
# wet LM
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetACC_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("b) Wet season ACC")

#CAP
breaks <- classIntervals(wetWPI_gwss_400NN_comp$SDF$wetCAP_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Purples')
# wet LM
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetCAP_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("c) Wet season CAP")

#USE
breaks <- classIntervals(wetWPI_gwss_400NN_comp$SDF$wetUSE_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Oranges')
# wet LM
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetUSE_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("d) Wet season USE")

#ENV
breaks <- classIntervals(wetWPI_gwss_400NN_comp$SDF$wetENV_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Greys')
# wet LM
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetENV_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("e) Wet season ENV")

#Total
breaks <- classIntervals(wetWPI_gwss_400NN_comp$SDF$wetWPIgpca_LM, n=5, style='quantile')
colours <- brewer.pal(6, 'Reds')
# wet LM
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetWPIgpca_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("f) Wet season WPI")




###### Plot wet season ranks (FIG 5.18)
temp <- data.frame(WPIdata$wid, rank(WPIdata$wetWPIgpca))
colnames(temp) <- c('id','Rank')
View(temp)
temp2 <- merge(voronoi.f,temp, by='id')

map <- ggplot(temp2, aes(x = long, y = lat, fill = Rank)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low='red', mid='yellow', high='blue', midpoint=4108) + theme_bw()
map <- map + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Wet season WPI ranks') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
map

rm('temp', 'temp2','map')


# calculate Moran's I
temp <- componentMoran2(wetWPIcomp, voronoi.nb.listw)
moranTable <- data.frame(temp)
rm('temp','moranTable')






# plot wet season WPI density per province (FIG 5.17)
temp <- table(WPIdata$bcne)
temp <- as.data.frame(temp)
tempnames <- as.character(temp[,"Var1"])
provinceWPIsummary <- vector()
provinceWPIplot <- rep(NA,1000)
for (i in 1:length(tempnames)) {
	temppro <- tempnames[i]
	tempwet <- summary(WPIdata$wetWPIgpca[WPIdata$bcne==temppro])
	provinceWPIsummary <- rbind(provinceWPIsummary, c(as.character(temppro), tempwet))
	
	tempwet <- c(WPIdata$wetWPIgpca[WPIdata$bcne==temppro], rep(NA, 1000 - length(WPIdata$wetWPIgpca[WPIdata$bcne==temppro])))
	provinceWPIplot <- data.frame(provinceWPIplot, tempwet)
}
provinceWPIsummary <- as.data.frame(provinceWPIsummary)
provinceWPIplot <- provinceWPIplot[,-1]
colnames(provinceWPIplot) <- tempnames
rm('temp','tempnames','temppro','tempwet')

temp <- melt(provinceWPIplot)

p1 <- ggplot(temp, aes(value, fill=variable, colour = variable)) +  geom_density(position='stack', alpha=0.5) + ggtitle('Provincial WPI densities') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + labs(x='WPI')
p2 <- ggplot(temp, aes(value, colour = variable)) +  geom_density() + ggtitle('Provincial WPI densities') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + labs(x='WPI')

multiplot(p1,p2,cols=2)

rm('p1','p2','temp', 'provinceWPIsummary','provinceWPIplot')




# perform an anova test for the means
temp <- pairwise.t.test(WPIdata$wetWPIgpca,Provinces,  p.adj = "none")




###### Local moran
temp <- localmoran(wetWPIcomp[,1], voronoi.nb.listw)
temp2 <- data.frame(WPIdata$wid, temp[,1])
colnames(temp2) <- c('id','LocalMoran')
temp3 <- merge(voronoi.f,temp2, by='id')

map <- ggplot(temp3, aes(x = long, y = lat, fill = LocalMoran)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low='red', mid='white', high='blue', midpoint=0) + theme_bw()
map <- map + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle("Dry season WPI Local Moran's I") + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
map

rm('temp', 'temp2','temp3','map')













#################################################
################ Chapter 5 - Comparing the seasonal WPI
#################################################


# Table 5.6 is the weight matrix calculated earlier:
globalPCAweights


# Perform PCA for both-season data
temp <- rbind(dryWPIcomp[,2:6], wetWPIcomp[,2:6])
PCA <- prcomp(temp, scale=F) # no scaling - the components are already scaled 0-100

##### get component loadings
varnames <- c('RES','ACC','CAP','USE','ENV')
loadings1 <- data.frame(cbind(varnames,as.numeric(PCA$rotation[,1])))
loadings1[,2] <- as.numeric(PCA$rotation[,1])
loadings2 <- data.frame(cbind(varnames,as.numeric(PCA$rotation[,2])))
loadings2[,2] <- as.numeric(PCA$rotation[,2])
loadings3 <- data.frame(cbind(varnames,as.numeric(PCA$rotation[,3])))
loadings3[,2] <- as.numeric(PCA$rotation[,3])

# plot loadings (FIG 5.19)
temp <- rbind(data.frame(id="PC1", loadings1), data.frame(id="PC2", loadings2), data.frame(id="PC3", loadings3) )
ggplot(temp, aes(x = 0, xend = V2, y = varnames, yend=varnames, colour=id)) + geom_point(aes(V2, varnames), size=3) + geom_segment() + ggtitle('PC Variable Loadings') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + facet_wrap( ~ id, ncol=3)

rm(temp, loadings1, loadings2, loadings3)



# Plot comparison of WPI calculated from different weighting schemes (look at table 5.6)
temp <- data.frame(WPIdata$wid, WPIdata$dryWPIsinglegpca, WPIdata$wetWPIsinglegpca, WPIdata$dryWPIgpca, WPIdata$wetWPIgpca)
colnames(temp) <- c('id','SingleDry','SingleWet','BothDry','BothWet')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = SingleDry)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#d73027", mid="#ffffbf", high="#1a9850", midpoint=50) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season - single') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = SingleWet)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#d73027", mid="#ffffbf", high="#1a9850", midpoint=50) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season - single') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = BothDry)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#f1a340", mid="#ffffbf", high="#998ec3", midpoint=50) + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Dry season - both') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

map4 <- ggplot(temp2, aes(x = long, y = lat, fill = BothWet)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#f1a340", mid="#ffffbf", high="#998ec3", midpoint=50) + theme_bw()
map4 <- map4 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('d) Wet season - both') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map1,map3,map2,map4, cols=2)



# Plot FIG5.21
map1 <- ggplot(temp2, aes(x = long, y = lat, fill = (BothDry-SingleDry))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#b35806", mid="#f7f7f7", high="#542788", name='WPI difference') + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season difference between weighting schemes') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = (BothWet-SingleWet))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#b35806", mid="#f7f7f7", high="#542788", name='WPI difference') + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season difference between weighting schemes') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map1,map2,cols=2)


# Plot FIG 5.22
map1 <- ggplot(temp2, aes(x = long, y = lat, fill = (BothWet-BothDry))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#b35806", mid="#ffffbf", high="#542788", name='WPI difference') + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Difference between seasonal WPI') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())
map1

rm('map1','map3','map2','map4', 'temp','temp2')



# Plot FIG 5.23 - dumbell comparison of provincial WPI's
temp <- table(WPIdata$bcne)
temp <- as.data.frame(temp)
tempnames <- as.character(temp[,"Var1"])
provinceWPI <- vector()
for (i in 1:length(tempnames)) {
	temppro <- tempnames[i]
	tempdry <- WPIdata$dryWPIgpca[WPIdata$bcne==temppro]
	tempwet <- WPIdata$wetWPIgpca[WPIdata$bcne==temppro]
	dryrank <- rank(WPIdata$dryWPIgpca)[WPIdata$bcne==temppro]
	wetrank <- rank(WPIdata$wetWPIgpca)[WPIdata$bcne==temppro]
	
	provinceWPI <- rbind(provinceWPI, c(as.character(temppro), as.numeric(round(mean(tempdry),1)), as.numeric(round(mean(tempwet),1)), as.numeric(round(mean(dryrank),1)), as.numeric(round(mean(wetrank),1))))
}
provinceWPI <- as.data.frame(provinceWPI)
colnames(provinceWPI) <- c('Province','DryWPI','WetWPI','DryRank','WetRank')

rm('temp','tempnames','temppro','tempdry','tempwet','dryrank','wetrank')

#sort by wet season WPI before plotting
provinceWPI$Province <- factor(provinceWPI$Province, levels = provinceWPI$Province[order(provinceWPI$WetWPI)])
p1 <- ggplot(provinceWPI, aes(x = Province, xend=Province, y = DryWPI, yend=WetWPI, fill=Province)) + geom_segment() + geom_point(aes(x=Province, y=DryWPI, color='blue'), size=3) + geom_point(aes(x=Province, y=WetWPI, color='red'), size=3) + ggtitle('a) Seasonal WPI differences') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y= 'WPI', x='')

#sort by wet season WPI rank before plotting
provinceWPI$Province <- factor(provinceWPI$Province, levels = provinceWPI$Province[order(provinceWPI$WetRank)])
p2 <- ggplot(provinceWPI, aes(x = Province, xend=Province, y = DryRank, yend=WetRank, fill=Province)) + geom_segment() + geom_point(aes(x=Province, y=DryRank, color='blue'), size=3) + geom_point(aes(x=Province, y=WetRank, color='red'), size=3) + ggtitle('b) Seasonal Rank differences') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y= 'WPI', x='')

multiplot(p1,p2,cols=1)

rm('p1','p2')



# Plot FIG 5.24 - GWSS
ux <- unique(s.grid@coords[,1])
uy <- unique(s.grid@coords[,2])
par(mfrow=c(1,2), mar=c(1,1,1,1))

############ Local mean
breaks <- classIntervals(c(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LM, wetWPI_gwss_400NN_comp$SDF$wetWPIgpca_LM), n=5, style='quantile')
colours <- brewer.pal(6, 'Purples')
# dry 
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("a) Dry season WPI; GW Mean")
# wet 
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetWPIgpca_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1))
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("b) Wet season WPI; GW Mean")

######### Standard deviation
breaks <- classIntervals(c(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LSD, wetWPI_gwss_400NN_comp$SDF$wetWPIgpca_LSD), n=5, style='quantile')
colours <- brewer.pal(7, 'Reds')
# dry 
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LSD, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("c) Dry season WPI; Standard Deviation")
# wet 
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetWPIgpca_LSD, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1))
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("d) Wet season WPI; Standard Deviation")

rm(predmat, sh, colours, breaks)



# Plot FIG 5.25
temp <- as.data.frame(cbind(as.data.frame(WPIdata$dryWPIgpca), as.data.frame(WPIdata$wetWPIgpca), as.factor(WPIdata$bcne), as.factor(WPIdata$tcne),as.factor(WPIdata$tcne)))
colnames(temp) <- c('Dry_WPI', 'Wet_WPI','Province','District','Village')
ggplot(data = temp, aes(Wet_WPI, Dry_WPI)) + geom_point(size=0.3) + geom_smooth(aes(color="Smooth trend")) + geom_abline(aes(colour='Equal WPI', slope=1, intercept=0), size=1) + theme_bw() + ggtitle('Relationship between dry and wet season WPI') +  scale_colour_manual("Legend",values=c("red","blue")) + scale_linetype_manual("Legend",values=c(1,1))



# Plot correlation tables in Table 5.7
# Dry season
corrtable <- matrix(nrow=5, ncol=5)
for(i in 1:5) {
	corrtable[1,i] <- mean(dryWPI_gwss_400NN_comp$SDF@data[,45+i])
}
for(i in 1:4) {
	corrtable[2,i+1] <- mean(dryWPI_gwss_400NN_comp$SDF@data[,50+i])
}
corrtable[2,]
for(i in 1:3) {
	corrtable[3,i+2] <- mean(dryWPI_gwss_400NN_comp$SDF@data[,54+i])
}
for(i in 1:2) {
	corrtable[4,i+3] <- mean(dryWPI_gwss_400NN_comp$SDF@data[,57+i])
}
	corrtable[5,5] <- mean(dryWPI_gwss_400NN_comp$SDF@data[,60])
colnames(corrtable) <- c('DryRES','DryACC','DryCAP','DryUSE','DryENV')
rownames(corrtable) <- c('DryWPI','DryRES','DryACC','DryCAP','DryUSE')

corrtable

# Wet season
corrtable <- matrix(nrow=5, ncol=5)
for(i in 1:5) {
	corrtable[1,i] <- mean(wetWPI_gwss_400NN_comp$SDF@data[,45+i])
}
for(i in 1:4) {
	corrtable[2,i+1] <- mean(wetWPI_gwss_400NN_comp$SDF@data[,50+i])
}
corrtable[2,]
for(i in 1:3) {
	corrtable[3,i+2] <- mean(wetWPI_gwss_400NN_comp$SDF@data[,54+i])
}
for(i in 1:2) {
	corrtable[4,i+3] <- mean(wetWPI_gwss_400NN_comp$SDF@data[,57+i])
}
	corrtable[5,5] <- mean(wetWPI_gwss_400NN_comp$SDF@data[,60])
colnames(corrtable) <- c('WetRES','WetACC','WetCAP','WetUSE','WetENV')
rownames(corrtable) <- c('WetWPI','WetRES','WetACC','WetCAP','WetUSE')

corrtable




# Plot local correlations FIG 5.26
par(mfrow=c(1,3), mar=c(1,1,1,1))
# Dry season
# RES
breaks <- classIntervals(dryWPI_gwss_400NN_comp$SDF@data[,46], n=5, style='quantile')
colours <- brewer.pal(5, 'Blues')
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF@data[,46]+1, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[2:5]+1,100), digits=3) )
plotBG()
sh <- shading(breaks=round(breaks$brks[2:5], digits=3), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("a) DryWPI vs DryRES")
# CAP
breaks <- classIntervals(dryWPI_gwss_400NN_comp$SDF@data[,48], n=5, style='quantile')
colours <- brewer.pal(5, 'Purples')
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF@data[,48]+1, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[2:5]+1,100), digits=3) )
plotBG()
sh <- shading(breaks=round(breaks$brks[2:5], digits=3), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("b) DryWPI vs. DryCAP")
# USE
breaks <- classIntervals(dryWPI_gwss_400NN_comp$SDF@data[,49], n=5, style='quantile')
colours <- brewer.pal(5, 'Oranges')
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF@data[,49]+1, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[2:5]+1,100), digits=3))
plotBG()
sh <- shading(breaks=round(breaks$brks[2:5], digits=3), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("c) DryWPI vs. DryUSE")

# Wet season
# RES
breaks <- classIntervals(wetWPI_gwss_400NN_comp$SDF@data[,48], n=5, style='quantile')
colours <- brewer.pal(5, 'Purples')
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF@data[,48]+1, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[2:5]+1,100), digits=3) )
plotBG()
sh <- shading(breaks=round(breaks$brks[2:5], digits=3), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("d) WetWPI vs WetCAP")
# USE
breaks <- classIntervals(wetWPI_gwss_400NN_comp$SDF@data[,49], n=5, style='quantile')
colours <- brewer.pal(5, 'Oranges')
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF@data[,49]+1, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[2:5]+1,100), digits=3))
plotBG()
sh <- shading(breaks=round(breaks$brks[2:5], digits=3), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("e) WetWPI vs. WetUSE")
# CAP
breaks <- classIntervals(wetWPI_gwss_400NN_comp$SDF@data[,50], n=5, style='quantile')
colours <- brewer.pal(5, 'Greys')
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF@data[,50]+1, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[2:5]+1,100), digits=3) )
plotBG()
sh <- shading(breaks=round(breaks$brks[2:5], digits=3), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("f) WetWPI vs. WetENV")

rm(predmat, sh, colours, breaks)


# pairwise t-test on WPI's
temp <- rbind(WPIdata$dryWPIgpca, WPIdata$wetWPIgpca)
temp2 <- rbind(rep("Dry", 8215), rep("Wet", 8215))
temp3 <- pairwise.t.test(temp, temp2)
temp3
rm('temp','temp2','temp3')














#################################################
################ Chapter 5 - Cluster Analysis
#################################################

# NOTE!!! Unfortunately when running the analysis for the thesis, seed was not set and therefore the result of running the following
# script will not yield with identical results. k-means does not produce the same outcome everytime it is ran. More information in 
# the thesis section 3.3.1

# k-means
#determine number of clusters using NbClust-package
library(NbClust)
# first scale coordinates 0-100 so its in the same scale as the components. Using sample because otherwise takes too long
scaledy <- WPIdata@coords[,1]
scaledy <- scaledy - min(scaledy)
scaledy <- scaledy/max(scaledy)*100
scaledx <- WPIdata@coords[,2]
scaledx <- scaledx - min(scaledx)
scaledx <- scaledx/max(scaledx)*100

#run NbClust analysis
temp <- data.frame(dryWPIcomp[sample,2:6], scaledy[sample],scaledx[sample])
drynclust <- NbClust(temp, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "all")
temp2 <- data.frame(wetWPIcomp[sample,2:6], scaledy[sample],scaledx[sample])
wetnclust <- NbClust(temp2, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "all")


#Table 5.8 is made of data from these two
drynclust
wetnclust


# DRY SEASON CLUSTERING
# cluster k-means with 3 clusters
temp <- data.frame(dryWPIcomp[,2:6], scaledy,scaledx)
tempcluster <- kmeans(temp,3)
tempcluster2 <- kmeans(temp,4)
tempcluster3 <- kmeans(temp,5)
tempcluster4 <- kmeans(temp,6)

clusters <- cbind(tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)

# clustering without spatial data
temp <- data.frame(dryWPIcomp[,2:6])
tempcluster <- kmeans(temp,3)
tempcluster2 <- kmeans(temp,4)
tempcluster3 <- kmeans(temp,5)
tempcluster4 <- kmeans(temp,6)

#add clusters to previously created variable clusters
clusters <- cbind(clusters,tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)


# WET SEASON CLUSTERING
# cluster k-means with 3 clusters
temp <- data.frame(wetWPIcomp[,2:6], scaledy,scaledx)
tempcluster <- kmeans(temp,3)
tempcluster2 <- kmeans(temp,4)
tempcluster3 <- kmeans(temp,5)
tempcluster4 <- kmeans(temp,6)

#add clusters to previously created variable clusters
clusters <- cbind(clusters,tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)

# clustering without spatial data
temp <- data.frame(wetWPIcomp[,2:6])
tempcluster <- kmeans(temp,3)
tempcluster2 <- kmeans(temp,4)
tempcluster3 <- kmeans(temp,5)
tempcluster4 <- kmeans(temp,6)

#add clusters to previously created variable clusters
clusters <- cbind(clusters,tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)

# name the columns in the data frame S signifies spatial clustering, no S means aspatial clustering. Number is the number of clusters
clusters <- as.data.frame(clusters)
colnames(clusters) <- c('dryS3','dryS4','dryS5','dryS6','dry3','dry4','dry5','dry6','wetS3','wetS4','wetS5','wetS6','wet3','wet4','wet5','wet6')
rm('tempcluster','tempcluster2','tempcluster3','tempcluster4')




# Plot selected clusters FIG 5.27
#first reorder wet season clusters so they are in the same order as in dry season
temp <- clusters$wetS4*10
temp[temp==30] <- 1
temp[temp==10] <- 2
temp[temp==40] <- 3
temp[temp==20] <- 4
temp <- data.frame(WPIdata$wid, clusters$dryS4, temp)
colnames(temp) <- c('id','DryClusters','WetClusters')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(DryClusters))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_manual(values=brewer.pal(12, "Paired")[c(1:4)], name='Cluster', labels=c('Rich North','Rich South','Poor South','Poor North')) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season clusters') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(WetClusters))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_manual(values=brewer.pal(12, "Paired")[c(5:6,9:10)], name='Cluster', labels=c('Rich North','Rich South','Poor South','Poor North')) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season clusters') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map1,map2,cols=2)

rm(temp, temp2)



# Plot FIG 5.28
temp <- data.frame(dryWPIcomp, clusters$dryS4, Provinces)
colnames(temp) <- c("dryWPI","dryRES", "dryACC", "dryCAP", "dryUSE", "dryENV", "Cluster", "Province") 
temp <- melt(temp[,1:7], id="Cluster")
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(Cluster))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Dry Season') + scale_fill_manual(name="Cluster", labels=c("Rich North", "Rich South", "Poor South","Poor North"),values=brewer.pal(4,"Paired"))# dry season

# Wet season
# First reorder clusters to same order as dry
temp <- clusters$wetS4*10
temp[temp==30] <- 1
temp[temp==10] <- 2
temp[temp==40] <- 3
temp[temp==20] <- 4
temp <- data.frame(wetWPIcomp, temp, Provinces)
colnames(temp) <- c("wetWPI","wetRES", "wetACC", "wetCAP", "wetUSE", "wetENV", "Cluster", "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[,1:7], id="Cluster")
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(Cluster))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Wet Season') + scale_fill_manual(name="Cluster", labels=c("Rich North", "Rich South", "Poor South","Poor North"),values=brewer.pal(10,"Paired")[c(5:6,9:10)])

multiplot(p1,p2,cols=1)


# Do the same plot for variables - this is not shown in the thesis
# dry season
temp <- data.frame(dryVAR, clusters$dryS4, Provinces)
colnames(temp) <- c(colnames(dryVAR), "Cluster", "Province") 
temp <- melt(temp[,1:19], id="Cluster")
ggplot(data = temp, aes(variable, value, fill = factor(Cluster))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Dry Season') + scale_fill_manual(name="Cluster", labels=c("Rich North", "Rich South", "Poor South","Poor North"),values=brewer.pal(4,"Paired"))# dry season

# Wet season
# First reorder clusters to same order as dry
temp <- clusters$wetS4*10
temp[temp==30] <- 1
temp[temp==10] <- 2
temp[temp==40] <- 3
temp[temp==20] <- 4
temp <- data.frame(wetVAR, temp, Provinces)
colnames(temp) <- c(colnames(wetVAR), "Cluster", "Province") 
temp <- melt(temp[,1:18], id="Cluster")
ggplot(data = temp, aes(variable, value, fill = factor(Cluster))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Wet Season') + scale_fill_manual(name="Cluster", labels=c("Rich North", "Rich South", "Poor South","Poor North"),values=brewer.pal(10,"Paired")[c(5:6,9:10)])




# Do rank clustering - this repeats most of the above code

tempdry <- vector()
tempwet <- vector()
for (i in 1:5) {
	temp <- as.integer(rank(dryWPIcomp[,i+1]))
	tempdry <- cbind(tempdry,temp)
	temp <- as.integer(rank(wetWPIcomp[,i+1]))
	tempwet <- cbind(tempwet,temp)	
}
tempdry <- data.frame(tempdry, rank(coordinates(WPIdata)[,1]), rank(coordinates(WPIdata)[,2]))
tempwet <- data.frame(tempwet, rank(coordinates(WPIdata)[,1]), rank(coordinates(WPIdata)[,2]))
colnames(tempdry) <- c('RES','ACC','CAP','USE','ENV','X','Y')
colnames(tempwet) <- c('RES','ACC','CAP','USE','ENV','X','Y')

drynclust.rank <- NbClust(tempdry, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "all")
wetnclust.rank <- NbClust(tempwet, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "all")

drynclust.rank
wetnclust.rank

# DRY SEASON RANK CLUSTERING
temp <- tempdry
tempcluster <- kmeans(temp,2)
tempcluster2 <- kmeans(temp,3)
tempcluster3 <- kmeans(temp,4)
tempcluster4 <- kmeans(temp,5)

clusters.rank <- cbind(tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)

temp <- tempdry[,1:5]
tempcluster <- kmeans(temp,2)
tempcluster2 <- kmeans(temp,3)
tempcluster3 <- kmeans(temp,4)
tempcluster4 <- kmeans(temp,5)

clusters.rank <- cbind(clusters,tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)


# WET SEASON RANK CLUSTERING
# cluster k-means with 3 clusters
temp <- tempwet
tempcluster <- kmeans(temp,2)
tempcluster2 <- kmeans(temp,3)
tempcluster3 <- kmeans(temp,4)
tempcluster4 <- kmeans(temp,5)

clusters.rank <- cbind(clusters,tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)

temp <- tempwet[,1:5]
tempcluster <- kmeans(temp,2)
tempcluster2 <- kmeans(temp,3)
tempcluster3 <- kmeans(temp,4)
tempcluster4 <- kmeans(temp,5)

clusters.rank <- cbind(clusters,tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)

clusters.rank <- as.data.frame(clusters.rank)
colnames(clusters.rank) <- c('dryS2','dryS3','dryS4','dryS5','dry2','dry3','dry4','dry5','wetS2','wetS3','wetS4','wetS5','wet2','wet3','wet4','wet5')


# Plot FIG 5.29
temp <- data.frame(WPIdata$wid, clusters.rank$dryS5, clusters.rank$wetS4)
colnames(temp) <- c('id','DryClusters','WetClusters')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(DryClusters))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_manual(values=brewer.pal(12, "Paired")[c(1:4,7:8)], name='Cluster', labels=c('Mekong South','Poor South','Northeast','Poor North','Bolaven','Vientiane')) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Rank clusters - Dry season') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(WetClusters))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_manual(values=brewer.pal(12, "Paired")[c(5:6,9:11)], name='Cluster', labels=c('Rich North','Rich South','Central','Poor South','Poor North')) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Rank clusters - Wet season') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map1,map2,cols=2)

















#################################################
################ Chapter 5 - GWPCA 
#################################################


# Compute GWPCA for WPI components 
dryWPI_gwpca_400NN_comp <- gwpca(WPIdata, vars=c("dryRES", "dryACC", "dryCAP", "dryUSE", "dryENV"), bw=400, adaptive=T, k=5, dMat=distMat)
wetWPI_gwpca_400NN_comp <- gwpca(WPIdata, vars=c("wetRES", "wetACC", "wetCAP", "wetUSE", "wetENV"), bw=400, adaptive=T, k=5, dMat=distMat)


############## Calculate weights for each village using GWPCA

weights.gwpca <- data.frame()
# individual weights for dry season
for (i in 1:8215) {
	temp <- dryWPI_gwpca_400NN_comp$loadings[i,,]
	sqroots <- c(  sqrt(abs(temp[1,1])) , sqrt(abs(temp[2,2])),  sqrt(abs(temp[3,3])),  sqrt(abs(temp[4,4])),  sqrt(abs(temp[5,5]))  )
	sumsqroots <- sum(sqroots)
	temp_weights <- c(sqrt(abs(temp[1,1]))/sumsqroots , sqrt(abs(temp[2,2]))/sumsqroots,  sqrt(abs(temp[3,3]))/sumsqroots,  sqrt(abs(temp[4,4]))/sumsqroots,  sqrt(abs(temp[5,5]))/sumsqroots)
	weights.gwpca <- rbind(weights.gwpca,temp_weights)
}
rm('sqroots','sumsqroots','temp','temp_weights')
colnames(weights.gwpca) <- c('RES','ACC','CAP','USE','ENV' )
weights.gwpca.dry <- weights.gwpca


weights.gwpca <- data.frame()
# individual weights for wet season
for (i in 1:8215) {
	temp <- wetWPI_gwpca_400NN_comp$loadings[i,,]
	sqroots <- c(  sqrt(abs(temp[1,1])) , sqrt(abs(temp[2,2])),  sqrt(abs(temp[3,3])),  sqrt(abs(temp[4,4])),  sqrt(abs(temp[5,5]))  )
	sumsqroots <- sum(sqroots)
	temp_weights <- c(sqrt(abs(temp[1,1]))/sumsqroots , sqrt(abs(temp[2,2]))/sumsqroots,  sqrt(abs(temp[3,3]))/sumsqroots,  sqrt(abs(temp[4,4]))/sumsqroots,  sqrt(abs(temp[5,5]))/sumsqroots)
	weights.gwpca <- rbind(weights.gwpca,temp_weights)
}
rm('sqroots','sumsqroots','temp','temp_weights')
colnames(weights.gwpca) <- c('RES','ACC','CAP','USE','ENV' )
weights.gwpca.wet <- weights.gwpca
rm('weights.gwpca')




# calculate locally weighted WPI
dryWPI <- vector()
wetWPI <- vector()
dryWPIgmean <- vector()
wetWPIgmean <- vector()
for (i in 1:8215) {
	dryWPI[i] <- weights.gwpca.dry[i,1]*dryWPIcomp[i,2] + weights.gwpca.dry[i,2]*dryWPIcomp[i,3] + weights.gwpca.dry[i,3]*dryWPIcomp[i,4] + weights.gwpca.dry[i,4]*dryWPIcomp[i,5] + weights.gwpca.dry[i,5]*dryWPIcomp[i,6]
	wetWPI[i] <- weights.gwpca.wet[i,1]*wetWPIcomp[i,2] + weights.gwpca.wet[i,2]*wetWPIcomp[i,3] + weights.gwpca.wet[i,3]*wetWPIcomp[i,4] + weights.gwpca.wet[i,4]*wetWPIcomp[i,5] + weights.gwpca.wet[i,5]*wetWPIcomp[i,6]
	
	dryWPIgmean[i] <- (dryWPIcomp[i,2]^weights.gwpca.dry[i,1] * dryWPIcomp[i,3]^weights.gwpca.dry[i,2] * dryWPIcomp[i,4]^weights.gwpca.dry[i,3] * dryWPIcomp[i,5]^weights.gwpca.dry[i,4] * dryWPIcomp[i,6]^weights.gwpca.dry[i,5])^(1/sum(weights.gwpca.dry[i,]))
	wetWPIgmean[i] <- (wetWPIcomp[i,2]^weights.gwpca.wet[i,1] * wetWPIcomp[i,3]^weights.gwpca.wet[i,2] * wetWPIcomp[i,4]^weights.gwpca.wet[i,3] * wetWPIcomp[i,5]^weights.gwpca.wet[i,4] * wetWPIcomp[i,6]^weights.gwpca.wet[i,5])^(1/sum(weights.gwpca.wet[i,]))
}

WPIdata$dryWPIgwpca <- dryWPI
WPIdata$wetWPIgwpca <- wetWPI
WPIdata$dryWPIggwpca <- dryWPIgmean
WPIdata$wetWPIggwpca <- wetWPIgmean


rm('dryWPI','wetWPI','dryWPIgmean','wetWPIgmean')




########################## Do PCA based on the variables used in WPI calculation
dryWPI_gwpca_400NN_vars <- gwpca(WPIdata, vars=c("DryAvail","AvMaxDDay", "Irrigation", "DrinkNS", "ToiletType", "TimeCap", "DryRoad", "LitPopSh", "IncPov", "IrrAreaSh", "AgAreaPerC", "AgAqDepend", "ECO_V_cat", "dryDisast", "SoilDeg", "HumanFP"), bw=400, adaptive=T, k=10, dMat=distMat)
wetWPI_gwpca_400NN_vars <- gwpca(WPIdata, vars=c("WetAvail", "Irrigation", "DrinkNS", "ToiletType", "TimeCap", "WetRoad", "LitPopSh", "IncPov", "IrrAreaSh", "AgAreaPerC", "AgAqDepend", "ECO_V_cat", "wetDisast", "SoilDeg","HumanFP"), bw=400, adaptive=T, k=10, dMat=distMat)






# plot weighting scheme box plots FIG 5.32
# dry
temp <- as.data.frame(weights.gwpca.dry)
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
temp <- melt(temp)
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('Dry season') + coord_flip()
# wet
temp <- as.data.frame(weights.gwpca.wet)
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
temp <- melt(temp)
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('Wet season') + coord_flip()

multiplot(p1,p2,cols=2)
rm('p1','p2')



########## Map weights - FIG 5.33
########### DRY SEASON
temp <- data.frame(WPIdata$wid, weights.gwpca.dry)
colnames(temp) <- c('id','RES','ACC', 'CAP','USE','ENV')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = RES)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0.2) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season \n RES weights') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = ACC)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0.2) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Dry season \n ACC weights') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = CAP)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0.2) + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Dry season \n CAP weights') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map4 <- ggplot(temp2, aes(x = long, y = lat, fill = USE)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0.2) + theme_bw()
map4 <- map4 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('d) Dry season \n USE weights') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map5 <- ggplot(temp2, aes(x = long, y = lat, fill = ENV)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0.2) + theme_bw()
map5 <- map5 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('e) Dry season \n ENV weights') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

multiplot(map1,map4,map2,map5,map3, cols=3)



########## WET SEASON
temp <- data.frame(WPIdata$wid, weights.gwpca.wet)
colnames(temp) <- c('id','RES','ACC', 'CAP','USE','ENV')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = RES)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#c51b7d", mid="white", high="#4d9221", midpoint=0.2) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('f) Wet season \n RES weights') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = ACC)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#c51b7d", mid="white", high="#4d9221", midpoint=0.2) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('g) Wet season \n ACC weights') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = CAP)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#c51b7d", mid="white", high="#4d9221", midpoint=0.2) + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('h) Wet season \n CAP weights') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map4 <- ggplot(temp2, aes(x = long, y = lat, fill = USE)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#c51b7d", mid="white", high="#4d9221", midpoint=0.2) + theme_bw()
map4 <- map4 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('i) Wet season \n USE weights') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map5 <- ggplot(temp2, aes(x = long, y = lat, fill = ENV)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#c51b7d", mid="white", high="#4d9221", midpoint=0.2) + theme_bw()
map5 <- map5 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('j) Wet season \n ENV weights') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

multiplot(map1,map4,map2,map5,map3, cols=3)



###### comparison plot - FIG 5.34
temp <- data.frame(WPIdata$wid, WPIdata$dryWPIggwpca, WPIdata$wetWPIggwpca, WPIdata$dryWPIggwpca-WPIdata$dryWPIsinglegpca, WPIdata$wetWPIggwpca-WPIdata$wetWPIsinglegpca)
colnames(temp) <- c('id','Dry','Wet', 'DryDiff','WetDiff')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = Dry)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=50) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season WPI (GWPCA)') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())


map2 <- ggplot(temp2, aes(x = long, y = lat, fill = Wet)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=50) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season WPI (GWPCA)') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = DryDiff)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#7f3b08", mid="#f7f7f7", high="#2d004b", midpoint=0, name='Dry season \n difference') + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Difference between locally and \n globally weighted dry WPI') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map4 <- ggplot(temp2, aes(x = long, y = lat, fill = WetDiff)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#7f3b08", mid="#f7f7f7", high="#2d004b", midpoint=0, name='Wet season \n difference') + theme_bw()
map4 <- map4 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('d) Difference between locally and \n globally weighted wet WPI') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

multiplot(map1,map3,map2,map4,cols=2)


# ####### PLOT wet season - dry season difference (WPI gmean pca) FIG 5.35

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = (Wet-Dry))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695") + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Seasonal difference (GWPCA)') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())

map1
rm('map1','map2','temp','temp2')



################## PLOT lead items for components 
############# dry season FIG 5.30
# plot lead item
local.loadings1 <- dryWPI_gwpca_400NN_comp$loadings[,,1] 
local.loadings2 <- dryWPI_gwpca_400NN_comp$loadings[,,2]
local.loadings3 <- dryWPI_gwpca_400NN_comp$loadings[,,3]
lead.item1 <- colnames(local.loadings1)[max.col(abs(local.loadings1))]
lead.item2 <- colnames(local.loadings2)[max.col(abs(local.loadings2))]
lead.item3 <- colnames(local.loadings3)[max.col(abs(local.loadings3))]


temp <- data.frame(WPIdata$wid, lead.item1, lead.item2, lead.item3)
colnames(temp) <- c('id','Lead item PC1','Lead item PC2', 'Lead item PC3')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,8]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season lead item, PC1') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,9]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Dry season lead item, PC2') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,10]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Dry season lead item, PC3') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

temp <- table(lead.item1)
temp <- as.data.frame(temp)
bar1 <- ggplot(temp) + geom_bar(aes(lead.item1,Freq, fill=factor(lead.item1)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('d) Dry season lead item, PC1')

temp <- table(lead.item2)
temp <- as.data.frame(temp)
bar2 <- ggplot(temp) + geom_bar(aes(lead.item2,Freq, fill=factor(lead.item2)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('e) Dry season lead item, PC2')

temp <- table(lead.item3)
temp <- as.data.frame(temp)
bar3 <- ggplot(temp) + geom_bar(aes(lead.item3,Freq, fill=factor(lead.item3)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('f) Dry season lead item, PC3')

multiplot(map1,bar1,map2,bar2,map3,bar3,cols=3)

########## wet season FIG 5.31
# plot lead item
local.loadings1 <- wetWPI_gwpca_400NN_comp$loadings[,,1] 
local.loadings2 <- wetWPI_gwpca_400NN_comp$loadings[,,2]
local.loadings3 <- wetWPI_gwpca_400NN_comp$loadings[,,3]
lead.item1 <- colnames(local.loadings1)[max.col(abs(local.loadings1))]
lead.item2 <- colnames(local.loadings2)[max.col(abs(local.loadings2))]
lead.item3 <- colnames(local.loadings3)[max.col(abs(local.loadings3))]


temp <- data.frame(WPIdata$wid, lead.item1, lead.item2, lead.item3)
colnames(temp) <- c('id','Lead item PC1','Lead item PC2', 'Lead item PC3')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,8]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Wet season lead item, PC1') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,9]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season lead item, PC2') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,10]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Wet season lead item, PC3') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

temp <- table(lead.item1)
temp <- as.data.frame(temp)
bar1 <- ggplot(temp) + geom_bar(aes(lead.item1,Freq, fill=factor(lead.item1)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('d) Wet season lead item, PC1')

temp <- table(lead.item2)
temp <- as.data.frame(temp)
bar2 <- ggplot(temp) + geom_bar(aes(lead.item2,Freq, fill=factor(lead.item2)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('e) Wet season lead item, PC2')

temp <- table(lead.item3)
temp <- as.data.frame(temp)
bar3 <- ggplot(temp) + geom_bar(aes(lead.item3,Freq, fill=factor(lead.item3)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('f) Wet season lead item, PC3')

multiplot(map1,bar1,map2,bar2,map3,bar3,cols=3)

rm('map1','map2','map3','bar1','bar2','bar3','temp','temp2')
rm('local.loadings1','local.loadings2','local.loadings3','lead.item1','lead.item2','lead.item3')



# test spatial variance of GWPCA using monte carlo approach
dry.gwpca.mc1 <- montecarlo.gwpca.1(WPIdata, bw = 400, vars=c('dryRES','dryACC','dryCAP','dryUSE','dryENV'), k = 2, nsims=99,robust = FALSE, kernel = "gaussian", adaptive = TRUE, p = 2, theta = 0, longlat = F, distMat)
wet.gwpca.mc1 <- montecarlo.gwpca.1(WPIdata, bw = 400, vars=c('wetRES','wetACC','wetCAP','wetUSE','wetENV'), k = 2, nsims=99,robust = FALSE, kernel = "gaussian", adaptive = TRUE, p = 2, theta = 0, longlat = F, distMat)
















#################################################
################ Chapter 5 - GWR
#################################################

### WARNING! These may take extensive time to finish. With my setup (8GB RAM, i7-5500U) some of the scripts ran 
# for 24h+. Leung's F3 test ran for 5 full days before erroring out. If the commands do not finish, try running
# with a random subset. Also, spgwr results with the hatmatrix (required for the F1-2-3 tests) are 500mb+. It may
# be a good idea only to run the ones you're interested in.

# create a name vector for model selection
names <- colnames(WPIdata_sample@data)[9:77]
newnames <- vector()
for (i in 1:length(names)) {
	newnames[i] <- paste("WPIdata_sample$",names[i],sep="")
}

# remove variables which cause problems (script errored out)
newnames <- newnames[-c(40,41,51,53,54)]
names <- names[-c(40,41,51,53,54)]

# run model selection based on cross validation (GWmodel)
dryModelSelection <- model.selection.gwr(DeVar="dryWPIgpca",InDeVars=newnames, data=WPIdata_sample,bw=400,approach="AIC", adaptive=T,kernel="gaussian",dMat=distMat.sample,p=2, theta=0, longlat=F)
wetModelSelection <- model.selection.gwr(DeVar="wetWPIgpca",InDeVars=newnames, data=WPIdata_sample,bw=400,approach="AIC", adaptive=T,kernel="gaussian",dMat=distMat.sample,p=2, theta=0, longlat=F)

# choose the model with lowest AIC
# dry season
temp <- which(dryModelSelection[[2]][,3]== min(dryModelSelection[[2]][,3]))
dryModel <- dryModelSelection[[1]][temp]
dryModelVars <- substring(dryModel[[1]][[2]], 16)
dryModelVars <- paste(dryModelVars, collapse='+')
# wet season
temp <- which(wetModelSelection[[2]][,3]== min(wetModelSelection[[2]][,3]))
wetModel <- wetModelSelection[[1]][temp]
wetModelVars <- substring(wetModel[[1]][[2]], 16)
wetModelVars <- paste(wetModelVars, collapse='+')

dryModelVars
wetModelVars

# add calculated wpi to WPIdata_orig so we can use it in regression
WPIdata_orig$dryWPIgpca <- WPIdata$dryWPIgpca
WPIdata_orig$wetWPIgpca <- WPIdata$wetWPIgpca



# calculate optimal bw for GWR. the long string of variables is taken from dry/wetModelVars
drygwr.bw.adapt <- bw.gwr(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data=WPIdata_orig, kernel='gaussian', adaptive=TRUE, dMat=distMat)
drygwr.bw <- bw.gwr(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data=WPIdata_orig, kernel='gaussian', adaptive=FALSE, dMat=distMat)
wetgwr.bw.adapt <- bw.gwr(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data=WPIdata_orig, kernel='gaussian', adaptive=TRUE, dMat=distMat)
wetgwr.bw <- bw.gwr(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data=WPIdata_orig, kernel='gaussian', adaptive=FALSE, dMat=distMat)


################ Perform GWR
dry.gwr.adapt <- gwr.basic(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data=WPIdata_orig, bw=drygwr.bw.adapt, kernel='gaussian', dMat=distMat, adaptive=TRUE)
dry.gwr <- gwr.basic(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data=WPIdata_orig, bw=drygwr.bw, kernel='gaussian', dMat=distMat, adaptive=FALSE)
wet.gwr.adapt <- gwr.basic(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data=WPIdata_orig, bw=wetgwr.bw.adapt, kernel='gaussian', dMat=distMat, adaptive=TRUE)
wet.gwr <- gwr.basic(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data=WPIdata_orig, bw=wetgwr.bw, kernel='gaussian', dMat=distMat, adaptive=FALSE)

# Do GWR with spgwr
dry.gwr.adapt.spgwr <- gwr(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data = WPIdata_orig, adapt=drygwr.bw.adapt/8215, hatmatrix=TRUE)
wet.gwr.adapt.spgwr <- gwr(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data = WPIdata_orig, adapt=wetgwr.bw.adapt/8215, hatmatrix=TRUE)

# Robust GWR
dry.gwr.adapt.robust <- gwr.robust(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data=WPIdata_orig, bw=drygwr.bw.adapt, kernel='gaussian', dMat=distMat, adaptive=TRUE)
wet.gwr.adapt.robust <- gwr.robust(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data=WPIdata_orig, bw=wetgwr.bw.adapt, kernel='gaussian', dMat=distMat, adaptive=TRUE)

# Mixed GWR
dry.gwr.adapt.mixed <- gwr.mixed(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data=WPIdata_orig, fixed.vars=c("DisFreNS", "FloodFre"), bw=drygwr.bw.adapt, kernel='gaussian', dMat=distMat, adaptive=TRUE)
wet.gwr.adapt.mixed <- gwr.mixed(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data=WPIdata_orig, fixed.vars=c("OtherDisFr", "PopDepAqua"),bw=wetgwr.bw.adapt, kernel='gaussian', dMat=distMat, adaptive=TRUE)


# Local multicollinearity diagnostics - Table 5.12 is based on these
dry.gwr.adapt.collins <- gwr.collin.diagno(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data=WPIdata_orig, bw=drygwr.bw.adapt, kernel='gaussian', dMat=distMat, adaptive=TRUE)
wet.gwr.adapt.collins <- gwr.collin.diagno(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data=WPIdata_orig, bw=wetgwr.bw.adapt, kernel='gaussian', dMat=distMat, adaptive=TRUE)


# test the GWR
#F3 tests could not be finished due to running for 5 days (120h) and couldn't be kept on as the deadline was approaching. 
#However, it is unlikely that the result would have differed from F1, F2 and the monte carlo tests
dry.F1 <- LMZ.F1GWR.test(dry.gwr.adapt.spgwr)
dry.F2 <- LMZ.F2GWR.test(dry.gwr.adapt.spgwr)
#dry.F3 <- LMZ.F3GWR.test(dry.gwr.adapt.spgwr)
wet.F1 <- LMZ.F1GWR.test(wet.gwr.adapt.spgwr)
wet.F2 <- LMZ.F2GWR.test(wet.gwr.adapt.spgwr)
#wet.F3 <- LMZ.F3GWR.test(wet.gwr.adapt.spgwr)

#Monte carlo test for the variables - Table 5.11 is based on these
dry.gwr.mctest <- montecarlo.gwr(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data = WPIdata_orig,nsims=99, kernel="gaussian",adaptive=T, bw=368, p=2, theta=0, longlat=F,dMat=distMat)
wet.gwr.mctest <- montecarlo.gwr(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data = WPIdata_orig,nsims=99, kernel="gaussian",adaptive=T, bw=368, p=2, theta=0, longlat=F,dMat=distMat)


# Do GWR with spgwr with collinearity problems addressed (a few variables removed)
dry.gwr.adapt.spgwr.2 <- gwr(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data = WPIdata_orig, adapt=drygwr.bw.adapt/8215, hatmatrix=FALSE)
wet.gwr.adapt.spgwr.2 <- gwr(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+StartRF3a+WatSupp, data = WPIdata_orig, adapt=wetgwr.bw.adapt/8215, hatmatrix=FALSE)



# Plot local r2 - FIG 5.36
temp <- data.frame(WPIdata$wid, dry.gwr.adapt.spgwr$SDF$localR2, wet.gwr.adapt.spgwr$SDF$localR2)
colnames(temp) <- c('id','DryR2','WetR2')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = DryR2)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0.70) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season GWR model R-square') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = WetR2)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0.70) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season GWR model R-square') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map1,map2,cols=2)

rm('map1','map2','temp','temp2')


# Plot prediction and residuals - 5.37
temp <- data.frame(WPIdata$wid, dry.gwr.adapt.spgwr$lm$fitted.values, dry.gwr.adapt.spgwr$lm$residuals)
colnames(temp) <- c('id','pred','residuals')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = pred)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=60) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season prediction') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = residuals)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#8e0152", mid="white", high="#276419") + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Dry season residuals') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

temp <- data.frame(WPIdata$wid, wet.gwr.adapt.spgwr$lm$fitted.values, wet.gwr.adapt.spgwr$lm$residuals)
colnames(temp) <- c('id','pred','residuals')
temp2 <- merge(voronoi.f,temp, by='id')

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = pred)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=60) + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Wet season prediction') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map4 <- ggplot(temp2, aes(x = long, y = lat, fill = residuals)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#8e0152", mid="white", high="#276419") + theme_bw()
map4 <- map4 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('d) Wet season residuals') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

multiplot(map1,map3,map2,map4, cols=2)

rm('map1','map2','map3','map4','temp','temp2')