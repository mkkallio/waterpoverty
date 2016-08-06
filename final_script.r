###################################
# Poverty in Laos
#################################

##################### Load libraries
library(rgdal)
library(sp)
library(GISTools)
#library(maptools)
library(spdep)
#library(sp)
#library(RColorBrewer)
#library(MASS)
#library(rgeos)
#library(Matrix)
library(ggplot2)
library(raster)
library(GWmodel)
#library(gstat)
library(deldir)
library(classInt)
library(DescTools)
library(reshape)
library(cluster)
library(plotly)
#library(ape)
#library(WVPlots)




####################################################################
####################### Load data 
####################################################################

# Admin borders
LaoAdmin0_WGS84 <- readShapePoly("admin areas/LAO_adm0", proj4string = CRS("+init=epsg:4326") ) # country border
LaoAdmin1_WGS84 <- readShapePoly("admin areas/LAO_adm1_correct_names_no_xaisomboun", proj4string = CRS("+init=epsg:4326")) # province border
LaoAdmin2_WGS84 <- readShapePoly("admin areas/LAO_adm2_correct_names", proj4string = CRS("+init=epsg:4326")) # district border
####################### transform to UTM 48N
LaoAdmin0 <- spTransform(LaoAdmin0_WGS84, CRS=CRS("+init=epsg:32648"))
LaoAdmin1 <- spTransform(LaoAdmin1_WGS84, CRS=CRS("+init=epsg:32648"))
LaoAdmin2 <- spTransform(LaoAdmin2_WGS84, CRS=CRS("+init=epsg:32648"))

rm('LaoAdmin0_WGS84','LaoAdmin1_WGS84','LaoAdmin2_WGS84')

# Map background
mapBG <- readShapePoly("MapBG", proj4string = CRS("+init=epsg:4326") ) # background
mapBG <- spTransform(mapBG, CRS=CRS("+init=epsg:32648"))
####################### Load WPI data
WPIdata <- readShapePoints("WPIdata2", proj4string=CRS("+init=epsg:32648")) # to process
WPIdata_orig <- readShapePoints("WPIdata2", proj4string=CRS("+init=epsg:32648")) # to keep the original values






##########################################
#### SOURCE other .r files
#########################################
source('aux_functions.r', encoding = 'UTF-8')
source('preprocess.r', encoding = 'UTF-8')







#############################################
######################## Calculate components
#############################################
# Resource
#WPIresource <- (WPIdata$DrySoil + WPIdata$WetSoil + WPIdata$DryAvail+ WPIdata$WetAvail + WPIdata$AvMaxDDay)/5
#WPIdata$RES <- WPIresource
# Access
#WPIaccess <- (WPIdata$Irrigation + WPIdata$DrinkNS + WPIdata$ToiletType)/3
#WPIdata$ACC <- WPIaccess
#Capacity
#WPIcapacity <-(WPIdata$TimeCap + WPIdata$RoadAcc + WPIdata$LitPopSh + WPIdata$IncPov)/4
#WPIdata$CAP <- WPIcapacity
# Use
#WPIuse <- (WPIdata$IrrAreaSh + WPIdata$AgAreaPerC + WPIdata$AgAqDepend)/3
#WPIdata$USE <- WPIuse
# Environment
#WPIenvironment <- (WPIdata$ECO_V_cat + WPIdata$Disaster + WPIdata$SoilDeg)/3
#WPIdata$ENV <- WPIenvironment
# Total WPI score --- no weighting
#WPIscore <- (WPIresource + WPIaccess + WPIcapacity + WPIuse + WPIenvironment)/5
#WPIdata$WPI <- WPIscore



########################## Calculate dry season WPI
# Resource
WPIresource <- (WPIdata$DryAvail + WPIdata$DryPrec + WPIdata$AvMaxDDay) /3
WPIdata$dryRES <- WPIresource
# Access
WPIaccess <- (WPIdata$Irrigation + WPIdata$DrinkNS + WPIdata$ToiletType)/3
WPIdata$dryACC <- WPIaccess
#Capacity
WPIcapacity <- (WPIdata$TimeCap + WPIdata$DryRoad + WPIdata$LitPopSh + WPIdata$IncPov)/4
WPIdata$dryCAP <- WPIcapacity
# Use
WPIuse <- (WPIdata$ShDryIrr + WPIdata$AgAreaPerC + WPIdata$AgAqDepend)/3
WPIdata$dryUSE <- WPIuse
# Environment
WPIenvironment <- (WPIdata$ECO_V_cat + WPIdata$dryDisast + WPIdata$SoilDeg + WPIdata$HumanFP)/4
WPIdata$dryENV <- WPIenvironment
# Total dry WPI score --- no weighting
WPIscore <- (WPIresource + WPIaccess + WPIcapacity + WPIuse + WPIenvironment)/5
WPIdata$dryWPI <- WPIscore


########################## Calculate wet season WPI
# Resource
WPIresource <- (WPIdata$WetAvail + WPIdata$WetPrec + 100)/3
WPIdata$wetRES <- WPIresource
# Access
WPIaccess <- (WPIdata$Irrigation + WPIdata$DrinkNS + WPIdata$ToiletType)/3
WPIdata$wetACC <- WPIaccess
#Capacity
WPIcapacity <- (WPIdata$TimeCap + WPIdata$WetRoad + WPIdata$LitPopSh + WPIdata$IncPov)/4
WPIdata$wetCAP <- WPIcapacity
# Use
WPIuse <- (WPIdata$IrrAreaSh + WPIdata$AgAreaPerC + WPIdata$AgAqDepend)/3
WPIdata$wetUSE <- WPIuse
# Environment
WPIenvironment <- (WPIdata$ECO_V_cat + WPIdata$wetDisast + WPIdata$SoilDeg + WPIdata$HumanFP)/4
WPIdata$wetENV <- WPIenvironment
# Total wet WPI score --- no weighting
WPIscore <- (WPIresource + WPIaccess + WPIcapacity + WPIuse + WPIenvironment)/5
WPIdata$wetWPI <- WPIscore

rm('WPIresource','WPIaccess','WPIcapacity','WPIuse','WPIenvironment','WPIscore')

########################## Calculate geometric mean WPI
gmeandry <- apply(cbind(WPIdata$dryRES, WPIdata$dryACC, WPIdata$dryCAP, WPIdata$dryUSE, WPIdata$dryENV), 1, Gmean)
gmeanwet <- apply(cbind(WPIdata$wetRES, WPIdata$wetACC, WPIdata$wetCAP, WPIdata$wetUSE, WPIdata$wetENV), 1, Gmean)
#gmeanwpi <- apply(cbind(WPIdata$RES, WPIdata$ACC, WPIdata$CAP, WPIdata$USE, WPIdata$ENV), 1, Gmean)
WPIdata$gmeanDryWPI <- gmeandry
WPIdata$gmeanWetWPI <- gmeanwet
#WPIdata$gmeanWPI <- gmeanwpi

rm('gmeandry', 'gmeanwet')

# collect components into data.frames
#WPIcomp <- cbind(WPIdata$WPI,WPIdata$RES,WPIdata$ACC,WPIdata$CAP,WPIdata$USE,WPIdata$ENV)
#colnames(WPIcomp) <- c('WPI','RES','ACC','CAP','USE','ENV' )
dryWPIcomp <- as.data.frame(cbind(WPIdata$dryWPI, WPIdata$dryRES,WPIdata$dryACC,WPIdata$dryCAP,WPIdata$dryUSE,WPIdata$dryENV))
colnames(dryWPIcomp) <- c('dryWPI','dryRES','dryACC','dryCAP','dryUSE','dryENV' )
wetWPIcomp <-  as.data.frame(cbind(WPIdata$wetWPI, WPIdata$wetRES,WPIdata$wetACC,WPIdata$wetCAP,WPIdata$wetUSE,WPIdata$wetENV))
colnames(wetWPIcomp) <- c('wetWPI','wetRES','wetACC','wetCAP','wetUSE','wetENV' )

# change 0-scores to 0.01 so that geometric mean does not equal to zero
temp <- dryWPIcomp[,5]
temp[temp==0] <- 0.01
dryWPIcomp[,5] <- temp
temp <- wetWPIcomp[,5]
temp[temp==0] <- 0.01
wetWPIcomp[,5] <- temp

rm('temp')

# collect all components together
#WPIscores <- cbind(WPIcomp,dryWPIcomp,wetWPIcomp)
WPIscores <- cbind(dryWPIcomp,wetWPIcomp)
#scaledScores <- scale(WPIscores)







###########################################
############## Global PCA weights for components
###########################################

# Perform PCA for dry season
PCA <- prcomp(dryWPIcomp[,2:6], scale=F) # no scaling - the components are already scaled 0-100

sqroots <- c(  sqrt(abs(PCA$rotation[1,1])) , sqrt(abs(PCA$rotation[2,2])),  sqrt(abs(PCA$rotation[3,3])),  sqrt(abs(PCA$rotation[4,4])),  sqrt(abs(PCA$rotation[5,5]))  )
weights <- sqroots/sum(sqroots)


# Total dry WPI score
WPIscore <- dryWPIcomp[,2]*weights[1] + dryWPIcomp[,3]*weights[2] + dryWPIcomp[,4]*weights[3] + dryWPIcomp[,5]*weights[4] + dryWPIcomp[,6]*weights[5]
WPIdata$dryWPIsinglepca <- WPIscore

# total dry WPI gmean score
dryWPImult <- (dryWPIcomp[,2]^weights[1] * dryWPIcomp[,3]^weights[2] * dryWPIcomp[,4]^weights[3] * dryWPIcomp[,5]^weights[4] * dryWPIcomp[,6]^weights[5])^(1/sum(weights))
WPIdata$dryWPIsinglegpca <- dryWPImult

# calculate wet WPI using dry season weights
wetWPImult <- (wetWPIcomp[,2]^weights[1] * wetWPIcomp[,3]^weights[2] * wetWPIcomp[,4]^weights[3] * wetWPIcomp[,5]^weights[4] * wetWPIcomp[,6]^weights[5])^(1/sum(weights))
WPIdata$wetWPIsingledrygpca <- wetWPImult

globalPCAweights <- weights

###################################################

# Perform PCA again for wet season
PCA <- prcomp(wetWPIcomp[,2:6], scale=F) # no scaling - the components are already scaled 0-100

sqroots <- c(  sqrt(abs(PCA$rotation[1,1])) , sqrt(abs(PCA$rotation[2,2])),  sqrt(abs(PCA$rotation[3,3])),  sqrt(abs(PCA$rotation[4,4])),  sqrt(abs(PCA$rotation[5,5]))  )
weights <- sqroots/sum(sqroots)

# Total wet WPI score
WPIscore <- wetWPIcomp[,2]*weights[1] + wetWPIcomp[,3]*weights[2] + wetWPIcomp[,4]*weights[3] + wetWPIcomp[,5]*weights[4] + wetWPIcomp[,6]*weights[5]
WPIdata$wetWPIsinglepca <- WPIscore

wetWPImult <- (wetWPIcomp[,2]^weights[1] * wetWPIcomp[,3]^weights[2] * wetWPIcomp[,4]^weights[3] * wetWPIcomp[,5]^weights[4] * wetWPIcomp[,6]^weights[5])^(1/sum(weights))
WPIdata$wetWPIsinglegpca <- wetWPImult

# calculate dry WPI using wet season weights
dryWPImult <- (dryWPIcomp[,2]^weights[1] * dryWPIcomp[,3]^weights[2] * dryWPIcomp[,4]^weights[3] * dryWPIcomp[,5]^weights[4] * dryWPIcomp[,6]^weights[5])^(1/sum(weights))
WPIdata$dryWPIsinglewetgpca <- dryWPImult



globalPCAweights <- cbind(globalPCAweights, weights)


###################################################

# Perform PCA again for Both season
temp <- rbind(dryWPIcomp[,2:6], wetWPIcomp[,2:6])
PCA <- prcomp(temp, scale=F) # no scaling - the components are already scaled 0-100

sqroots <- c(  sqrt(abs(PCA$rotation[1,1])) , sqrt(abs(PCA$rotation[2,2])),  sqrt(abs(PCA$rotation[3,3])),  sqrt(abs(PCA$rotation[4,4])),  sqrt(abs(PCA$rotation[5,5]))  )
weights <- sqroots/sum(sqroots)

globalPCAweights <- cbind(globalPCAweights, weights)
colnames(globalPCAweights) <- c('Dry','Wet','Both')
rownames(globalPCAweights) <- c('RES','ACC','CAP','USE','ENV')

##### Plot loadings dumbell
varnames <- c('RES','ACC','CAP','USE','ENV')
loadings1 <- data.frame(cbind(varnames,as.numeric(PCA$rotation[,1])))
loadings1[,2] <- as.numeric(PCA$rotation[,1])
loadings2 <- data.frame(cbind(varnames,as.numeric(PCA$rotation[,2])))
loadings2[,2] <- as.numeric(PCA$rotation[,2])
loadings3 <- data.frame(cbind(varnames,as.numeric(PCA$rotation[,3])))
loadings3[,2] <- as.numeric(PCA$rotation[,3])


p1 <- ggplot(loadings1, aes(x = 0, xend = V2, y = varnames, yend=varnames, fill=varnames)) + geom_point(aes(V2, varnames), size=3) + geom_segment() + ggtitle('PC1 loadings') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none")
p2 <- ggplot(loadings2, aes(x = 0, xend = V2, y = varnames, yend=varnames, fill=varnames)) + geom_point(aes(V2, varnames), size=3) + geom_segment() + ggtitle('PC2 loadings') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none")
p3 <- ggplot(loadings3, aes(x = 0, xend = V2, y = varnames, yend=varnames, fill=varnames)) + geom_point(aes(V2, varnames), size=3) + geom_segment() + ggtitle('PC3 loadings') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none")
multiplot(p1,p2,p3, cols=3)

rm('PCA','sqroots','weights','varnames','loadings1','loadings2','loadings3','p1','p2','p3','temp')

############### calculate WPI with both-weights

weights <- globalPCAweights[,3]
# Total dry WPI score
WPIscore <- dryWPIcomp[,2]*weights[1] + dryWPIcomp[,3]*weights[2] + dryWPIcomp[,4]*weights[3] + dryWPIcomp[,5]*weights[4] + dryWPIcomp[,6]*weights[5]
WPIdata$dryWPIpca <- WPIscore

# Total wet WPI score
WPIscore <- wetWPIcomp[,2]*weights[1] + wetWPIcomp[,3]*weights[2] + wetWPIcomp[,4]*weights[3] + wetWPIcomp[,5]*weights[4] + wetWPIcomp[,6]*weights[5]
WPIdata$wetWPIpca <- WPIscore

dryWPImult <- (dryWPIcomp[,2]^weights[1] * dryWPIcomp[,3]^weights[2] * dryWPIcomp[,4]^weights[3] * dryWPIcomp[,5]^weights[4] * dryWPIcomp[,6]^weights[5])^(1/sum(weights))
wetWPImult <- (wetWPIcomp[,2]^weights[1] * wetWPIcomp[,3]^weights[2] * wetWPIcomp[,4]^weights[3] * wetWPIcomp[,5]^weights[4] * wetWPIcomp[,6]^weights[5])^(1/sum(weights))

WPIdata$dryWPIgpca <- dryWPImult
WPIdata$wetWPIgpca <- wetWPImult

rm('WPIscore','dryWPImult','wetWPImult','weights')










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

# example on how to add variables to the dataframe. both must have same id name!
# temp2 <- merge(voronoi.f,temp, by = 'id')
# then plot with ggplot2
# ggplot(temp2, aes(long, lat, group = group)) + geom_polygon(aes(fill=dry))+ theme_bw() + scale_fill_gradient(low='red', high='blue') + coord_fixed()

sample <- sample(8215,2000)
# create a sample
WPIdata_sample <- WPIdata_orig[sample,]
WPIdata_sample$dryWPIgpca <- WPIdata$dryWPIgpca[sample]
WPIdata_sample$wetWPIgpca <- WPIdata$wetWPIgpca[sample]

# calculate distance matrices
distMat <- gw.dist(dp.locat = cbind(as.numeric(WPIdata_orig@coords[,1]), as.numeric(WPIdata_orig@coords[,2])))
distMat.grid <- gw.dist(dp.locat = cbind(as.numeric(WPIdata_orig@coords[,1]), as.numeric(WPIdata_orig@coords[,2])), rp.locat = s.grid@coords)
distMat.sample <-  gw.dist(dp.locat = cbind(as.numeric(WPIdata_sample@coords[,1]), as.numeric(WPIdata_sample@coords[,2])))


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










########################################################################
############### Description of Laos
#######################################################################

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



temp <- gCentroid(LaoAdmin1,byid=TRUE)
temp <- data.frame(temp@coords[,1], temp@coords[,2], LaoAdmin1$NAME_1)
colnames(temp) <- c('x','y','name')

bmap <- ggplot(LaoAdmin2.f, aes(x = long, y = lat)) + geom_polygon(aes(group = group), colour = I("grey90"), size=0.2, fill = "white") + coord_equal() 
bmap <- bmap + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = I("grey30"), size=0.2, fill = NA) + coord_equal() 
bmap <- bmap + geom_text(data=temp, aes(x = x,y = y, label=name))+ theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
bmap










########################################################################
############### Initial dataset exploration
#######################################################################

#plot village numbers per province
temp <- table(WPIdata$bcne)
temp <- as.data.frame(temp)
ggplot(temp) + geom_bar(aes(Var1,Freq), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('Number of Villages')
rm('temp')

# plot dot density map of the villages


bmap <- ggplot(LaoAdmin1.f, aes(x = long, y = lat)) + geom_polygon(aes(group = group), colour = I("grey65"), size=0.2, fill = "white") + coord_equal() 
map <- bmap + geom_point(data=as.data.frame(coordinates(WPIdata)), aes(x=coords.x1,y=coords.x2), size=0.8)+ theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
dtop <- ggplot()+geom_line(aes(as.data.frame(coordinates(WPIdata)[,1])), stat='density')+ theme_bw() + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
dright <- ggplot()+geom_line(aes(as.data.frame(coordinates(WPIdata)[,1])), stat='density') + coord_flip()+ theme_bw() + theme(axis.line=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
empty <- ggplot()+geom_point(aes(1,1), colour="white") + theme(axis.ticks=element_blank(), panel.background=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

grid.arrange(dtop, empty, map, dright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

rm('bmap','map')





#RES
# dry season
temp <- as.data.frame(dryRESvar)
colnames(temp) <- c('Dry Season \n Surface Availability','Dry Season \n Precipitation','Consecutive Drought Days') 
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp)
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('a) Dry Season RES')
# wet season
temp <- as.data.frame(cbind(wetRESvar, rep.int(100,8215)))
colnames(temp) <- c('Wet Season \n Surface Availability','Wet Season \n Precipitation','Consecutive Drought Days') 
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Wet Season')
temp <- melt(temp)
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('b) Wet Season RES') 
multiplot(p1,p2, cols=2)

# global Moran's I
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




#ACC
temp <- as.data.frame(ACCvar)
colnames(temp) <- c('Irrigation Type','Drinking Water Source','Toilet Type')
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth) 
temp <- melt(temp)
ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('ACC')
# global Moran's I
temp <- componentMoran2(ACCvar, voronoi.nb.listw)
moranTable <- data.frame(temp)




#CAP
temp <- as.data.frame(dryCAPvar)
colnames(temp) <- c('Travel Time to Capital','Road Access','Literacy Rate','Incidence of Poverty') 
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
temp <- melt(temp)
ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('Dry Season CAP')
# global Moran's I
temp <- componentMoran2(dryCAPvar, voronoi.nb.listw)
temp2 <- componentMoran2(wetCAPvar, voronoi.nb.listw)
moranTable <- data.frame(rbind(temp, temp2))
#rownames(moranCAP) <- c('Travel Time to Capital','(Dry) Road Access','Literacy Rate','Incidence of Poverty','Travel Time to Capital','(Wet) Road Access','Literacy Rate','Incidence of Poverty') 
rm('temp','temp2')

# plot villages with less than 100 score on surface water availability
selection <- WPIdata$DryRoad < 100
selection.f <- fortify(voronoi[selection,], region='ID')
p1 <- ggplot(LaoAdmin1.f, aes(x = long, y = lat)) + geom_polygon(aes(group = group), colour = I("grey65"), size=0.2, fill = "white") + coord_equal() + geom_polygon(data=selection.f, aes(group=group), fill="red") + theme_bw() + ggtitle('a) Villages without road access in the dry season')

selection <- WPIdata$WetRoad < 100
selection.f <- fortify(voronoi[selection,], region='ID')
p2 <- ggplot(LaoAdmin1.f, aes(x = long, y = lat)) + geom_polygon(aes(group = group), colour = I("grey65"), size=0.2, fill = "white") + coord_equal() + geom_polygon(data=selection.f, aes(group=group), fill="orange") + theme_bw() + ggtitle('b) Villages without road access in the wet season')

multiplot(p1,p2,cols=2)

rm('selection','selection.f','p1','p2')







#USE
temp <- as.data.frame(dryUSEvar)
colnames(temp) <- c('Irrigation Rate','Agr. Area per Capita','Pop. Rate Depending on Water') 
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
temp <- melt(temp)
ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('Dry Season USE')
# dry vs wet irrigation
temp <- as.data.frame(cbind(WPIdata$IrrAreaSh, WPIdata$ShDryIrr))
colnames(temp) <- c('WetIrrigation','DryIrrigation')
p1 <- ggplot(data = temp) + geom_point(aes(WetIrrigation, DryIrrigation)) + geom_abline(color='red') + theme_bw() + ggtitle('Seasonal Irrigation')
temp <- melt(temp)
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('Irrigation Scores')
multiplot(p1,p2, cols=2)
# global Moran's I
temp <- componentMoran2(dryUSEvar, voronoi.nb.listw)
temp2 <- componentMoran2(wetUSEvar, voronoi.nb.listw)
moranTable <- data.frame(rbind(temp, temp2))
#rownames(moranUSE) <- c('Irrigation Rate','Agr. Area per Capita','Pop. Rate Depending on Water') 
rm('temp','p1','p2')








#ENV
# dry season
temp <- as.data.frame(dryENVvar)
colnames(temp) <- c('Threatened \n Amphibians','Disasters','Soil \n Degradation','Human \n Footprint') 
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
temp <- melt(temp)
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('a) Dry Season ENV')
# wet season
temp <- as.data.frame(cbind(WPIdata$dryDisast, WPIdata$wetDisast))
colnames(temp) <- c('Dry season \n disasters','Wet season \n disasters')
pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
temp <- melt(temp)
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(variable))) + geom_jitter(position=position_jitter(width=.6), aes(color=factor(variable)) ,pch='.') + geom_boxplot(outlier.shape=NA) + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none") + ggtitle('b) Dry and Wet Season Disasters')
multiplot(p1,p2,cols=2)
# global Moran's I
temp <- componentMoran2(dryENVvar, voronoi.nb.listw)
temp2 <- componentMoran2(wetENVvar, voronoi.nb.listw)
moranTable <- data.frame(rbind(temp, temp2))
#rownames(moranENV) <- c('Threatened Amphibians','(Dry) Disasters','Soil Degradation','Human Footprint','Threatened Amphibians','(Wet) Disasters','Soil Degradation','Human Footprint')
rm('temp','temp2', 'p1', 'p2')












###########################################################################
############## GWSS
###########################################################################

# on a grid of 2000 points

# components
dryWPI_gwss_400NN_comp <- gwss(WPIdata, s.grid, vars=c("dryWPIgpca", "dryRES", "dryACC", "dryCAP", "dryUSE", "dryENV"), bw=400, adaptive=T, quantile=F, dMat=distMat.grid)
wetWPI_gwss_400NN_comp <- gwss(WPIdata, s.grid, vars=c("wetWPIgpca", "wetRES", "wetACC", "wetCAP", "wetUSE", "wetENV"), bw=400, adaptive=T, quantile=F, dMat=distMat.grid)

# variables
dryWPI_gwss_400NN_vars <- gwss(WPIdata, s.grid, vars=c("dryWPIgpca", "DryAvail","AvMaxDDay", "Irrigation", "DrinkNS", "ToiletType", "TimeCap", "DryRoad", "LitPopSh", "IncPov", "IrrAreaSh", "AgAreaPerC", "AgAqDepend", "ECO_V_cat", "dryDisast", "SoilDeg","HumanFP"), bw=400, adaptive=T, quantile=F, dMat=distMat.grid)
wetWPI_gwss_400NN_vars <- gwss(WPIdata, s.grid, vars=c("wetWPIgpca", "WetAvail", "Irrigation", "DrinkNS", "ToiletType", "TimeCap", "WetRoad", "LitPopSh", "IncPov", "IrrAreaSh", "AgAreaPerC", "AgAqDepend", "ECO_V_cat", "wetDisast", "SoilDeg", "HumanFP"), bw=400, adaptive=T, quantile=F, dMat=distMat.grid)













#######################################################################
#################### Exploration of WPI
#######################################################################

################### dry season component maps FOR APPENDIX
par(mfrow=c(1,2),mar=c(9.5,10,1,2))
# Resources
shading <- auto.shading(WPIdata$dryRES, cols = brewer.pal(9,"Blues"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$dryRES, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Dry season RES')

# Access
shading <- auto.shading(WPIdata$dryACC, cols = brewer.pal(9,"Greens"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$dryACC, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Dry season ACC')

# Capacity
shading <- auto.shading(WPIdata$dryCAP, cols = brewer.pal(9,"Purples"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$dryCAP, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Dry season CAP')

# Use
shading <- auto.shading(WPIdata$dryUSE, cols = brewer.pal(9,"Oranges"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$dryUSE, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Dry season USE')

# Environment
shading <- auto.shading(WPIdata$dryENV, cols = brewer.pal(9,"Greys"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$dryENV, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Dry season ENV')

# Total
shading <- auto.shading(WPIdata$dryWPI, cols = brewer.pal(9,"Reds"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$dryWPI, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Dry season WPI')



########## FOR TEXT 
par(mfrow=c(1,2), mar=c(1,3,1,1))
############ Local mean

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




###### Plot dry season ranks // GGPLOT2
temp <- data.frame(WPIdata$wid, rank(WPIdata$dryWPIgpca))
colnames(temp) <- c('id','Rank')
View(temp)
temp2 <- merge(voronoi.f,temp, by='id')

map <- ggplot(temp2, aes(x = long, y = lat, fill = Rank)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low='red', mid='yellow', high='blue', midpoint=4108) + theme_bw()
map <- map + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Dry season WPI ranks') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
map

rm('temp', 'temp2','map')




# Calculate autocorrelation 
temp <- componentMoran2(dryWPIcomp, voronoi.nb.listw)
moranTable <- data.frame(temp)
rm('temp','moranTable')






#########plot WPI density per province
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




# perform an anova test for the means
temp <- pairwise.t.test(WPIdata$dryWPIgpca,Provinces,  p.adj = "none")




###### Local moran
temp <- localmoran(dryWPIcomp[,1], voronoi.nb.listw)
temp2 <- data.frame(WPIdata$wid, temp[,1])
colnames(temp2) <- c('id','LocalMoran')
temp3 <- merge(voronoi.f,temp2, by='id')

map <- ggplot(temp3, aes(x = long, y = lat, fill = LocalMoran)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low='red', mid='white', high='blue', midpoint=0) + theme_bw()
map <- map + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle("Dry season WPI Local Moran's I") + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
map

rm('temp', 'temp2','temp3','map')





################## wet season FOR APPENDIX
# Resources
shading <- auto.shading(WPIdata$wetRES, cols = brewer.pal(9,"Blues"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$wetRES, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Wet season RES')

# Access
shading <- auto.shading(WPIdata$wetACC, cols = brewer.pal(9,"Greens"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$wetACC, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Wet season ACC')

# Capacity
shading <- auto.shading(WPIdata$wetCAP, cols = brewer.pal(9,"Purples"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$wetCAP, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Wet season CAP')

# Use
shading <- auto.shading(WPIdata$wetUSE, cols = brewer.pal(9,"Oranges"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$wetUSE, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Wet season USE')

# Environment
shading <- auto.shading(WPIdata$wetENV, cols = brewer.pal(9,"Greys"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$wetENV, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Wet season ENV')

# Total
shading <- auto.shading(WPIdata$wetWPI, cols = brewer.pal(9,"Reds"))
plot(LaoAdmin0, col=NA, border=NA)
choropleth(voronoi, WPIdata$wetWPI, shading=shading, border=NA, add=T)
plotBG()
choro.legend(px='bottomleft', sh=shading)
title('Wet season WPI')




########## FOR TEXT 
par(mfrow=c(1,2), mar=c(1,3,1,1))
############ Local mean

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
title("Wet season RES")

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
title("Wet season ACC")

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
title("Wet season CAP")

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
title("Wet season USE")

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
title("Wet season ENV")

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
title("Wet season WPI")




###### Plot wet season ranks // GGPLOT2
temp <- data.frame(WPIdata$wid, rank(WPIdata$wetWPIgpca))
colnames(temp) <- c('id','Rank')
View(temp)
temp2 <- merge(voronoi.f,temp, by='id')

map <- ggplot(temp2, aes(x = long, y = lat, fill = Rank)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low='red', mid='yellow', high='blue', midpoint=4108) + theme_bw()
map <- map + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Wet season WPI ranks') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
map

rm('temp', 'temp2','map')


# autocorrelation
temp <- componentMoran2(wetWPIcomp, voronoi.nb.listw)
moranTable <- data.frame(temp)
rm('temp','moranTable')






#plot WPI density per province
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
################ Comparing seasons
#################################################



########### PLOT WPI from different pca weights
# par(mfrow=c(2,2),mar=c(1,6,1,0))
#single season pca
#dry
# shading <- auto.shading(c(WPIdata$wetWPIsinglegpca,WPIdata$dryWPIsinglegpca) , cols = brewer.pal(9,"Reds"))
# plot(LaoAdmin0, col=NA, border=NA)
# choropleth(voronoi, WPIdata$dryWPIsinglegpca, shading=shading, border=NA, add=T)
# plotBG()
# title('Dry season, single season PCA')
# choro.legend(px='bottomleft', sh=shading, title='WPI')

#wet
# plot(LaoAdmin0, col=NA, border=NA)
# choropleth(voronoi, WPIdata$wetWPIsinglegpca, shading=shading, border=NA, add=T)
# plotBG()
# title('Wet season, single season PCA')
# choro.legend(px='bottomleft', sh=shading, title='WPI')


#both season pca
#dry
# shading <- auto.shading( c(WPIdata$wetWPIgpca, WPIdata$dryWPIgpca) , cols = brewer.pal(9,"Greys"))
# plot(LaoAdmin0, col=NA, border=NA)
# choropleth(voronoi, WPIdata$dryWPIgpca, shading=shading, border=NA, add=T)
# plotBG()
# title('Dry season, both seasons PCA')
# choro.legend(px='bottomleft', sh=shading, title='WPI')

#wet
# plot(LaoAdmin0, col=NA, border=NA)
# choropleth(voronoi, WPIdata$wetWPIgpca, shading=shading, border=NA, add=T)
# plotBG()
# title('Wet season, both seasons PCA')
# choro.legend(px='bottomleft', sh=shading, title='WPI')



###### comparison // GGPLOT2
temp <- data.frame(WPIdata$wid, WPIdata$dryWPIsinglegpca, WPIdata$wetWPIsinglegpca, WPIdata$dryWPIgpca, WPIdata$wetWPIgpca)
colnames(temp) <- c('id','SingleDry','SingleWet','BothDry','BothWet')
View(temp)
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
rm('map1','map3','map2','map4')





######### PLOT pca weighting scheme difference


# par(mfrow=c(1,2),mar=c(6,6,1,0))
# # dry
# shading <- auto.shading(WPIdata$dryWPIgpca-WPIdata$dryWPIsinglegpca , cols = brewer.pal(9,"Reds"))
# plot(LaoAdmin0, col=NA, border=NA)
# choropleth(voronoi, WPIdata$dryWPIgpca-WPIdata$dryWPIsinglegpca, shading=shading, border=NA, add=T)
# plotBG()
# title('Dry season difference between weighting schemes')
# choro.legend(px='bottomleft', sh=shading, title='WPI')

# # wet
# shading <- auto.shading(WPIdata$wetWPIgpca-WPIdata$wetWPIsinglegpca , cols = brewer.pal(9,"Blues"))
# plot(LaoAdmin0, col=NA, border=NA)
# choropleth(voronoi, WPIdata$wetWPIgpca-WPIdata$wetWPIsinglegpca, shading=shading, border=NA, add=T)
# plotBG()
# title('Wet season difference between weighting schemes')
# choro.legend(px='bottomleft', sh=shading, title='WPI')


map1 <- ggplot(temp2, aes(x = long, y = lat, fill = (BothDry-SingleDry))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#b35806", mid="#f7f7f7", high="#542788", name='WPI difference') + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season difference between weighting schemes') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = (BothWet-SingleWet))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#b35806", mid="#f7f7f7", high="#542788", name='WPI difference') + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season difference between weighting schemes') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map1,map2,cols=2)

rm('map1','map2')


####### PLOT wet season - dry season difference (WPI gmean pca)
# par(mfrow=c(1,2))
# quick.map(WPIdata$wetWPIgpca-WPIdata$dryWPIgpca,9,"Blues")
# title('Difference between Wet and Dry season')


map1 <- ggplot(temp2, aes(x = long, y = lat, fill = (BothWet-BothDry))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#b35806", mid="#ffffbf", high="#542788", name='WPI difference') + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Difference between seasonal WPI') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

map1
rm('map1','temp','temp2')






####### WPI per province

#plot village numbers per province
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
#sort
provinceWPI$Province <- factor(provinceWPI$Province, levels = provinceWPI$Province[order(provinceWPI$WetWPI)])

rm('temp','tempnames','temppro','tempdry','tempwet','dryrank','wetrank')
p1 <- ggplot(provinceWPI, aes(x = Province, xend=Province, y = DryWPI, yend=WetWPI, fill=Province)) + geom_segment() + geom_point(aes(x=Province, y=DryWPI, color='blue'), size=3) + geom_point(aes(x=Province, y=WetWPI, color='red'), size=3) + ggtitle('a) Seasonal WPI differences') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y= 'WPI', x='')

#sort
provinceWPI$Province <- factor(provinceWPI$Province, levels = provinceWPI$Province[order(provinceWPI$WetRank)])
p2 <- ggplot(provinceWPI, aes(x = Province, xend=Province, y = DryRank, yend=WetRank, fill=Province)) + geom_segment() + geom_point(aes(x=Province, y=DryRank, color='blue'), size=3) + geom_point(aes(x=Province, y=WetRank, color='red'), size=3) + ggtitle('b) Seasonal Rank differences') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y= 'WPI', x='')
multiplot(p1,p2,cols=1)

rm('p1','p2')




# Plot
ux <- unique(s.grid@coords[,1])
uy <- unique(s.grid@coords[,2])
par(mfrow=c(1,2), mar=c(1,1,1,1))

############ Local mean
breaks <- classIntervals(c(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LM, wetWPI_gwss_400NN_comp$SDF$wetWPIgpca_LM), n=5, style='quantile')
colours <- brewer.pal(6, 'Purples')
# dry LM
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LM, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("a) Dry season WPI; GW Mean")
# wet LM
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
# dry LM
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LSD, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1) )
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("c) Dry season WPI; Standard Deviation")
# wet LM
predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetWPIgpca_LSD, length(ux), length(uy) )
plot(LaoAdmin0, border=NA, col=NA)
.filled.contour(ux,uy, predmat, col=colours, levels=round(c(0,breaks$brks[1:5],100), digits=1))
plotBG()
sh <- shading(breaks=round(breaks$brks[1:5], digits=1), cols=colours)
choro.legend(px='bottomleft', sh=sh, bg='white')
title("d) Wet season WPI; Standard Deviation")



# ggplot2 alternative
predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LM, length(ux), length(uy))
colnames(predmat) <- uy
rownames(predmat) <- ux
temp <- melt.array(predmat)

map1 <- ggplot(temp, aes(x=X1, y=X2)) + geom_raster(aes(fill=value),interpolate=TRUE) + coord_equal() + geom_polygon(data=mapBG.f, aes(x=long, y=lat, group=id), fill="white", color="white") + theme_bw() + ggtitle('a) Dry season WPI; GW Mean') + geom_polygon(data=LaoAdmin1.f, aes(x=long, y=lat, group=group), fill=NA, color="black", size=0.2) + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=50, limits=c(0,100))

predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetWPIgpca_LM, length(ux), length(uy) )
colnames(predmat) <- uy
rownames(predmat) <- ux
temp <- melt.array(predmat)

map2 <- ggplot(temp, aes(x=X1, y=X2)) + geom_raster(aes(fill=value),interpolate=TRUE) + coord_equal() + geom_polygon(data=mapBG.f, aes(x=long, y=lat, group=id), fill="white", color="white") + theme_bw() + ggtitle('c) Wet season WPI; GW Mean') + geom_polygon(data=LaoAdmin1.f, aes(x=long, y=lat, group=group), fill=NA, color="black", size=0.2) + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=50, limits=c(0,100))

predmat <- matrix(dryWPI_gwss_400NN_comp$SDF$dryWPIgpca_LSD, length(ux), length(uy) )
colnames(predmat) <- uy
rownames(predmat) <- ux
temp <- melt.array(predmat)

map3 <- ggplot(temp, aes(x=X1, y=X2)) + geom_raster(aes(fill=value),interpolate=TRUE) + coord_equal() + geom_polygon(data=mapBG.f, aes(x=long, y=lat, group=id), fill="white", color="white") + theme_bw() + ggtitle('d) Dry season WPI; GW Standard Deviation') + geom_polygon(data=LaoAdmin1.f, aes(x=long, y=lat, group=group), fill=NA, color="black", size=0.2) + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())  + scale_fill_gradient(low='white', high='black',limits=c(0,15))

predmat <- matrix(wetWPI_gwss_400NN_comp$SDF$wetWPIgpca_LSD, length(ux), length(uy) )
colnames(predmat) <- uy
rownames(predmat) <- ux
temp <- melt.array(predmat)

map4 <- ggplot(temp, aes(x=X1, y=X2)) + geom_raster(aes(fill=value),interpolate=TRUE) + coord_equal() + geom_polygon(data=mapBG.f, aes(x=long, y=lat, group=id), fill="white", color="white") + theme_bw() + ggtitle('d) Wet season WPI; GW Standard Deviation') + geom_polygon(data=LaoAdmin1.f, aes(x=long, y=lat, group=group), fill=NA, color="black", size=0.2) + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())  + scale_fill_gradient(low='white', high='black',limits=c(0,15))

multiplot(map1,map2,map3,map4, cols=2)

rm('map1', 'map2', 'map3', 'map4')












#################################################
################ Scatterplots
#################################################

temp <- as.data.frame(cbind(as.data.frame(WPIdata$dryWPIgpca), as.data.frame(WPIdata$wetWPIgpca), as.factor(WPIdata$bcne), as.factor(WPIdata$tcne),as.factor(WPIdata$tcne)))
colnames(temp) <- c('Dry_WPI', 'Wet_WPI','Province','District','Village')
ggplot(data = temp, aes(Wet_WPI, Dry_WPI)) + geom_point(size=0.3) + geom_smooth(aes(color="Smooth trend")) + geom_abline(aes(colour='Equal WPI', slope=1, intercept=0), size=1) + theme_bw() + ggtitle('Relationship between dry and wet season WPI') +  scale_colour_manual("Legend",values=c("red","blue")) + scale_linetype_manual("Legend",values=c(1,1))



#plot dry wet WPI scatterplots for each province APPENDIX
tempnames <- table(WPIdata$bcne)
tempnames <- as.data.frame(tempnames)
tempnames <- as.character(tempnames[,"Var1"])
for (i in 1:17) {
	tempdata <- temp[,3] == tempnames[i]
	assign(paste0("p",i), ggplot(data = temp[tempdata,]) + geom_point(aes(Wet_WPI, Dry_WPI, colour=District)) + geom_abline(colour='red', size=1) + theme_bw() + ggtitle(tempnames[i]) )
}
#multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17, cols=6)
multiplot(p5, p9, p10, p16, cols=2)
multiplot(p1,p2,p3,p4,p5,p6, cols=3)
multiplot(p7,p8,p9,p10,p11,p12, cols=3)
multiplot(p13,p14,p15,p16,p17, cols=3)

rm('p1','p2','p3','p4','p5','p6','p7','p8','p9','p10','p11','p12','p13','p14','p15','p16','p17')
rm('temp')


# example on how to add variables to the dataframe. both must have same id name!
# temp2 <- merge(voronoi.f,temp, by = 'id')
# then plot with ggplot2
# ggplot(temp2, aes(long, lat, group = group)) + geom_polygon(aes(fill=dry))+ theme_bw() + scale_fill_gradient(low='red', high='blue') + coord_fixed()

################################
center <- ggplot(data = temp[,17]) + geom_point(aes(Wet_WPI, Dry_WPI, colour=District)) + geom_abline(colour='red', size=1) + theme_bw() + ggtitle('Xekong')
dtop <- ggplot()+geom_line(aes(temp[,17])), stat='density')+ theme_bw() + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
dright <- ggplot()+geom_line(aes(as.data.frame(coordinates(WPIdata)[,1])), stat='density') + coord_flip()+ theme_bw() + theme(axis.line=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

empty <- ggplot()+geom_point(aes(1,1), colour="white") + theme(axis.ticks=element_blank(), panel.background=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

grid.arrange(dtop, empty, map, dright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
################################

temp <- data.frame(WPIdata$wid, WPIdata$bcne, WPIdata$tcne, WPIdata$wetWPIgpca- WPIdata$dryWPIgpca)
dryvswet <- subset(temp, temp[,4] <0)



#COMPONENTS
# dry season
temp <- data.frame(dryWPIcomp, Provinces)
colnames(temp) <- c("dryWPI","dryRES", "dryACC", "dryCAP", "dryUSE", "dryENV", "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[Provinces %in% c("Xekong","Oudomxai","Phongsaly","Houaphan"),], id="Province")
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(Province))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Dry Season') + scale_fill_discrete(name="Province")

# Wet season

temp <- data.frame(wetWPIcomp, Provinces)
colnames(temp) <- c("wetWPI","wetRES", "wetACC", "wetCAP", "wetUSE", "wetENV", "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[Provinces %in% c("Xekong","Oudomxai","Phongsaly","Houaphan"),], id="Province")
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(Province))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Wet Season') + scale_fill_discrete(name="Province")

multiplot(p1,p2,cols=1)



#VARIABLES
# dry season
temp <- data.frame(dryVAR, Provinces)
colnames(temp) <- c(colnames(dryVAR), "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[Provinces %in% c("Xekong","Oudomxai","Phongsaly","Houaphan"),], id="Province")
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(Province))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Dry Season') + scale_fill_discrete(name="Province")

# Wet season

temp <- data.frame(wetVAR, Provinces)
colnames(temp) <- c(colnames(wetVAR), "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[Provinces %in% c("Xekong","Oudomxai","Phongsaly","Houaphan"),], id="Province")
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(Province))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Wet Season') + scale_fill_discrete(name="Province")

multiplot(p1,p2,cols=1)



#VARIABLES DISTRICT
# dry season
temp <- data.frame(dryVAR, Districts)
colnames(temp) <- c(colnames(dryVAR),"District") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[Provinces %in% c("Xekong"),], id="District")
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(District))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Dry Season') + scale_fill_discrete(name="Province")

# Wet season

temp <- data.frame(wetVAR, Districts)
colnames(temp) <- c(colnames(wetVAR), "District") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[Provinces %in% c("Xekong"),], id="District")
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(District))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Wet Season') + scale_fill_discrete(name="Province")

multiplot(p1,p2,cols=1)






###########################################################################
############## Parallel Coordinate Plots
###########################################################################

#dry season
temp <- as.data.frame(cbind(dryWPIcomp, as.factor(WPIdata$bcne)))
colnames(temp)[7] <- "Province"
parallelplot(~temp[c(2:6)] | factor(temp[,7]), temp, groups=temp[,7], horizontal.axis=FALSE)
rm('temp')
#wet season
temp <- as.data.frame(cbind(wetWPIcomp, as.factor(WPIdata$bcne)))
colnames(temp)[7] <- "Province"
parallelplot(~temp[c(2:6)] | factor(temp[,7]), temp, groups=temp[,7], horizontal.axis=FALSE)

rm('temp')













###########################################################################
############## Create correlation tables
###########################################################################

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





par(mfrow=c(1,3), mar=c(1,1,1,1))
############ Local Correlations

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



############# Test difference in seasons
temp <- rbind(WPIdata$dryWPIgpca, WPIdata$wetWPIgpca)
temp2 <- rbind(rep("Dry", 8215), rep("Wet", 8215))
temp3 <- pairwise.t.test(temp, temp2)
temp3
rm('temp','temp2','temp3')






###########################################################################
############## Clustering
###########################################################################

# k-means
#determine number of clusters
library(NbClust)
# first scale coordinates 0-100 so its in the same scale as the components. Using sample because otherwise takes too long
scaledy <- WPIdata@coords[,1]
scaledy <- scaledy - min(scaledy)
scaledy <- scaledy/max(scaledy)*100
scaledx <- WPIdata@coords[,2]
scaledx <- scaledx - min(scaledx)
scaledx <- scaledx/max(scaledx)*100

#test the clustering
temp <- data.frame(dryWPIcomp[sample,2:6], scaledy[sample],scaledx[sample])
drynclust <- NbClust(temp, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "all")
temp2 <- data.frame(wetWPIcomp[sample,2:6], scaledy[sample],scaledx[sample])
wetnclust <- NbClust(temp2, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "all")

# temp <- data.frame(dryWPIcomp[sample,2:6], scaledy[sample],scaledx[sample])
# drynclust <- NbClust(temp, distance = "euclidean", min.nc=2, max.nc=15, method = "ward.D2", index = "all")
# temp <- data.frame(dryWPIcomp[sample,2:6])
# drynclust <- NbClust(temp, distance = "euclidean", min.nc=2, max.nc=15, method = "ward.D2", index = "all")


# DRY SEASON CLUSTERING
# cluster k-means with 3 clusters
temp <- data.frame(dryWPIcomp[,2:6], scaledy,scaledx)
tempcluster <- kmeans(temp,3)
tempcluster2 <- kmeans(temp,4)
tempcluster3 <- kmeans(temp,5)
tempcluster4 <- kmeans(temp,6)

clusters <- cbind(tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)

temp <- data.frame(dryWPIcomp[,2:6])
tempcluster <- kmeans(temp,3)
tempcluster2 <- kmeans(temp,4)
tempcluster3 <- kmeans(temp,5)
tempcluster4 <- kmeans(temp,6)

clusters <- cbind(clusters,tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)


# WET SEASON CLUSTERING
# cluster k-means with 3 clusters
temp <- data.frame(wetWPIcomp[,2:6], scaledy,scaledx)
tempcluster <- kmeans(temp,3)
tempcluster2 <- kmeans(temp,4)
tempcluster3 <- kmeans(temp,5)
tempcluster4 <- kmeans(temp,6)

clusters <- cbind(clusters,tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)

temp <- data.frame(wetWPIcomp[,2:6])
tempcluster <- kmeans(temp,3)
tempcluster2 <- kmeans(temp,4)
tempcluster3 <- kmeans(temp,5)
tempcluster4 <- kmeans(temp,6)

clusters <- cbind(clusters,tempcluster$cluster, tempcluster2$cluster, tempcluster3$cluster,tempcluster4$cluster)

# name the columns in the data frame
clusters <- as.data.frame(clusters)
colnames(clusters) <- c('dryS3','dryS4','dryS5','dryS6','dry3','dry4','dry5','dry6','wetS3','wetS4','wetS5','wetS6','wet3','wet4','wet5','wet6')
rm('tempcluster','tempcluster2','tempcluster3','tempcluster4')



# #### plot selected cluster schemes

# #plot the cluster schemes
# par(mfrow=c(1,2),mar=c(9.5,10,1,2))
# brks <- c(1.5,2.5,3.5)
# colours <- brewer.pal(12, "Paired")
# sh <- shading(brks, cols=colours[1:4])
# choropleth(voronoi, clusters$dryS4, shading=sh, border=NA)
# legend(x='bottomleft', fill=colours[1:4], legend=c('Poor North','Poor South','Rich South','Rich North'))
# title('Dry season spatial clustering')

# # rearrange clusters so that they are poor-poor-rich-rich
# temp <- clusters$wetS4*10
# temp[temp==30] <- 1
# temp[temp==10] <- 2
# temp[temp==40] <- 3
# temp[temp==20] <- 4
# sh <- shading(brks, cols=colours[c(5:6,9:10)])
# choropleth(voronoi, temp, shading=sh, border=NA)
# legend(x='bottomleft', fill=colours[c(5:6,9:10)], legend=c('Poor North','Poor South','Rich South','Rich North'))
# title('Wet season spatial clustering')

######  GGPLOT2
# rearrange clusters so that they are placed the same in both maps
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


rm(temp)

#### plot dry clusters 3,4,5,6 FOR APPENDIX

#plot the cluster schemes
par(mfrow=c(1,2),mar=c(9.5,10,1,2))
brks <- c(1.5,2.5)
colours <- brewer.pal(3, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters$dryS3, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3))
title('Dry season, k=3')

brks <- c(1.5,2.5,3.5)
colours <- brewer.pal(4, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters$dryS4, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3,4))
title('Dry season, k=4')

brks <- c(1.5,2.5,3.5,4.5)
colours <- brewer.pal(5, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters$dryS5, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3,4,5))
title('Dry season, k=5')

brks <- c(1.5,2.5,3.5,4.5,5.5)
colours <- brewer.pal(6, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters$dryS6, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3,4,5,6))
title('Dry season, k=6')

#### plot wet clusters 3,4,5,6 FOR APPENDIX

#plot the cluster schemes
par(mfrow=c(1,2),mar=c(9.5,10,1,2))
brks <- c(1.5,2.5)
colours <- brewer.pal(3, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters$wetS3, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3))
title('Wet season, k=3')

brks <- c(1.5,2.5,3.5)
colours <- brewer.pal(4, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters$wetS4, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3,4))
title('Wet season, k=4')

brks <- c(1.5,2.5,3.5,4.5)
colours <- brewer.pal(5, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters$wetS5, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3,4,5))
title('Wet season, k=5')

brks <- c(1.5,2.5,3.5,4.5,5.5)
colours <- brewer.pal(6, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters$wetS6, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3,4,5,6))
title('Wet season, k=6')







########## Cluster exploration

#COMPONENTS
# dry season
temp <- data.frame(dryWPIcomp, clusters$dryS4, Provinces)
colnames(temp) <- c("dryWPI","dryRES", "dryACC", "dryCAP", "dryUSE", "dryENV", "Cluster", "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
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

#VARIABLES
#RES
# dry season
temp <- data.frame(dryVAR, clusters$dryS4, Provinces)
colnames(temp) <- c(colnames(dryVAR), "Cluster", "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
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
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[,1:18], id="Cluster")
ggplot(data = temp, aes(variable, value, fill = factor(Cluster))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Wet Season') + scale_fill_manual(name="Cluster", labels=c("Rich North", "Rich South", "Poor South","Poor North"),values=brewer.pal(10,"Paired")[c(5:6,9:10)])






# Plot heatmap

temp <- clusters$wetS4*10
temp[temp==30] <- "Wet_PN"
temp[temp==10] <- "Wet_PS"
temp[temp==40] <- "Wet_RN"
temp[temp==20] <- "Wet_RS"
temp2 <- clusters$dryS4
temp2[temp2==1] <- "Dry_PN"
temp2[temp2==2] <- "Dry_PS"
temp2[temp2==3] <- "Dry_RN"
temp2[temp2==4] <- "Dry_RS"

temp3 <- c(temp2,temp)
temp4 <- c(Provinces,Provinces)
temp <- data.frame(temp3, temp4)
colnames(temp) <- c('Cluster','Province')

tempnames <- table(WPIdata$bcne)
tempnames <- as.data.frame(tempnames)
tempnames <- as.character(tempnames[,"Var1"])
proclust <- vector()
for (i in 1:17) {
	tempname <- tempnames[i]
	temppro <- temp[,'Province']==tempname
	temptable <- table(temp[temppro,1])
	proclust <- rbind(proclust, c(tempname, temptable))
}

proclust <- as.data.frame(proclust)
colnames(proclust) <- c('Province',"Dry_PN","Dry_PS","Dry_RS","Dry_RN","Wet_PN", "Wet_PS","Wet_RS","Wet_RN")

for (i in 2:ncol(proclust)) {
	proclust[,i] <- as.numeric(levels(proclust[,i])[proclust[,i]])
}
attach(proclust)
proclust2 <- data.frame(proclust[,'Province'], (Dry_PS+Dry_PN)/(Dry_PS + Dry_PN + Dry_RS + Dry_RN), (Dry_RS+Dry_RN)/(Dry_PS + Dry_PN + Dry_RS + Dry_RN), (Wet_PS+Wet_PN)/(Wet_PS + Wet_PN + Wet_RS + Wet_RN), (Wet_RS+Wet_RN)/(Wet_PS + Wet_PN + Wet_RS + Wet_RN))
detach(proclust)
colnames(proclust2) <- c('Province', 'Share of dry poor','share of dry rich','share of wet poor', 'share of wet rich')
temp <- melt(proclust[1:5], id="Province")


ggplot(temp, aes(Province,as.numeric(levels(value)[value]), group=variable)) + geom_bar(aes(fill = variable), colour = "white", stat='identity', position=position_dodge(width=3))  + scale_fill_manual(name="Cluster", labels=c("Poor North", "Poor South", "Rich South","Rich North"),values=brewer.pal(4,"Paired"))

ggplot(temp, aes(variable,Province)) + geom_tile(aes(fill = as.numeric(levels(value)[value])), colour = "white")  + scale_fill_gradient(name="Count", low='blue',high='red')






############## Rank clustering

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

#drynclust.rank <- NbClust(tempdry, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "all")
#wetnclust.rank <- NbClust(tempwet, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans", index = "all")


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



#### plot dry clusters 3,4,5,6 FOR APPENDIX
#plot the cluster schemes
par(mfrow=c(1,2),mar=c(9.5,10,1,2))
brks <- c(1.5)
colours <- brewer.pal(2, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters.rank$dryS2, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2))
title('Dry season, k=2')

brks <- c(1.5,2.5)
colours <- brewer.pal(3, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters.rank$dryS3, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3))
title('Dry season, k=3')

brks <- c(1.5,2.5,3.5)
colours <- brewer.pal(4, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters.rank$dryS4, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3,4))
title('Dry season, k=4')

brks <- c(1.5,2.5,3.5,4.5)
colours <- brewer.pal(5, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters.rank$dryS5, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3,4,5))
title('Dry season, k=5')


#### plot wet clusters 3,4,5,6 FOR APPENDIX

#plot the cluster schemes
brks <- c(1.5)
colours <- brewer.pal(2, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters.rank$wetS2, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2))
title('Wet season, k=6')

brks <- c(1.5,2.5)
colours <- brewer.pal(3, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters.rank$wetS3, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3))
title('Wet season, k=3')

brks <- c(1.5,2.5,3.5)
colours <- brewer.pal(4, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters.rank$wetS4, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3,4))
title('Wet season, k=4')

brks <- c(1.5,2.5,3.5,4.5)
colours <- brewer.pal(5, "Paired")
sh <- shading(brks, cols=colours)
choropleth(voronoi, clusters.rank$wetS5, shading=sh, border=NA)
legend(x='bottomleft', fill=colours, legend=c(1,2,3,4,5))
title('Wet season, k=5')

## 
# brks <- c(1.5,2.5,3.5)
# colours <- brewer.pal(4, "Paired")
# sh <- shading(brks, cols=colours)
# choropleth(voronoi, clusters.rank$dryS4, shading=sh, border=NA)
# legend(x='bottomleft', fill=colours, legend=c(1,2,3,4))
# title('Dry season, k=4')

# brks <- c(1.5,2.5,3.5)
# colours <- brewer.pal(4, "Paired")
# sh <- shading(brks, cols=colours)
# choropleth(voronoi, clusters.rank$wetS4, shading=sh, border=NA)
# legend(x='bottomleft', fill=colours, legend=c(1,2,3,4))
# title('Wet season, k=4')



#### plot selected cluster schemes

# #plot the cluster schemes
# par(mfrow=c(1,2),mar=c(9.5,10,1,2))
# brks <- c(1.5,2.5,3.5,4.5)
# colours <- brewer.pal(12, "Paired")
# sh <- shading(brks, cols=colours[c(1:4,7)])
# choropleth(voronoi, clusters.rank$dryS5, shading=sh, border=NA)
# legend(x='bottomleft', fill=colours[c(1:4,7)], legend=c(1,2,3,4,5))
# title('a) Dry season spatial clustering using Ranks')

# # # rearrange clusters so that they are poor-poor-rich-rich
# # temp <- clusters$wetS4*10
# # temp[temp==30] <- 1
# # temp[temp==10] <- 2
# # temp[temp==40] <- 3
# # temp[temp==20] <- 4
# brks <- c(1.5,2.5,3.5)
# sh <- shading(brks, cols=colours[c(5:6,9:10)])
# choropleth(voronoi, clusters.rank$wetS4, shading=sh, border=NA)
# legend(x='bottomleft', fill=colours[c(5:6,9:10)], legend=c(1,2,3,4))
# title('b) Wet season spatial clustering using Ranks')

######  GGPLOT2
temp <- data.frame(WPIdata$wid, clusters.rank$dryS5, clusters.rank$wetS4)
colnames(temp) <- c('id','DryClusters','WetClusters')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(DryClusters))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_manual(values=brewer.pal(12, "Paired")[c(1:4,7:8)], name='Cluster', labels=c('Mekong South','Poor South','Northeast','Poor North','Bolaven','Vientiane')) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Rank clusters - Dry season') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(WetClusters))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_manual(values=brewer.pal(12, "Paired")[c(5:6,9:11)], name='Cluster', labels=c('Rich North','Rich South','Central','Poor South','Poor North')) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Rank clusters - Wet season') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map1,map2,cols=2)


########## Cluster exploration

# dry season
temp <- data.frame(dryWPIcomp, clusters.rank$dryS5, Provinces)
colnames(temp) <- c("dryWPI","dryRES", "dryACC", "dryCAP", "dryUSE", "dryENV", "Cluster", "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[,1:7], id="Cluster")
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(Cluster))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Dry Season') + scale_fill_manual(name="Cluster",values=brewer.pal(12,"Paired")[c(1:4,7:8)], labels=c('Mekong South','Poor South','Northeast','Poor North','Bolaven','Vientiane'))# dry season

# Wet season
# First reorder clusters to same order as dry
temp <- data.frame(wetWPIcomp, clusters.rank$wetS4, Provinces)
colnames(temp) <- c("wetWPI","wetRES", "wetACC", "wetCAP", "wetUSE", "wetENV", "Cluster", "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[,1:7], id="Cluster")
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(Cluster))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Wet Season') + scale_fill_manual(name="Cluster",values=brewer.pal(12,"Paired")[c(5:6,9:11)], labels=c('Rich North','Rich South','Central','Poor South','Poor North'))

multiplot(p1,p2,cols=1)


#VARIABLES
# dry season
temp <- data.frame(dryVAR, clusters.rank$dryS5, Provinces)
colnames(temp) <- c(colnames(dryVAR), "Cluster", "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[,1:19], id="Cluster")
p1 <- ggplot(data = temp, aes(variable, value, fill = factor(Cluster))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Dry Season') + scale_fill_manual(name="Cluster", labels=c('Mekong South','Poor South','Northeast','Poor North','Bolaven','Vientiane'),values=brewer.pal(12,"Paired")[c(1:4,7:8)])# dry season
# Wet season
# First reorder clusters to same order as dry
temp <- data.frame(wetVAR, clusters.rank$wetS4, Provinces)
colnames(temp) <- c(colnames(wetVAR), "Cluster", "Province") 
#pairs(temp, pch='.', upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
#title('Dry season')
temp <- melt(temp[,1:18], id="Cluster")
p2 <- ggplot(data = temp, aes(variable, value, fill = factor(Cluster))) + geom_boxplot() + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle('Wet Season') + scale_fill_manual(name="Cluster", labels=c('Rich North','Rich South','Central','Poor South','Poor North'),values=brewer.pal(12,"Paired")[c(5:6,9:11)])

multiplot(p1,p2,cols=1)







###########################################################################
############## GWPCA
###########################################################################

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


###################################################
# GWPCA for added WPI from both seasons (to represent both seasons)

temp <- WPIdata
temp@data <- as.data.frame( (dryWPIcomp[,2:6]+ wetWPIcomp[,2:6]) )
colnames(temp@data) <- c('RES','ACC','CAP','USE','ENV')
addedWPI_gwpca_400NN_comp <- gwpca(temp, vars=colnames(temp@data), bw=400, adaptive=T, k=5, dMat=distMat)

#calculate weights
weights.gwpca <- data.frame()
# individual weights for wet season
for (i in 1:8215) {
	temp <- addedWPI_gwpca_400NN_comp$loadings[i,,]
	sqroots <- c(  sqrt(abs(temp[1,1])) , sqrt(abs(temp[2,2])),  sqrt(abs(temp[3,3])),  sqrt(abs(temp[4,4])),  sqrt(abs(temp[5,5]))  )
	sumsqroots <- sum(sqroots)
	temp_weights <- c(sqrt(abs(temp[1,1]))/sumsqroots , sqrt(abs(temp[2,2]))/sumsqroots,  sqrt(abs(temp[3,3]))/sumsqroots,  sqrt(abs(temp[4,4]))/sumsqroots,  sqrt(abs(temp[5,5]))/sumsqroots)
	weights.gwpca <- rbind(weights.gwpca,temp_weights)
}
rm('sqroots','sumsqroots','temp','temp_weights')
colnames(weights.gwpca) <- c('RES','ACC','CAP','USE','ENV' )
weights.gwpca.both <- weights.gwpca
rm('weights.gwpca')
###################################################



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





####### PLOT

# plot weighting scheme box plots
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



####### correlation between GWPCA weights ########

drycorr <- vector()
wetcorr <- vector()

for (i in 1:5) {
	for (i in 15) {
		drycorr[1,i] <- boot::corr(c(weights.gwpca.dry[,i],weights.gwpca.dry[,j]))
		drycorr[2,i] <- boot::corr(c(weights.gwpca.dry[,i],weights.gwpca.dry[,j]))
		drycorr[3,i] <- boot::corr(c(weights.gwpca.dry[,i],weights.gwpca.dry[,j]))
		drycorr[4,i] <- boot::corr(c(weights.gwpca.dry[,i],weights.gwpca.dry[,j]))
		drycorr[5,i] <- boot::corr(c(weights.gwpca.dry[,i],weights.gwpca.dry[,j]))
		
		wetcorr[1,i] <- boot::corr(c(weights.gwpca.wet[,i],weights.gwpca.wet[,j]))
		wetcorr[2,i] <- boot::corr(c(weights.gwpca.wet[,i],weights.gwpca.wet[,j]))
		wetcorr[3,i] <- boot::corr(c(weights.gwpca.wet[,i],weights.gwpca.wet[,j]))
		wetcorr[4,i] <- boot::corr(c(weights.gwpca.wet[,i],weights.gwpca.wet[,j]))
		wetcorr[5,i] <- boot::corr(c(weights.gwpca.wet[,i],weights.gwpca.wet[,j]))
	}	
}


########## Map weights

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




####### Moran test for the weights
temp <- componentMoran2(weights.gwpca.dry, voronoi.nb.listw)






###### comparison // GGPLOT2
temp <- data.frame(WPIdata$wid, WPIdata$dryWPIggwpca, WPIdata$wetWPIggwpca, WPIdata$dryWPIggwpca-WPIdata$dryWPIsinglegpca, WPIdata$wetWPIggwpca-WPIdata$wetWPIsinglegpca)
colnames(temp) <- c('id','Dry','Wet', 'DryDiff','WetDiff')
temp2 <- merge(voronoi.f,temp, by='id')

# # Plot the gwpca WPI's
# par(mfrow=c(1,2))
# shading <- auto.shading(c(WPIdata$dryWPIggwpca,WPIdata$wetWPIggwpca), cols = brewer.pal(9,"Oranges"))
# plot(LaoAdmin0, col=NA, border=NA)
# choropleth(voronoi, WPIdata$dryWPIggwpca, shading=shading, border=NA, add=T)
# plotBG()
# choro.legend(px='bottomleft', sh=shading, title='WPI (GWPCA)')
# title('Dry season WPI')

# shading <- auto.shading(c(WPIdata$dryWPIggwpca,WPIdata$wetWPIggwpca), cols = brewer.pal(9,"Blues"))
# plot(LaoAdmin0, col=NA, border=NA)
# choropleth(voronoi, WPIdata$wetWPIggwpca, shading=shading, border=NA, add=T)
# plotBG()
# choro.legend(px='bottomleft', sh=shading, title='WPI (GWPCA)')
# title('Wet season WPI')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = Dry)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=50) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season WPI (GWPCA)') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())


map2 <- ggplot(temp2, aes(x = long, y = lat, fill = Wet)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=50) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season WPI (GWPCA)') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = DryDiff)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#7f3b08", mid="#f7f7f7", high="#2d004b", midpoint=0, name='Dry season \n difference') + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Difference between locally and \n globally weighted dry WPI') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

map4 <- ggplot(temp2, aes(x = long, y = lat, fill = WetDiff)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#7f3b08", mid="#f7f7f7", high="#2d004b", midpoint=0, name='Wet season \n difference') + theme_bw()
map4 <- map4 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('d) Difference between locally and \n globally weighted wet WPI') + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())

multiplot(map1,map3,map2,map4,cols=2)


# ####### PLOT wet season - dry season difference (WPI gmean pca)

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = (Wet-Dry))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695") + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Seasonal difference (GWPCA)') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())

map1
rm('map1','map2','temp','temp2')







###################### Compare single season pca with gwpca
temp <- data.frame(WPIdata$wid, WPIdata$dryWPIggwpca, WPIdata$dryWPIsinglepca, WPIdata$wetWPIggwpca, WPIdata$wetWPIsinglepca)
colnames(temp) <- c('id','Dry_GWPCA','Dry_PCA','Wet_GWPCA','Wet_PCA')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = Dry_GWPCA-Dry_PCA)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0, name="Difference") + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Dry season WPI difference (GWPCA-PCA)') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())


map2 <- ggplot(temp2, aes(x = long, y = lat, fill = Wet_GWPCA-Wet_PCA)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0, name="Difference") + theme_bw() 
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Wet season WPI difference (GWPCA-PCA)') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map1,map2,cols=2)
rm('map1','map2','temp','temp2')







####### PLOT wet season - dry season difference RANK ---- FOR APPENDIX

temp <- data.frame(WPIdata$wid, rank(WPIdata$dryWPIggwpca), rank(WPIdata$wetWPIggwpca))
colnames(temp) <- c('id','DryRank','WetRank')
temp2 <- merge(voronoi.f,temp, by='id')


# temp <- data.frame(rank(WPIdata$dryWPIggwpca), rank(WPIdata$wetWPIggwpca) )
# par(mfrow=c(1,1), mar=c(1,20,1,1))
# shading <- auto.shading(c(temp[,2]-temp[,1], -(temp[,2]-temp[,1])), cols = brewer.pal(5,"RdYlBu"))
# plot(LaoAdmin0, col=NA, border=NA)
# choropleth(voronoi, temp[,2]-temp[,1], shading=shading, border=NA, add=T)
# plotBG()
# choro.legend(px='bottomleft', sh=shading, title='Rank')
# title('Difference between Wet and Dry season')
# rm('temp')
map1 <- ggplot(temp2, aes(x = long, y = lat, fill = DryRank)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=4108) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Dry season Rank (GWPCA)') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())


map2 <- ggplot(temp2, aes(x = long, y = lat, fill = WetRank)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=4108) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Wet season Rank (GWPCA)') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map1,map2,cols=2)



map1 <- ggplot(temp2, aes(x = long, y = lat, fill = (WetRank-DryRank))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695") + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('Seasonal difference in ranks (GWPCA)') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())

map1

################## PLOT lead items for components
############# dry season
# plot lead item
local.loadings1 <- dryWPI_gwpca_400NN_comp$loadings[,,1] 
local.loadings2 <- dryWPI_gwpca_400NN_comp$loadings[,,2]
local.loadings3 <- dryWPI_gwpca_400NN_comp$loadings[,,3]
lead.item1 <- colnames(local.loadings1)[max.col(abs(local.loadings1))]
lead.item2 <- colnames(local.loadings2)[max.col(abs(local.loadings2))]
lead.item3 <- colnames(local.loadings3)[max.col(abs(local.loadings3))]
#df <- SpatialPolygonsDataFrame(voronoi, data=data.frame(lead.item1, lead.item2, lead.item3))

# par(mfrow=c(2,3))
# # lead item in PC1
# colour1 <- brewer.pal(5,"Set3")[match(lead.item1,unique(lead.item1))]
# plot(voronoi, col=colour1, border=NA)
# plotBG()
# #legend('bottomleft',as.character(unique(lead.item2)),pch=15, cex=1,col=brewer.pal(5,"Set3"))
# title("'Winning' variable, PC1")

# # lead item in PC2
# colour2 <- brewer.pal(5,"Set3")[match(lead.item2,unique(lead.item2))]
# plot(voronoi, col=colour2, border=NA)
# plotBG()
# #legend('bottomleft',as.character(unique(lead.item2)),pch=15, cex=1,col=brewer.pal(5,"Set3"))
# title("'Winning' variable, PC2")

# # lead item in PC3
# colour3 <- brewer.pal(5,"Set3")[match(lead.item3,unique(lead.item3))]
# plot(voronoi, col=colour3, border=NA)
# plotBG()
# #legend('bottomleft',as.character(unique(lead.item3)),pch=15, cex=1,col=brewer.pal(5,"Set3"))
# title("'Winning' variable, PC3")

# barplot(table(lead.item1),ylab="Freqency",las=2, col=c("#BEBADA", "#FB8072", "#80B1D3", "#FFFFB3", "#8DD3C7"))
# barplot(table(lead.item2),ylab="Freqency",las=2, col=c("#FFFFB3", "#FB8072", "#80B1D3", "#8DD3C7", "#BEBADA"))
# barplot(table(lead.item3),ylab="Freqency",las=2, col=c("#8DD3C7", "#80B1D3", "#BEBADA", "#FFFFB3", "#FB8072"))

########## plot ggplot2 alternative

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

rm('map1','map2','map3','bar1','bar2','bar3','temp','temp2')


########### SECOND ITEM
############ dry season
# plot lead item
local.loadings1 <- dryWPI_gwpca_400NN_comp$loadings[,,1] 
local.loadings2 <- dryWPI_gwpca_400NN_comp$loadings[,,2]
local.loadings3 <- dryWPI_gwpca_400NN_comp$loadings[,,3]

second.item1 <- vector()
second.item2 <- vector()
second.item3 <- vector()
for (i in 1:8215) {
	temp <- local.loadings1[i,]
	temp2 <- order(abs(temp), decreasing=TRUE)
	temp3 <- colnames(local.loadings1)[temp2[2]]
	second.item1 <- cbind(second.item1, temp3)
	
	temp <- local.loadings2[i,]
	temp2 <- order(abs(temp), decreasing=TRUE)
	temp3 <- colnames(local.loadings2)[temp2[2]]
	second.item2 <- cbind(second.item2, temp3)
	
	temp <- local.loadings3[i,]
	temp2 <- order(abs(temp), decreasing=TRUE)
	temp3 <- colnames(local.loadings3)[temp2[2]]
	second.item3 <- cbind(second.item3, temp3)
}


temp <- data.frame(WPIdata$wid, as.factor(second.item1), as.factor(second.item2), as.factor(second.item3))
colnames(temp) <- c('id','second item PC1','second item PC2', 'second item PC3')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,8]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season second item, PC1') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,9]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Dry season second item, PC2') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,10]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Dry season second item, PC3') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

temp <- table(second.item1)
temp <- as.data.frame(temp)
bar1 <- ggplot(temp) + geom_bar(aes(second.item1,Freq, fill=factor(second.item1)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('d) Dry season second item, PC1')

temp <- table(second.item2)
temp <- as.data.frame(temp)
bar2 <- ggplot(temp) + geom_bar(aes(second.item2,Freq, fill=factor(second.item2)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('e) Dry season second item, PC2')

temp <- table(second.item3)
temp <- as.data.frame(temp)
bar3 <- ggplot(temp) + geom_bar(aes(second.item3,Freq, fill=factor(second.item3)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('f) Dry season second item, PC3')




multiplot(map1,bar1,map2,bar2,map3,bar3,cols=3)

rm('map1','map2','map3','bar1','bar2','bar3','temp','temp2')





########## wet season
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

########### SECOND ITEM
############ wet season
# plot lead item
local.loadings1 <- wetWPI_gwpca_400NN_comp$loadings[,,1] 
local.loadings2 <- wetWPI_gwpca_400NN_comp$loadings[,,2]
local.loadings3 <- wetWPI_gwpca_400NN_comp$loadings[,,3]

second.item1 <- vector()
second.item2 <- vector()
second.item3 <- vector()
for (i in 1:8215) {
	temp <- local.loadings1[i,]
	temp2 <- order(abs(temp), decreasing=TRUE)
	temp3 <- colnames(local.loadings1)[temp2[2]]
	second.item1 <- cbind(second.item1, temp3)
	
	temp <- local.loadings2[i,]
	temp2 <- order(abs(temp), decreasing=TRUE)
	temp3 <- colnames(local.loadings2)[temp2[2]]
	second.item2 <- cbind(second.item2, temp3)
	
	temp <- local.loadings3[i,]
	temp2 <- order(abs(temp), decreasing=TRUE)
	temp3 <- colnames(local.loadings3)[temp2[2]]
	second.item3 <- cbind(second.item3, temp3)
}


temp <- data.frame(WPIdata$wid, as.factor(second.item1), as.factor(second.item2), as.factor(second.item3))
colnames(temp) <- c('id','second item PC1','second item PC2', 'second item PC3')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,8]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Wet season second item, PC1') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,9]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season second item, PC2') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,10]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Wet season second item, PC3') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

temp <- table(second.item1)
temp <- as.data.frame(temp)
bar1 <- ggplot(temp) + geom_bar(aes(second.item1,Freq, fill=factor(second.item1)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('d) Wet season second item, PC1')

temp <- table(second.item2)
temp <- as.data.frame(temp)
bar2 <- ggplot(temp) + geom_bar(aes(second.item2,Freq, fill=factor(second.item2)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('e) Wet season second item, PC2')

temp <- table(second.item3)
temp <- as.data.frame(temp)
bar3 <- ggplot(temp) + geom_bar(aes(second.item3,Freq, fill=factor(second.item3)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('f) Wet season second item, PC3')




multiplot(map1,bar1,map2,bar2,map3,bar3,cols=3)

rm('map1','map2','map3','bar1','bar2','bar3','temp','temp2')

rm('local.loadings1','local.loadings2','local.loadings3','lead.item1','lead.item2','lead.item3') #,'colour1','colour2','colour3')
rm('map1','map2','map3','bar1','bar2','bar3','temp','temp2')



# test GWPCA
dry.gwpca.mc1 <- montecarlo.gwpca.1(WPIdata, bw = 400, vars=c('dryRES','dryACC','dryCAP','dryUSE','dryENV'), k = 2, nsims=99,robust = FALSE, kernel = "gaussian", adaptive = TRUE, p = 2, theta = 0, longlat = F, distMat)

wet.gwpca.mc1 <- montecarlo.gwpca.1(WPIdata, bw = 400, vars=c('wetRES','wetACC','wetCAP','wetUSE','wetENV'), k = 2, nsims=99,robust = FALSE, kernel = "gaussian", adaptive = TRUE, p = 2, theta = 0, longlat = F, distMat)









###### *************************************** #############
################## PLOT lead items for variables
############# dry season
# plot lead item
local.loadings1 <- dryWPI_gwpca_400NN_vars$loadings[,,1] 
local.loadings2 <- dryWPI_gwpca_400NN_vars$loadings[,,2]
local.loadings3 <- dryWPI_gwpca_400NN_vars$loadings[,,3]
lead.item1 <- colnames(local.loadings1)[max.col(abs(local.loadings1))]
lead.item2 <- colnames(local.loadings2)[max.col(abs(local.loadings2))]
lead.item3 <- colnames(local.loadings3)[max.col(abs(local.loadings3))]
#df <- SpatialPolygonsDataFrame(voronoi, data=data.frame(lead.item1, lead.item2, lead.item3))

temp <- data.frame(WPIdata$wid, lead.item1, lead.item2, lead.item3)
colnames(temp) <- c('id','Lead item PC1','Lead item PC2', 'Lead item PC3')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,8]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season lead item, PC1') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Variable')

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,9]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Dry season lead item, PC2') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Variable')

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,10]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Dry season lead item, PC3') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Variable')

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



############# wet season
# plot lead item
local.loadings1 <- wetWPI_gwpca_400NN_vars$loadings[,,1] 
local.loadings2 <- wetWPI_gwpca_400NN_vars$loadings[,,2]
local.loadings3 <- wetWPI_gwpca_400NN_vars$loadings[,,3]
lead.item1 <- colnames(local.loadings1)[max.col(abs(local.loadings1))]
lead.item2 <- colnames(local.loadings2)[max.col(abs(local.loadings2))]
lead.item3 <- colnames(local.loadings3)[max.col(abs(local.loadings3))]
#df <- SpatialPolygonsDataFrame(voronoi, data=data.frame(lead.item1, lead.item2, lead.item3))

temp <- data.frame(WPIdata$wid, lead.item1, lead.item2, lead.item3)
colnames(temp) <- c('id','Lead item PC1','Lead item PC2', 'Lead item PC3')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,8]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Wet season lead item, PC1') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Variable')

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,9]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season lead item, PC2') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Variable')

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,10]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Wet season lead item, PC3') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Variable')

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




########### SECOND ITEM
############ wet season
# plot lead item
local.loadings1 <- wetWPI_gwpca_400NN_comp$loadings[,,1] 
local.loadings2 <- wetWPI_gwpca_400NN_comp$loadings[,,2]
local.loadings3 <- wetWPI_gwpca_400NN_comp$loadings[,,3]

second.item1 <- vector()
second.item2 <- vector()
second.item3 <- vector()
for (i in 1:8215) {
	temp <- local.loadings1[i,]
	temp2 <- order(abs(temp), decreasing=TRUE)
	temp3 <- colnames(local.loadings1)[temp2[2]]
	second.item1 <- cbind(second.item1, temp3)
	
	temp <- local.loadings2[i,]
	temp2 <- order(abs(temp), decreasing=TRUE)
	temp3 <- colnames(local.loadings2)[temp2[2]]
	second.item2 <- cbind(second.item2, temp3)
	
	temp <- local.loadings3[i,]
	temp2 <- order(abs(temp), decreasing=TRUE)
	temp3 <- colnames(local.loadings3)[temp2[2]]
	second.item3 <- cbind(second.item3, temp3)
}


temp <- data.frame(WPIdata$wid, as.factor(second.item1), as.factor(second.item2), as.factor(second.item3))
colnames(temp) <- c('id','second item PC1','second item PC2', 'second item PC3')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,8]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Wet season second item, PC1') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,9]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season second item, PC2') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

map3 <- ggplot(temp2, aes(x = long, y = lat, fill = factor(temp2[,10]))) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + theme_bw()
map3 <- map3 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('c) Wet season second item, PC3') + theme(rect=element_blank(), line=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_fill_discrete(name='Component')

temp <- table(second.item1)
temp <- as.data.frame(temp)
bar1 <- ggplot(temp) + geom_bar(aes(second.item1,Freq, fill=factor(second.item1)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('d) Wet season second item, PC1')

temp <- table(second.item2)
temp <- as.data.frame(temp)
bar2 <- ggplot(temp) + geom_bar(aes(second.item2,Freq, fill=factor(second.item2)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('e) Wet season second item, PC2')

temp <- table(second.item3)
temp <- as.data.frame(temp)
bar3 <- ggplot(temp) + geom_bar(aes(second.item3,Freq, fill=factor(second.item3)), stat='identity') + theme_bw() + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle('f) Wet season second item, PC3')




multiplot(map1,bar1,map2,bar2,map3,bar3,cols=3)



####################################################
#############      GWR
###################################################


# create a name vector for model selection
names <- colnames(WPIdata_sample@data)[9:77]
newnames <- vector()
for (i in 1:length(names)) {
	newnames[i] <- paste("WPIdata_sample$",names[i],sep="")
}

# remove variables which cause problems
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

# add calculated wpi to the original WPIdata
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


# Local collinearity diagnostics
dry.gwr.adapt.collins <- gwr.collin.diagno(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data=WPIdata_orig, bw=drygwr.bw.adapt, kernel='gaussian', dMat=distMat, adaptive=TRUE)

wet.gwr.adapt.collins <- gwr.collin.diagno(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data=WPIdata_orig, bw=wetgwr.bw.adapt, kernel='gaussian', dMat=distMat, adaptive=TRUE)





# test them!
dry.F1 <- LMZ.F1GWR.test(dry.gwr.adapt.spgwr)
dry.F2 <- LMZ.F2GWR.test(dry.gwr.adapt.spgwr)
#dry.F3 <- LMZ.F3GWR.test(dry.gwr.adapt.spgwr)
wet.F1 <- LMZ.F1GWR.test(wet.gwr.adapt.spgwr)
wet.F2 <- LMZ.F2GWR.test(wet.gwr.adapt.spgwr)
#wet.F3 <- LMZ.F3GWR.test(wet.gwr.adapt.spgwr)

dry.gwr.mctest <- montecarlo.gwr(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+WetPrec+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+WetSurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+ConsIrr+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data = WPIdata_orig,nsims=99, kernel="gaussian",adaptive=T, bw=368, p=2, theta=0, longlat=F,dMat=distMat)

wet.gwr.mctest <- montecarlo.gwr(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+DryPrec+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+ConsIrr+StartRF3a+WatSupp, data = WPIdata_orig,nsims=99, kernel="gaussian",adaptive=T, bw=368, p=2, theta=0, longlat=F,dMat=distMat)



########### New GWR without collinearity problems (maybe)
# Do GWR with spgwr
dry.gwr.adapt.spgwr.2 <- gwr(dryWPIgpca~ PopDepCrop+TimeProCap+SoilDeg+RoadAcc+TotalPop+TotAgrArea+LitPopSh+ShDryIrr+IncPov+IrrAreaSh+DryPrec+PestsFre+AvMaxDDay+HumanFP+DrySurf+DrinkRainW+Drought+DrinkOther+TotalCons+LandType+Irrigation+Elevation+TimeDisCap+IrrTempWei+TotIrrArea+AmountRF3a+OtherDisFr+DrinkSurfW+DisFreNS+DisFreNo+FloodFre+PopDepAqua+IrrGabion+Disaster+IrrReservo+Slopeclass+ToiletType, data = WPIdata_orig, adapt=drygwr.bw.adapt/8215, hatmatrix=FALSE)

wet.gwr.adapt.spgwr.2 <- gwr(wetWPIgpca~ RoadAcc+PopDepCrop+LitPopSh+SoilDeg+TimeDisCap+TotAgrArea+TotalPop+IrrAreaSh+WetPrec+IncPov+PestsFre+LandslFre+Flood+DrinkRainW+HumanFP+TotalCons+TimeProCap+AvMaxDDay+OtherDisFr+TotIrrArea+IrrReservo+DrinkSurfW+DrinkOther+HealthCent+ToiletType+PopDepAqua+Slopeclass+DroughtFre+Elevation+LandType+StartRF3a+WatSupp, data = WPIdata_orig, adapt=wetgwr.bw.adapt/8215, hatmatrix=FALSE)




########## plot r2

temp <- data.frame(WPIdata$wid, dry.gwr.adapt.spgwr$SDF$localR2, wet.gwr.adapt.spgwr$SDF$localR2)
colnames(temp) <- c('id','DryR2','WetR2')
temp2 <- merge(voronoi.f,temp, by='id')

map1 <- ggplot(temp2, aes(x = long, y = lat, fill = DryR2)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0.70) + theme_bw()
map1 <- map1 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('a) Dry season GWR model R-square') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())

map2 <- ggplot(temp2, aes(x = long, y = lat, fill = WetR2)) + geom_polygon(aes(group = group), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", midpoint=0.70) + theme_bw()
map2 <- map2 + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle('b) Wet season GWR model R-square') + theme(axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map1,map2,cols=2)

rm('map1','map2','temp','temp2')


# Plot prediction and residuals

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


