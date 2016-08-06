###################################
# Poverty in Laos
#################################

##################### Load libraries
library(rgdal)
library(sp)
library(GISTools)
library(maptools)
library(spdep)
library(sp)
library(RColorBrewer)
library(MASS)
library(rgeos)
library(Matrix)
library(ggplot2)
library(raster)
library(GWmodel)
library(gstat)
library(deldir)
library(classInt)
library(DescTools)

####################### Load data from shapefile into a sp dataframe
#CensusData_WGS84 <- readShapePoints("Population_census_2005_shape/data", proj4string = CRS("+init=epsg:4326")) # villages
#AgrCensus_WGS84 <- readShapePoints("agricultural_census_2010-2011_shape_general_data/data", proj4string = CRS("+init=epsg:4326")) #villages
VillageData_WGS84 <- readShapePoints("CenAgr_reduced_new_vars", proj4string = CRS ("+init=epsg:4326"))
LaoAdmin0_WGS84 <- readShapePoly("admin areas/LAO_adm0", proj4string = CRS("+init=epsg:4326") ) # country border
LaoAdmin1_WGS84 <- readShapePoly("admin areas/LAO_adm1_correct_names_no_xaisomboun", proj4string = CRS("+init=epsg:4326")) # province border
LaoAdmin2_WGS84 <- readShapePoly("admin areas/LAO_adm2_correct_names", proj4string = CRS("+init=epsg:4326")) # district border


####################### transform to UTM 48N
#CensusData <- spTransform(CensusData_WGS84, CRS=CRS("+init=epsg:32648"))
#AgrCensus <- spTransform(AgrCensus_WGS84, CRS=CRS("+init=epsg:32648"))
VillageData <- spTransform(VillageData_WGS84, CRS=CRS("+init=epsg:32648"))
LaoAdmin0 <- spTransform(LaoAdmin0_WGS84, CRS=CRS("+init=epsg:32648"))
LaoAdmin1 <- spTransform(LaoAdmin1_WGS84, CRS=CRS("+init=epsg:32648"))
LaoAdmin2 <- spTransform(LaoAdmin2_WGS84, CRS=CRS("+init=epsg:32648"))

rm('LaoAdmin0_WGS84','LaoAdmin1_WGS84','LaoAdmin2_WGS84')

# Map background
mapBG <- readShapePoly("MapBG", proj4string = CRS("+init=epsg:4326") ) # background
mapBG <- spTransform(mapBG, CRS=CRS("+init=epsg:32648"))

####################### Load WPI data
WPIdata <- readShapePoints("WPIdata_new2", proj4string=CRS("+init=epsg:32648")) # to process
WPIdata_orig <- readShapePoints("WPIdata_new2", proj4string=CRS("+init=epsg:32648")) # to keep the original values
#WPIdry <- readShapePoints("WPIdry", proj4string=CRS("+init=epsg:32648"))
#WPIwet <- readShapePoints("WPIwet", proj4string=CRS("+init=epsg:32648"))


####################### transform variables from factors to numeric
WPIdata$Disaster <- as.numeric(WPIdata$Disaster)
WPIdata$Flood <- as.numeric(WPIdata$Flood)
WPIdata$Drought <- as.numeric(WPIdata$Drought)
WPIdata$Landslide <- as.numeric(WPIdata$Landslide)
WPIdata$Pests <- as.numeric(WPIdata$Pests)
WPIdata$OtherDis <- as.numeric(WPIdata$OtherDis)
WPIdata$DisastNS <- as.numeric(WPIdata$DisastNS)
WPIdata$FloodFre <- as.numeric(WPIdata$FloodFre)
WPIdata$DroughtFre <- as.numeric(WPIdata$DroughtFre)
WPIdata$LandslFre <- as.numeric(WPIdata$LandslFre)
WPIdata$PestsFre <- as.numeric(WPIdata$PestsFre)
WPIdata$OtherDisFr <- as.numeric(WPIdata$OtherDisFr)
WPIdata$DisFreNS <- as.numeric(WPIdata$DisFreNS)

WPIdata$DisFreNo <- as.numeric(WPIdata$DisFreNo)

WPIdata$Irrigation <- as.numeric(WPIdata$Irrigation)
WPIdata$IrrPermWei <- as.numeric(WPIdata$IrrPermWei)
WPIdata$IrrReservo <- as.numeric(WPIdata$IrrReservo)
WPIdata$IrrPump <- as.numeric(WPIdata$IrrPump)
WPIdata$IrrDyke <- as.numeric(WPIdata$IrrDyke)
WPIdata$IrrTempWei <- as.numeric(WPIdata$IrrTempWei)
WPIdata$IrrGabion <- as.numeric(WPIdata$IrrGabion)
WPIdata$IrrOther <- as.numeric(WPIdata$IrrOther)
WPIdata$IrrNS <- as.numeric(WPIdata$IrrNS)

WPIdata$DrinkPipe <- as.numeric(WPIdata$DrinkPipe)
WPIdata$DrinkPrWel <- as.numeric(WPIdata$DrinkPrWel)
WPIdata$DrinkUnprW <- as.numeric(WPIdata$DrinkUnprW)
WPIdata$DrinkSurfW <- as.numeric(WPIdata$DrinkSurfW)
WPIdata$DrinkRainW <- as.numeric(WPIdata$DrinkRainW)
WPIdata$DrinkOther <- as.numeric(WPIdata$DrinkOther)
WPIdata$DrinkNS <- as.numeric(WPIdata$DrinkNS)
WPIdata$Drink70Pip <- as.numeric(WPIdata$Drink70Pip)

WPIdata$DrinkSourc <- as.numeric(WPIdata$DrinkSourc)
WPIdata$WatSupp <- as.numeric(WPIdata$WatSupp)

WPIdata$NormRF3a <- as.numeric(WPIdata$NormRF3a)
WPIdata$StartRF3a <- as.numeric(WPIdata$StartRF3a)
WPIdata$AmountRF3a <- as.numeric(WPIdata$AmountRF3a)

WPIdata$HealthCent <- as.numeric(WPIdata$HealthCent)
WPIdata$TWalkHC <- as.numeric(WPIdata$TWalkHC)
WPIdata$RoadAcc <- as.numeric(WPIdata$RoadAcc)
WPIdata$ECO_V_cat <- as.numeric(WPIdata$ECO_V_cat)
WPIdata$SoilDeg <- as.numeric(WPIdata$SoilDeg)
WPIdata$LandType <- as.numeric(WPIdata$LandType)


WPIdata$wcdagqa10 <- as.numeric(WPIdata$wcdagqa10)


## Transform variable values from 1-2 to boolean 0-1
WPIdata$Flood <- WPIdata$Flood -1
WPIdata$Drought <- WPIdata$Drought -1
WPIdata$Landslide <- WPIdata$Landslide -1
WPIdata$Pests <- WPIdata$Pests -1
WPIdata$OtherDis <- WPIdata$OtherDis -1
WPIdata$DisastNS <- WPIdata$DisastNS -1
WPIdata$FloodFre <- WPIdata$FloodFre -1
WPIdata$DroughtFre <- WPIdata$DroughtFre -1
WPIdata$LandslFre <- WPIdata$LandslFre -1
WPIdata$PestsFre <- WPIdata$PestsFre -1
WPIdata$OtherDisFr <- WPIdata$OtherDisFr -1
WPIdata$DisFreNS <- WPIdata$DisFreNS -1

WPIdata$DisFreNo <- WPIdata$DisFreNo -1

WPIdata$Irrigation <- WPIdata$Irrigation -1
WPIdata$IrrPermWei <- WPIdata$IrrPermWei -1
WPIdata$IrrReservo <- WPIdata$IrrReservo -1
WPIdata$IrrPump <- WPIdata$IrrPump -1
WPIdata$IrrDyke <- WPIdata$IrrDyke -1
WPIdata$IrrTempWei <- WPIdata$IrrTempWei -1
WPIdata$IrrGabion <- WPIdata$IrrGabion -1
WPIdata$IrrOther <- WPIdata$IrrOther -1
WPIdata$IrrNS <- WPIdata$IrrNS -1

WPIdata$DrinkPipe <- WPIdata$DrinkPipe -1
WPIdata$DrinkPrWel <- WPIdata$DrinkPrWel -1
WPIdata$DrinkUnprW <- WPIdata$DrinkUnprW -1
WPIdata$DrinkSurfW <- WPIdata$DrinkSurfW -1
WPIdata$DrinkRainW <- WPIdata$DrinkRainW -1
WPIdata$DrinkOther <- WPIdata$DrinkOther -1
WPIdata$DrinkNS <- WPIdata$DrinkNS -1
WPIdata$Drink70Pip <- WPIdata$Drink70Pip -1

#WPIdata$DrinkSourc <- as.numeric(WPIdata$DrinkSourc)
#WPIdata$WatSupp <- as.numeric(WPIdata$WatSupp)

#WPIdata$NormRF3a <- as.numeric(WPIdata$NormRF3a)
#WPIdata$StartRF3a <- as.numeric(WPIdata$StartRF3a)
#WPIdata$AmountRF3a <- as.numeric(WPIdata$AmountRF3a)

#WPIdata$HealthCent <- as.numeric(WPIdata$HealthCent)
#WPIdata$TWalkHC <- as.numeric(WPIdata$TWalkHC)
#WPIdata$RoadAcc <- as.numeric(WPIdata$RoadAcc)
#WPIdata$ECO_V_cat <- as.numeric(WPIdata$ECO_V_cat)
#WPIdata$SoilDeg <- as.numeric(WPIdata$SoilDeg)
#WPIdata$LandType <- as.numeric(WPIdata$LandType)

WPIdata$wcdagqa10 <- as.numeric(WPIdata$wcdagqa10)


############################ Calculate scores
# Calculate scores for disasters
WPIdata$Disaster <- WPIdata$Flood + WPIdata$Drought + WPIdata$Landslide + WPIdata$Pests + WPIdata$OtherDis + WPIdata$DisastNS + WPIdata$FloodFre + WPIdata$DroughtFre + WPIdata$LandslFre + WPIdata$PestsFre + WPIdata$OtherDisFr + WPIdata$DisFreNS

WPIdata$dryDisast <- WPIdata$Drought + WPIdata$Pests + WPIdata$OtherDis + WPIdata$DisastNS + WPIdata$DroughtFre + WPIdata$PestsFre + WPIdata$OtherDisFr + WPIdata$DisFreNS

WPIdata$wetDisast <- WPIdata$Flood + WPIdata$Landslide + WPIdata$Pests + WPIdata$OtherDis + WPIdata$DisastNS + WPIdata$DroughtFre + WPIdata$LandslFre + WPIdata$PestsFre + WPIdata$OtherDisFr + WPIdata$DisFreNS

WPIdata$Disaster <- WPIdata$Disaster/12
WPIdata$dryDisast <- WPIdata$dryDisast/12
WPIdata$wetDisast <- WPIdata$wetDisast/12

WPIdata$Disaster <- 100-(WPIdata$Disaster*100)
WPIdata$dryDisast <- 100-(WPIdata$dryDisast*100)
WPIdata$wetDisast <- 100-(WPIdata$wetDisast*100)

#Irrigation
WPIdata$Irrigation <- WPIdata$IrrPermWei*2 + WPIdata$IrrReservo*2 + WPIdata$IrrPump*2 +  WPIdata$IrrDyke*2 + WPIdata$IrrTempWei + WPIdata$IrrGabion + WPIdata$IrrOther + WPIdata$IrrNS
WPIdata$Irrigation <- WPIdata$Irrigation/max(WPIdata$Irrigation)
WPIdata$Irrigation <- 100-(WPIdata$Irrigation*100)

#drinking water source
WPIdata$DrinkNS <- WPIdata$DrinkPipe + WPIdata$DrinkPrWel + WPIdata$DrinkUnprW - WPIdata$DrinkSurfW - WPIdata$DrinkRainW - WPIdata$DrinkOther
WPIdata$DrinkNS <- WPIdata$DrinkNS +3
WPIdata$DrinkNS <- WPIdata$DrinkNS/max(WPIdata$DrinkNS)
WPIdata$DrinkNS <- 100-WPIdata$DrinkNS*100
# Alternative based on Drinking water source from PopCensus
#WPIdata$DrinkSourc <- 100 - ((WPIdata$DrinkSourc -1) * 100/6)

# Toilet type
TT <- WPIdata$ToiletType
TT[TT==1] <- 100
TT[TT==2] <- 66
TT[TT==3] <- 33
TT[TT==4] <- 0
TT[TT==99] <- 50
WPIdata$ToiletType <- TT
rm(TT)

# time to district capital + province capital

WPIdata$TimeCap <- WPIdata$TimeDisCap + WPIdata$TimeProCap

# Time to district and province capitals
TPC <- WPIdata$TimeProCap
TDC <- WPIdata$TimeDisCap
TPC[TPC==-999]<-0
TDC[TDC==-999]<-0
TPC <- TPC/max(TPC)
TDC <- TDC/max(TDC) 
TPC <- 100-(TPC*100)
TDC <- 100-(TDC*100)
WPIdata$TimeProCap <- TPC
WPIdata$TimeDisCap <- TDC

TC <- WPIdata$TimeCap
TC[TC<0] <- 0
TC[TC>600] <- 600
TC = TC/max(TC)
TC = 100-(TC*100)
WPIdata$TimeCap <- TC

# Road access
RA <- WPIdata$RoadAcc
RA[RA==1] <- 50
RA[RA==2] <- 100
RA[RA==3] <- 0
RA[RA==4] <- 0

DRYRA <- RA==50 | RA ==100
DRYRA <- DRYRA*100
WETRA <- RA == 100
WETRA <- WETRA*100

WPIdata$RoadAcc <- RA
WPIdata$DryRoad <- DRYRA
WPIdata$WetRoad <- WETRA
 

rm('TPC', 'TDC', 'TC', 'RA', 'WETRA', 'DRYRA')
#rm(RA)

# Threatened Amphibians
TA <- WPIdata$ECO_V_cat
TA[TA==4] <- 0
TA[TA==3] <- 33
TA[TA==2] <- 66
TA[TA==1] <- 100
WPIdata$ECO_V_cat <- TA
rm(TA)

# Soil Degradation
SD <- WPIdata$SoilDeg
SD[SD==1] <- 100
SD[SD==2] <- 66
SD[SD==3] <- 33
SD[SD==4] <- 0
SD[SD==5] <- median(SD)
SD[SD==6] <- median(SD)
WPIdata$SoilDeg <- SD
rm(SD)


# Dependency on crops or aquaculture
CADEP <- WPIdata$PopDepCrop + WPIdata$PopDepAqua
CADEP <- 100-CADEP
CADEP[CADEP<0] <- 0
WPIdata$AgAqDepend <- CADEP
rm(CADEP)

# Area of agricultural area per capita : max score with 1 ha per capita, zero score with 0.1 ha pp.
AGA <- WPIdata$TotAgrArea/WPIdata$TotalPop
AGA <- AGA
AGA[AGA>1] <- 1
AGA[AGA<0.1] <- 0
AGA <- AGA - 0.1
AGA[AGA <0] <- 0
AGA <- AGA/max(AGA)
AGA <- AGA*100
WPIdata$AgAreaPerC <- AGA
rm(AGA)

# Average consecutive drought days
DD <- WPIdata$AvMaxDDay
DD <- DD-min(DD)
DD <- DD/max(DD)
DD <- 100-(DD*100)
WPIdata$AvMaxDDay <-DD
rm(DD)

########################## Model results
#well <- cbind(WPIdata$DrinkPrWel, WPIdata$DrinkUnprW)
#well <- WPIdata$DrinkPrWel + WPIdata$DrinkUnprW
#wells <- vector()
#for (i in nrow(well)) {
#	if (well[i,1] == 1 || well[i,2] == 1) {
#		wells <- c(wells, 1)
#	} else {
#		wells <- c(wells, 0)
#	}
#}

#surfacewater and precipitation, mm/1000 capita
#POP <- WPIdata$TotalPop
#POP <- POP/1000
#DRY <- WPIdata$DrySurf  + WPIdata$DryPrec/100
#DRY <- DRY/POP
#DRY[DRY > 5] <- 5
#DRY <- DRY/max(DRY)
#WET <- WPIdata$WetSurf  + WPIdata$WetPrec/100
#WET <- WET/POP
#WET[WET > 5] <- 5
#WET <- WET/max(WET)

#WPIdata$DryAvail <- DRY*100
#WPIdata$WetAvail <- WET*100

# NEW
POP <- WPIdata$TotalPop
DRY <- WPIdata$DrySurf
DRY <- DRY/1000*5000*5000/POP
AvScarcity <- 1700/182.5
DRY[DRY > AvScarcity] <- AvScarcity
AbsScarcity <- 500/182.5
DRY[DRY < AbsScarcity] <- AbsScarcity
DRY <- DRY-AbsScarcity
DRY <- DRY/max(DRY)


WET <- WPIdata$WetSurf 
WET <- WET/1000*5000*5000/POP
WET[WET > AvScarcity] <- AvScarcity
WET[WET < AbsScarcity] <- AbsScarcity
WET <- WET-AbsScarcity
WET <- WET/max(WET)
WPIdata$DryAvail <- DRY*100
WPIdata$WetAvail <- WET*100


rm('WET','DRY','AbsScarcity','AvScarcity')

WPIdata$DrySoil <- WPIdata$DrySoil/max(WPIdata$WetSoil)*100
WPIdata$WetSoil <- WPIdata$WetSoil/max(WPIdata$WetSoil)*100

WPIdata$DryPrec <- WPIdata$DryPrec/max(WPIdata$WetPrec)*100
WPIdata$WetPrec <- WPIdata$WetPrec/max(WPIdata$WetPrec)*100


# Create domestic water consumption rate (Rattykone thesis -- WHO/WASA)
POP <- WPIdata$TotalPop
popo2000 <- POP >2000
pop2000 <- POP==2000
popu2000 <- POP <2000
WU <- POP
WU[popo2000] <- WU[popo2000]*0.15
WU[pop2000] <- WU[pop2000]*0.15
WU[popu2000] <- WU[popu2000]*0.06
# water use per available resource
#AVW
#blaaaaa
WPIdata$DomWatUse <- WU
rm('WU', 'popo2000', 'popu2000','POP')

# total water consumption
WC <- 0 

# Incidence of poverty
WPIdata$IncPov <- 100-WPIdata$IncPov



######################### Calculate components
# Resource
 WPIresource <- ( WPIdata$DrySoil + WPIdata$WetSoil + WPIdata$DryAvail+ WPIdata$WetAvail + WPIdata$AvMaxDDay)/5
 WPIdata$RES <- WPIresource

# Access
WPIaccess <- (WPIdata$Irrigation + WPIdata$DrinkNS + WPIdata$ToiletType)/3
WPIdata$ACC <- WPIaccess

#Capacity
WPIcapacity <-(WPIdata$TimeCap + WPIdata$RoadAcc + WPIdata$LitPopSh + WPIdata$IncPov)/4
WPIdata$CAP <- WPIcapacity

# Use
WPIuse <- (WPIdata$IrrAreaSh + WPIdata$AgAreaPerC + WPIdata$AgAqDepend)/3
WPIdata$USE <- WPIuse

# Environment
WPIenvironment <- (WPIdata$ECO_V_cat + WPIdata$Disaster + WPIdata$SoilDeg)/3
WPIdata$ENV <- WPIenvironment

# Total WPI score --- no weighting
WPIscore <- (WPIresource + WPIaccess + WPIcapacity + WPIuse + WPIenvironment)/5
WPIdata$WPI <- WPIscore



########################## Calculate dry season WPI
# Resource
 WPIresource <- (WPIdata$DryAvail + WPIdata$DrySoil + WPIdata$DryPrec + WPIdata$AvMaxDDay) /4
 WPIdata$dryRES <- WPIresource

# Access
WPIaccess <- (WPIdata$Irrigation + WPIdata$DrinkNS + WPIdata$ToiletType)/3
WPIdata$dryACC <- WPIaccess

#Capacity
WPIcapacity <- (WPIdata$TimeCap + WPIdata$DryRoad + WPIdata$LitPopSh + WPIdata$IncPov)/4
WPIdata$dryCAP <- WPIcapacity

# Use
WPIuse <- (WPIdata$IrrAreaSh + WPIdata$AgAreaPerC + WPIdata$AgAqDepend)/3
WPIdata$dryUSE <- WPIuse

# Environment
WPIenvironment <- (WPIdata$ECO_V_cat + WPIdata$dryDisast + WPIdata$SoilDeg)/3
WPIdata$dryENV <- WPIenvironment

# Total dry WPI score --- no weighting
WPIscore <- (WPIresource + WPIaccess + WPIcapacity + WPIuse + WPIenvironment)/5
WPIdata$dryWPI <- WPIscore


########################## Calculate wet season WPI
# Resource
WPIresource <- (WPIdata$WetAvail + WPIdata$WetSoil + WPIdata$WetPrec + 100)/4
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
WPIenvironment <- (WPIdata$ECO_V_cat + WPIdata$wetDisast + WPIdata$SoilDeg)/3
WPIdata$wetENV <- WPIenvironment

# Total wet WPI score --- no weighting
WPIscore <- (WPIresource + WPIaccess +WPIcapacity + WPIuse + WPIenvironment)/5
WPIdata$wetWPI <- WPIscore



########################## Calculate geometric means
gmeandry <- apply(cbind(WPIdata$dryRES, WPIdata$dryACC, WPIdata$dryCAP, WPIdata$dryUSE, WPIdata$dryENV), 1, Gmean)
gmeanwet <- apply(cbind(WPIdata$wetRES, WPIdata$wetACC, WPIdata$wetCAP, WPIdata$wetUSE, WPIdata$wetENV), 1, Gmean)
gmeanwpi <- apply(cbind(WPIdata$RES, WPIdata$ACC, WPIdata$CAP, WPIdata$USE, WPIdata$ENV), 1, Gmean)

WPIdata$gmeanDryWPI <- gmeandry
WPIdata$gmeanWetWPI <- gmeanwet
WPIdata$gmeanWPI <- gmeanwpi

rm('gmeandry', 'gmeanwet', 'gmeanwpi')

WPIcomp <- cbind(WPIdata$WPI,WPIdata$RES,WPIdata$ACC,WPIdata$CAP,WPIdata$USE,WPIdata$ENV)
colnames(dryWPIcomp) <- c('WPI','RES','ACC','CAP','USE','ENV' )
dryWPIcomp <- cbind(WPIdata$dryWPI, WPIdata$dryRES,WPIdata$dryACC,WPIdata$dryCAP,WPIdata$dryUSE,WPIdata$dryENV)
colnames(dryWPIcomp) <- c('WPI','RES','ACC','CAP','USE','ENV' )
wetWPIcomp <- cbind(WPIdata$wetWPI, WPIdata$wetRES,WPIdata$wetACC,WPIdata$wetCAP,WPIdata$wetUSE,WPIdata$wetENV)
colnames(dryWPIcomp) <- c('WPI','RES','ACC','CAP','USE','ENV' )

WPIscores <- cbind(WPIcomp,dryWPIcomp,wetWPIcomp)
scaledScores <- scale(WPIscores)

####################### subset villages on province level. Census data
for (i in 1:17) {
	incl.vil <- WPIdata$bid == i
	output <- WPIdata[incl.vil,]
	#provName <- as.character(provNameList[i])
	provName <- as.character(output$bcne[1] )
	if (i == 1) {
		provNameList <- provName
	} else {
		provNameList <- c(provNameList, provName)
	}
	 
	assign(paste("villages.prov.",provName,sep="" ), output	)

}

######################## subset villages on district level. Census data
distrCode <- unique(WPIdata$tid)
for (i in 1:length(distrCode)) {
	#distrCode <- unique(VillageData$did)
	incl.vil <- VillageData$did == distrCode[i]
	output <- VillageData[incl.vil,]
	distrName <- as.character(output$dcne[1] )
	dCode <- output$did[1]
	if (i == 1) {
		distrNameList <- distrName
		distrCodeList <- dCode
	} else {
		distrNameList <- c(distrNameList, distrName)
		distrCodeList <- c(distrCodeList, dCode)
	}
	 
	assign(paste("villages.distr.",distrCode[i],sep="" ), output	)
}

######################### Extract province borders
provBorderList <- vector()
for (i in 1:17) {
	provName <- provNameList[i]
	provSelectionIndex <- LaoAdmin1$NAME_1 == provName
	provSelection <- LaoAdmin1[provSelectionIndex,]
	assign(paste("province.border.",provName, sep="" ), provSelection  )
	provBorderList <- cbind(provBorderList, paste(provName, "Border", sep="") )
}

############### extract district borders using names (not necessarily good idea --> comment)
#distrBorderList <- vector()
#for (i in 1:length(distrCode)) {
#	distrName <- distrNameList[i]
#	distrSelectionIndex <- LaoAdmin2$NAME_2 == distrName
#	distrSelection <- LaoAdmin2[distrSelectionIndex,]
#	assign(paste("district_",distrName, "Border", sep="" ), distrSelection  )
#	distrBorderList <- cbind(distrBorderList, paste("district_",provName, "Border", sep="") )
#}

######################### extract district borders using district codes
distrBorderCodeList <- vector()
for (i in 1:length(distrCode)) {
	distrName <- distrCode[i]
	distrSelectionIndex <- LaoAdmin2$ID_2 == distrName
	distrSelection <- LaoAdmin2[distrSelectionIndex,]
	assign(paste("district.border.",distrName, sep="" ), distrSelection  )
	distrBorderList <- cbind(distrBorderCodeList, paste("district_",distrName, "Border", sep="") )
}