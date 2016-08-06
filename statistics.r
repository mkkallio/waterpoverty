# Summary statistics
summary(WPIdata@data)


############### Perform PCA
PCA <- prcomp(WPIcomp, scale=T)

sqroots <- c(  sqrt(abs(PCA$rotation[1,1])) , sqrt(abs(PCA$rotation[2,2])),  sqrt(abs(PCA$rotation[3,3])),  sqrt(abs(PCA$rotation[4,4])),  sqrt(abs(PCA$rotation[5,5]))  )
sumsqroots <- sum(sqroots)
weights <- c(  sqrt(abs(PCA$rotation[1,1]))/sumsqroots , sqrt(abs(PCA$rotation[2,2]))/sumsqroots,  sqrt(abs(PCA$rotation[3,3]))/sumsqroots,  sqrt(abs(PCA$rotation[4,4]))/sumsqroots,  sqrt(abs(PCA$rotation[5,5]))/sumsqroots  )

eigenvalues <- PCA$sdev^2

for (i in 1:5) {
	weights[i] <- sum(PCA$rotation[i,] * ( sqrt(eigenvalues[i]) / sum(sqrt(eigenvalues))))
}



# calculate PCA weighted WPI
# Resource
# WPIresource <- ( WPIdata$DrySoil + WPIdata$WetSoil + WPIdata$DryAvail+ WPIdata$WetAvail + WPIdata$AvMaxDDay)/5
# WPIdata$RES <- WPIresource
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
#Total 
#WPIscore <- WPIresource*weights[1] + WPIaccess*weights[2] + WPIcapacity*weights[3] + WPIuse*weights[4] + WPIenvironment*weights[5]
#WPIdata$WPIpca <- WPIscore

############## same for dry and wet season
PCA <- prcomp(dryWPIcomp, scale=F)

sqroots <- c(  sqrt(abs(PCA$rotation[1,1])) , sqrt(abs(PCA$rotation[2,2])),  sqrt(abs(PCA$rotation[3,3])),  sqrt(abs(PCA$rotation[4,4])),  sqrt(abs(PCA$rotation[5,5]))  )
sumsqroots <- sum(sqroots)
weights <- c(  sqrt(abs(PCA$rotation[1,1]))/sumsqroots , sqrt(abs(PCA$rotation[2,2]))/sumsqroots,  sqrt(abs(PCA$rotation[3,3]))/sumsqroots,  sqrt(abs(PCA$rotation[4,4]))/sumsqroots,  sqrt(abs(PCA$rotation[5,5]))/sumsqroots  )
########################## Calculate dry season WPI
# Resource
 WPIresource <- (WPIdata$DryAvail + WPIdata$DrySoil + WPIdata$AvMaxDDay) /3
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
# Total dry WPI score
WPIscore <- WPIresource*weights[1] + WPIaccess*weights[2] + WPIcapacity*weights[3] + WPIuse*weights[4] + WPIenvironment*weights[5]
WPIdata$dryWPIpca <- WPIscore



########################## Calculate wet season WPI
PCA <- prcomp(wetWPIcomp, scale=F)

sqroots <- c(  sqrt(abs(PCA$rotation[1,1])) , sqrt(abs(PCA$rotation[2,2])),  sqrt(abs(PCA$rotation[3,3])),  sqrt(abs(PCA$rotation[4,4])),  sqrt(abs(PCA$rotation[5,5]))  )
sumsqroots <- sum(sqroots)
weights <- c(  sqrt(abs(PCA$rotation[1,1]))/sumsqroots , sqrt(abs(PCA$rotation[2,2]))/sumsqroots,  sqrt(abs(PCA$rotation[3,3]))/sumsqroots,  sqrt(abs(PCA$rotation[4,4]))/sumsqroots,  sqrt(abs(PCA$rotation[5,5]))/sumsqroots  )

# Resource
WPIresource <- (WPIdata$WetAvail + WPIdata$WetSoil + WPIdata$WetPrec)/3
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
# Enviroment
WPIenvironment <- (WPIdata$ECO_V_cat + WPIdata$wetDisast + WPIdata$SoilDeg)/3
WPIdata$wetENV <- WPIenvironment
# Total wet WPI score
WPIscore <- WPIresource*weights[1] + WPIaccess*weights[2] + WPIcapacity*weights[3] + WPIuse*weights[4] + WPIenvironment*weights[5]
WPIdata$wetWPIpca <- WPIscore


# gmean WPI alternative
PCA <- prcomp(WPIcomp, scale=F)
eigenvalues <- PCA$sdev^2

for (i in 1:5) {
	weights[i] <- sum(abs(PCA$rotation[i,]) * ( sqrt(eigenvalues[i]) / sum(sqrt(eigenvalues))))
}
weights <- abs(weights)

WPImult <- (WPIcomp[,1]^weights[1] * WPIcomp[,2]^weights[2] * WPIcomp[,3]^weights[3] * WPIcomp[,4]^weights[4] * WPIcomp[,5]^weights[5])^(1/sum(weights))

# DRY SEASON
PCA <- prcomp(dryWPIcomp, scale=F)
eigenvalues <- PCA$sdev^2

for (i in 1:5) {
	weights[i] <- sum(PCA$rotation[i,] * ( sqrt(eigenvalues[i]) / sum(sqrt(eigenvalues))))
}
weights <- abs(weights)

dryWPImult <- (dryWPIcomp[,1]^weights[1] * dryWPIcomp[,2]^weights[2] * dryWPIcomp[,3]^weights[3] * dryWPIcomp[,4]^weights[4] * dryWPIcomp[,5]^weights[5])^(1/sum(weights))

# WET SEASON
PCA <- prcomp(wetWPIcomp, scale=F)
eigenvalues <- PCA$sdev^2

for (i in 1:5) {
	weights[i] <- sum(PCA$rotation[i,] * ( sqrt(eigenvalues[i]) / sum(sqrt(eigenvalues))))
}
weights <- abs(weights)

wetWPImult <- (wetWPIcomp[,1]^weights[1] * wetWPIcomp[,2]^weights[2] * wetWPIcomp[,3]^weights[3] * wetWPIcomp[,4]^weights[4] * wetWPIcomp[,5]^weights[5])^(1/sum(weights))

WPIdata$WPImult <- WPImult
WPIdata$dryWPImult <- dryWPImult
WPIdata$wetWPImult <- wetWPImult

