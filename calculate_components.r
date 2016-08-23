#############################################
######################## Calculate components
#############################################


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

WPIdata$gmeanDryWPI <- gmeandry
WPIdata$gmeanWetWPI <- gmeanwet
#WPIdata$gmeanWPI <- gmeanwpi

rm('gmeandry', 'gmeanwet')


# collect components into data.frames
dryWPIcomp <- as.data.frame(cbind(WPIdata$dryWPI, WPIdata$dryRES,WPIdata$dryACC,WPIdata$dryCAP,WPIdata$dryUSE,WPIdata$dryENV))
wetWPIcomp <-  as.data.frame(cbind(WPIdata$wetWPI, WPIdata$wetRES,WPIdata$wetACC,WPIdata$wetCAP,WPIdata$wetUSE,WPIdata$wetENV))

# change 0-scores to 0.01 so that geometric mean does not result in zero WPI
temp <- dryWPIcomp[,5]
temp[temp==0] <- 0.01
dryWPIcomp[,5] <- temp
temp <- wetWPIcomp[,5]
temp[temp==0] <- 0.01
wetWPIcomp[,5] <- temp

rm('temp')

# collect all components together
WPIscores <- cbind(dryWPIcomp,wetWPIcomp)








###########################################
############## Calculate WPI with "objective" weights derived from PCA
###########################################

# Perform PCA for dry season
PCA <- prcomp(dryWPIcomp[,2:6], scale=F) # no scaling - the components are already scaled 0-100

sqroots <- c(  sqrt(abs(PCA$rotation[1,1])) , sqrt(abs(PCA$rotation[2,2])),  sqrt(abs(PCA$rotation[3,3])),  sqrt(abs(PCA$rotation[4,4])),  sqrt(abs(PCA$rotation[5,5]))  )
weights <- sqroots/sum(sqroots)


# Total dry WPI score - equal weights
WPIscore <- dryWPIcomp[,2]*weights[1] + dryWPIcomp[,3]*weights[2] + dryWPIcomp[,4]*weights[3] + dryWPIcomp[,5]*weights[4] + dryWPIcomp[,6]*weights[5]
WPIdata$dryWPIsinglepca <- WPIscore

# total dry WPI gmean score - geometric mean
dryWPImult <- (dryWPIcomp[,2]^weights[1] * dryWPIcomp[,3]^weights[2] * dryWPIcomp[,4]^weights[3] * dryWPIcomp[,5]^weights[4] * dryWPIcomp[,6]^weights[5])^(1/sum(weights))
WPIdata$dryWPIsinglegpca <- dryWPImult

# calculate wet WPI using dry season weights - geometric mean
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

# Perform PCA again using data from both seasons
temp <- rbind(dryWPIcomp[,2:6], wetWPIcomp[,2:6])
PCA <- prcomp(temp, scale=F) # no scaling - the components are already scaled 0-100

sqroots <- c(  sqrt(abs(PCA$rotation[1,1])) , sqrt(abs(PCA$rotation[2,2])),  sqrt(abs(PCA$rotation[3,3])),  sqrt(abs(PCA$rotation[4,4])),  sqrt(abs(PCA$rotation[5,5]))  )
weights <- sqroots/sum(sqroots)

globalPCAweights <- cbind(globalPCAweights, weights)
colnames(globalPCAweights) <- c('Dry','Wet','Both')
rownames(globalPCAweights) <- c('RES','ACC','CAP','USE','ENV')


############### calculate WPI with both-season weights

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

# name component collections
colnames(dryWPIcomp) <- c('dryWPI','dryRES','dryACC','dryCAP','dryUSE','dryENV' )
colnames(wetWPIcomp) <- c('wetWPI','wetRES','wetACC','wetCAP','wetUSE','wetENV' )