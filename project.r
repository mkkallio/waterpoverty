
clusterX <- read.csv("C:/Users/Marko/OneDrive/Aalto/Water and People in a Changing World/Project/clusterX.csv", header=FALSE)

colnames(clusterX) <- c('Elevation','Prec', 'Runoff_ratio','Crop_area', 'Pasture_area','Ecozone','Discharge', 'Temperature', 'Cluster')
PCA <- prcomp(na.omit(clusterX[,1:8]), scale=T)
summary(PCA)

scaled_X <- scale(clusterX[,1:8])


PC1 <- cbind(PCA$rotation[1,1]*clusterX[,1],  PCA$rotation[2,1]*clusterX[,2], PCA$rotation[3,1]*clusterX[,3], PCA$rotation[4,1]*clusterX[,4], PCA$rotation[5,1]*clusterX[,5], PCA$rotation[6,1]*clusterX[,6], PCA$rotation[7,1]*clusterX[,7], PCA$rotation[8,1]*clusterX[,8])

PC1_map <- (PC1[,1] + PC1[,2] + PC1[,3] + PC1[,4] + PC1[,5] + PC1[,6] + PC1[,7] + PC1[,8])/8
PC1_map <- matrix(PC1_map,nrow=50,ncol=29)


PC2 <- cbind(PCA$rotation[1,2]*clusterX[,1],  PCA$rotation[2,2]*clusterX[,2], PCA$rotation[3,2]*clusterX[,3], PCA$rotation[4,2]*clusterX[,4], PCA$rotation[5,2]*clusterX[,5], PCA$rotation[6,2]*clusterX[,6], PCA$rotation[7,2]*clusterX[,7], PCA$rotation[8,2]*clusterX[,8])

PC2_map <- (PC2[,1] + PC2[,2] + PC2[,3] + PC2[,4] + PC2[,5] + PC2[,6] + PC2[,7] + PC2[,8])/8
PC2_map <- matrix(PC2_map,nrow=50,ncol=29)


PC3 <- cbind(PCA$rotation[1,3]*clusterX[,1],  PCA$rotation[2,3]*clusterX[,2], PCA$rotation[3,3]*clusterX[,3], PCA$rotation[4,3]*clusterX[,4], PCA$rotation[5,3]*clusterX[,5], PCA$rotation[6,3]*clusterX[,6], PCA$rotation[7,3]*clusterX[,7], PCA$rotation[8,3]*clusterX[,8])

PC3_map <- (PC3[,1] + PC3[,2] + PC3[,3] + PC3[,4] + PC3[,5] + PC3[,6] + PC3[,7] + PC3[,8])/8
PC3_map <- matrix(PC3_map,nrow=50,ncol=29)

PC4 <- cbind(PCA$rotation[1,4]*clusterX[,1],  PCA$rotation[2,4]*clusterX[,2], PCA$rotation[3,4]*clusterX[,3], PCA$rotation[4,4]*clusterX[,4], PCA$rotation[5,4]*clusterX[,5], PCA$rotation[6,4]*clusterX[,6], PCA$rotation[7,4]*clusterX[,7], PCA$rotation[8,4]*clusterX[,8])

PC4_map <- (PC4[,1] + PC4[,2] + PC4[,3] + PC4[,4] + PC4[,5] + PC4[,6] + PC4[,7] + PC4[,8])/8
PC4_map <- matrix(PC4_map,nrow=50,ncol=29)

composite <- (PC1_map*(0.6052/0.894) + PC2_map*(0.1785/0.894) + PC3_map*(0.1103/0.894))

par(mfrow=c(1,1))
r <- raster(composite)
plot(r)

par(mfrow=c(2,2))
r1 <- raster(PC1_map)
plot(r1)
title('PC1')
r2 <- raster(PC2_map)
plot(r2)
title('PC2')
r3 <- raster(PC3_map)
plot(r3)
title('PC3')
r <- raster(composite)
plot(r)
title('Composite')

r1 <- gplot(r1) + geom_tile(aes(fill = value)) +
	scale_fill_gradient(low = 'white', high = 'red') +
    coord_equal()
r2 <- gplot(r2) + geom_tile(aes(fill = value)) +
	scale_fill_gradient(low = 'white', high = 'red') +
    coord_equal()
r3 <- gplot(r3) + geom_tile(aes(fill = value)) +
	scale_fill_gradient(low = 'white', high = 'red') +
    coord_equal()
multiplot(r1,r2,r3)


loadings1 <- data.frame(cbind(varnames,as.numeric(PCA$rotation[,1])))
loadings1[,2] <- as.numeric(PCA$rotation[,1])
loadings2 <- data.frame(cbind(varnames,as.numeric(PCA$rotation[,2])))
loadings2[,2] <- as.numeric(PCA$rotation[,2])
loadings3 <- data.frame(cbind(varnames,as.numeric(PCA$rotation[,3])))
loadings3[,2] <- as.numeric(PCA$rotation[,3])

p1 <- ggplot(loadings1, aes(x = 0, xend = V2, y = varnames, yend=varnames, fill=varnames)) + 
	geom_point(aes(V2, varnames)) + geom_segment() + ggtitle('Loadings PC1')
p2 <- ggplot(loadings2, aes(x = 0, xend = V2, y = varnames, yend=varnames, fill=varnames)) + 
	geom_point(aes(V2, varnames)) + geom_segment() + ggtitle('Loadings PC2')
p3 <- ggplot(loadings3, aes(x = 0, xend = V2, y = varnames, yend=varnames, fill=varnames)) + 
    geom_point(aes(V2, varnames)) + geom_segment() + ggtitle('Loadings PC3')
multiplot(p1,p2,p3, cols=3)


############## Model

Rtable <- read.csv("C:/Users/Marko/OneDrive/Aalto/Water and People in a Changing World/Project/Rtable.csv", header=FALSE)
colnames(Rtable) <- c('Elevation','Prec', 'Runoff_ratio','Crop_area', 'Pasture_area','Ecozone','Discharge', 'Temperature', 'Urban_pop_share', 'Total_pop', 'Water_consumption','Cluster', 'Vulnerability','Governance','Economy','Social_capital','Human_footprint','Natural_hazards','Water_stress')





postscript("scatmat.eps")
pairs(ctable, pch='.', upper.panel = panel.cor,
    diag.panel = panel.hist,
    lower.panel = panel.smooth) # Make plot
dev.off()