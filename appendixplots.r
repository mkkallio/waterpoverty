######## Plot variable maps for appendix

########## DRY SEASON GWR COEFFICIENTS
# temp <- data.frame(WPIdata$wid, dryRESvar)
# colnames(temp)[1] <- c('id')
# temp2 <- merge(voronoi.f,temp, by='id')

# for (i in 0:3) {
	# map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,7+i]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#006837", midpoint=50, name=colnames(temp2)[7+i]) + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[7+i])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
	
	# assign(paste0("map",i),map)
# }
# multiplot(map1,map2,map3,cols=4)


plot.appendix(dryRESvar)
multiplot(map1,map2,map3,cols=4)

plot.appendix(wetRESvar)
multiplot(map1,map2,cols=4)

plot.appendix(ACCvar)
multiplot(map1,map2,map3,cols=4)

plot.appendix(dryCAPvar)
multiplot(map1,map2,map3,map4,cols=4)

plot.appendix(wetCAPvar)
multiplot(map1,map2,map3,map4,cols=4)

plot.appendix(dryUSEvar)
multiplot(map1,map2,map3,cols=4)

plot.appendix(wetUSEvar)
multiplot(map1,map2,map3,cols=4)

plot.appendix(dryENVvar)
multiplot(map1,map2,map3,map4,cols=4)

plot.appendix(wetENVvar)
multiplot(map1,map2,map3,map4,cols=4)









##### plot GWR variable loadings for APPENDIX



###################################
########## DRY SEASON GWR COEFFICIENTS
temp <- data.frame(WPIdata$wid, dry.gwr.adapt$SDF@data)
colnames(temp)[1] <- c('id')
temp2 <- merge(voronoi.f,temp, by='id')

for (i in 0:3) {
	for (j in 1:9) {
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,7+(i*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", name='Coefficient \n estimate') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[7+(i*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
			
			 assign(paste0("map",j),map)
	 }
	 multiplot(map1,map4,map7,map2,map5,map8,map3,map6,map9,cols=3)
	 #multiplot(map1,map2,map3,map4,map5, cols=3)
	 #grid.arrange(map1,map4,map7,map2,map5,map8,map3,map6,map9, ncol=3, nrow=3)
	 pause()
}

for (j in 1:5) {
	
		map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,7+(4*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", name='Coefficient \n estimate') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[7+(4*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
		
		 assign(paste0("map",j),map)
}
 
 multiplot(map1,map4,map2,map5,map3, cols=3)

rm('map','map1','map3','map2','map4','map5','map6','map7','map8','map9')


###################################
########## WET SEASON GWR COEFFICIENTS
temp <- data.frame(WPIdata$wid, wet.gwr.adapt$SDF@data)
colnames(temp)[1] <- c('id')
temp2 <- merge(voronoi.f,temp, by='id')

for (i in 0:2) {
	for (j in 1:9) {
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,7+(i*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", name='Coefficient \n estimate') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[7+(i*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
			
			 assign(paste0("map",j),map)
	 }
	 multiplot(map1,map4,map7,map2,map5,map8,map3,map6,map9,cols=3)
	 #multiplot(map1,map2,map3,map4,map5, cols=3)
	 #grid.arrange(map1,map4,map7,map2,map5,map8,map3,map6,map9, ncol=3, nrow=3)
	 pause()
}

for (j in 1:8) {
	
		map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,7+(3*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#313695", name='Coefficient \n estimate') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[7+(3*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
		
		 assign(paste0("map",j),map)
}
 
 multiplot(map1,map4,map2,map5,map3, cols=3)

rm('map','map1','map3','map2','map4','map5','map6','map7','map8','map9')




##### plot variable t stats for APPENDIX

###################################
###################################
###################################
########## DRY SEASON
temp <- data.frame(WPIdata$wid, dry.gwr.adapt$SDF@data)
colnames(temp)[1] <- c('id')
temp2 <- merge(voronoi.f,temp, by='id')

for (i in 0:3) {
	for (j in 1:9) {
		
		if (any(dry.gwr.adapt$SDF@data[,87+(i*9)+j] <2 & dry.gwr.adapt$SDF@data[,87+(i*9)+j] >-2)) {
			variablename = colnames(temp2[94+(i*9)+j])
		
			selection <- temp2[,94+(i*9)+j] 
			selection <- selection <2 & selection >-2
			
			temp <-  temp2[,(95:(95+8))]
			#tempsel <- temp2[,(94+(i*9)+j)]
			
			
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,94+(i*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data=temp2[selection,], aes(group = group), fill="white") + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[94+(i*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())
			
			assign(paste0("map",j),map, envir=.GlobalEnv)
			
		 } else {
		
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,94+(i*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[94+(i*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
			
			 assign(paste0("map",j),map)
			
		 }
	 }
	 multiplot(map1,map4,map7,map2,map5,map8,map3,map6,map9,cols=3)
	 #multiplot(map1,map2,map3,map4,map5, cols=3)
	 #grid.arrange(map1,map4,map7,map2,map5,map8,map3,map6,map9, ncol=3, nrow=3)
	 pause()
}

for (j in 1:5) {
		
	if (any(dry.gwr.adapt$SDF@data[,87+(4*9)+j] <2 & dry.gwr.adapt$SDF@data[,87+(4*9)+j] >-2)) {
		variablename = colnames(temp2[94+(4*9)+j])
	
		selection <- temp2[,94+(4*9)+j] 
		selection <- selection <2 & selection >-2		
		
		map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,94+(4*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data=temp2[selection,], aes(group = group), fill="white") + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[94+(4*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())
		
		assign(paste0("map",j),map, envir=.GlobalEnv)
		
	 } else {
	
		map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,94+(4*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[94+(4*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
		
		 assign(paste0("map",j),map)
		
	 }
}
 
 multiplot(map1,map4,map2,map5,map3, cols=3)

rm('map','map1','map3','map2','map4','map5','map6','map7','map8','map9')



########## WET SEASON GWR COEFFICIENTS
temp <- data.frame(WPIdata$wid, wet.gwr.adapt$SDF@data)
colnames(temp)[1] <- c('id')
temp2 <- merge(voronoi.f,temp, by='id')

for (i in 0:2) {
	for (j in 1:9) {
		
		
		if (any(wet.gwr.adapt$SDF@data[,75+(i*9)+j] <2 & wet.gwr.adapt$SDF@data[,75+(i*9)+j] >-2)) {
			variablename = colnames(temp2[82+(i*9)+j])
		
			selection <- temp2[,82+(i*9)+j] 
			selection <- selection <2 & selection >-2
			
			
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,82+(i*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data=temp2[selection,], aes(group = group), fill="white") + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[82+(i*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())
			
			assign(paste0("map",j),map, envir=.GlobalEnv)
			
		 } else {
		
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,82+(i*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[82+(i*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
			
			 assign(paste0("map",j),map)
			
		 }
	 }
	 multiplot(map1,map4,map7,map2,map5,map8,map3,map6,map9,cols=3)
	 #multiplot(map1,map2,map3,map4,map5, cols=3)
	 #grid.arrange(map1,map4,map7,map2,map5,map8,map3,map6,map9, ncol=3, nrow=3)
	 pause()
}

for (j in 1:8) {
		
		if (any(wet.gwr.adapt$SDF@data[,75+(3*9)+j] <2 & wet.gwr.adapt$SDF@data[,75+(3*9)+j] >-2)) {
			variablename = colnames(temp2[94+(3*9)+j])
		
			selection <- temp2[,94+(3*9)+j] 
			selection <- selection <2 & selection >-2			
			
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,94+(3*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data=temp2[selection,], aes(group = group), fill="white") + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[94+(3*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())
			
			assign(paste0("map",j),map, envir=.GlobalEnv)
			
		 } else {
		
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,94+(3*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[94+(3*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
			
			 assign(paste0("map",j),map)
			
		 }
	 }
	 multiplot(map1,map4,map7,map2,map5,map8,map3,map6,cols=3)

rm('map','map1','map3','map2','map4','map5','map6','map7','map8','map9')


########## WET SEASON t-VALUES
temp <- data.frame(WPIdata$wid, wet.gwr.adapt$SDF@data)
colnames(temp)[1] <- c('id')
temp2 <- merge(voronoi.f,temp, by='id')

for (i in 0:2) {
	for (j in 1:9) {
		
		
		if (any(wet.gwr.adapt$SDF@data[,75+(i*9)+j] <2 & wet.gwr.adapt$SDF@data[,75+(i*9)+j] >-2)) {
			variablename = colnames(temp2[82+(i*9)+j])
		
			selection <- temp2[,82+(i*9)+j] 
			selection <- selection <2 & selection >-2
			
			
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,82+(i*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data=temp2[selection,], aes(group = group), fill="white") + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[82+(i*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())
			
			assign(paste0("map",j),map, envir=.GlobalEnv)
			
		 } else {
		
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,82+(i*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[82+(i*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
			
			 assign(paste0("map",j),map)
			
		 }
	 }
	 multiplot(map1,map4,map7,map2,map5,map8,map3,map6,map9,cols=3)
	 #multiplot(map1,map2,map3,map4,map5, cols=3)
	 #grid.arrange(map1,map4,map7,map2,map5,map8,map3,map6,map9, ncol=3, nrow=3)
	 pause()
}

for (j in 1:8) {
		
		if (any(wet.gwr.adapt$SDF@data[,75+(3*9)+j] <2 & wet.gwr.adapt$SDF@data[,75+(3*9)+j] >-2)) {
			variablename = colnames(temp2[94+(3*9)+j])
		
			selection <- temp2[,94+(3*9)+j] 
			selection <- selection <2 & selection >-2			
			
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,94+(3*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data=temp2[selection,], aes(group = group), fill="white") + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[94+(3*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank())
			
			assign(paste0("map",j),map, envir=.GlobalEnv)
			
		 } else {
		
			map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,94+(3*9)+j]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#543005", mid="#f5f5f5", high="#003c30", midpoint=0, name='t-statistic') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[94+(3*9)+j])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
			
			 assign(paste0("map",j),map)
			
		 }
	 }
	 multiplot(map1,map4,map7,map2,map5,map8,map3,map6,cols=3)

rm('map','map1','map3','map2','map4','map5','map6','map7','map8','map9')




####### plot component scores
plot.appendix(dryWPIcomp)
multiplot(map1,map3,map5,map2,map4,map6,cols=2)
plot.appendix(wetWPIcomp)
multiplot(map1,map3,map5,map2,map4,map6,cols=2)



# plot  clusters
plot.appendix.factor(clusters[,1:8])
multiplot(map1,map4,map7,map2,map5,map8,map3,map6,cols=3)

plot.appendix.factor(clusters[,9:16])
multiplot(map1,map4,map7,map2,map5,map8,map3,map6,cols=3)

plot.appendix.factor(clusters[,c(1:4,9:12)])
multiplot(map1,map4,map7,map2,map5,map8,map3,map6,cols=3)

# plot rank clusters
plot.appendix.factor(clusters.rank[,c(1:4,9:12)])
multiplot(map1,map4,map7,map2,map5,map8,map3,map6,cols=3)



