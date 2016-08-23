
# ###################### quick mapping choropleths and village plots. from the book "R for spatial analysis and mapping"
# quick.map <- function(spdf, var, legend.title, main.title ) {
	# x <- spdf@data[,var]
	# cut.vals <- pretty(x)
	# x.cut <- cut(x, cut.vals)
	# cut.levels <- levels(x.cut)
	# cut.band <- match(x.cut, cut.levels)
	# colors <- brewer.pal(length(cut.levels), 'Reds')
	# par(mar=c(1,1,1,1))
	# plot(LaoAdmin0, col='grey85')
	# title(main.title)
	# plot(spdf, add=T, col=colors[cut.band],pch=16)
	# plot(mapBG, col='white', add=T)
	# legend('bottomleft', cut.levels, col=colors,pch=16,bty='n', title=legend.title)
# }


# modified quick.map
quick.map <- function(variable,colnumber,cols) {
	shading <- auto.shading(variable, cols = brewer.pal(colnumber,cols))
	plot(LaoAdmin0, col=NA, border=NA)
	choropleth(voronoi, variable, shading=shading, border=NA, add=T)
	plotBG()
	choro.legend(px='bottomleft', sh=shading)
}


################### Calculate moran's I from components

# componentMoran <- function(x, dMat) {
	# exit <- vector()

	# for (i in 1:ncol(x)) {
		# temp <- Moran.I( x[,i],dMat)
		# exit <- rbind(exit, temp)
	# }
	# rownames(exit) <- colnames(x)
	# return(exit)
# }

componentMoran2 <- function(x, listw) {
	exit <- vector()

	for (i in 1:ncol(x)) {
		temp <- moran.test( x[,i],listw)
		exit <- rbind(exit, c(temp$estimate))
	}
	exit <- as.data.frame(exit)
	rownames(exit) <- colnames(x)
	colnames(exit) <- names(temp$estimate)
	return(exit)
}


################### From the book R for spatial analysis and mapping chapter 6: point pattern analysis
voronoipolygons <- function(layer) {
	crds <- layer@coords
	z <- deldir(crds[,1], crds[,2])
	w <- tile.list(z)
	polys <- vector(mode='list', length=length(w) )
	
	for (i in seq(along=polys)) {
		pcrds <- cbind(w[[i]]$x, w[[i]]$y )
		pcrds <- rbind(pcrds, pcrds[1,])
		polys[i] <- Polygons(list(Polygon(pcrds) ), ID=as.character(i) )
	}
	
	SP <- SpatialPolygons(polys)
	voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1], y=crds[,2], layer@data, row.names=sapply(slot(SP, 'polygons'), function(x) slot(x, 'ID') ) ) )
	
	return (voronoi)
}


####################### plot map background
plotBG <- function() {
	plot(mapBG, col='white', border=NA, add=T)
	plot(LaoAdmin1, add=T)
}


# RPubs Chris Brunsdon GWPCA tutorial
prop.var <- function(gwpca.obj, n.components) {
  return(rowSums(gwpca.obj$var[,1:n.components])/rowSums(gwpca.obj$var))
}



# Multiple plot function FROM COOKBOOK FOR R, http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# R cookbook scatterplot matrix
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}
panel.hist <- function(x, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(usr[1:2], 0, 1.5) )
h <- hist(x, plot = FALSE)
breaks <- h$breaks
nB <- length(breaks)
y <- h$counts
y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}


# Pause function from http://diego.assencio.com?index=86c137b502561d44b8be02f06d80ee16
pause = function()
{
    if (interactive()) {
        invisible(readline(prompt = "Press <Enter> to continue..."))
    }
    else {
        cat("Press <Enter> to continue...")
        invisible(readLines(file("stdin"), 1))
    }
}



# create a lot of plots automatically with fill from continuous variable
plot.appendix <- function(data) {

	temp <- data.frame(WPIdata$wid, data)
	colnames(temp)[1] <- c('id')
	temp2 <- merge(voronoi.f,temp, by='id')

	for (i in 1:ncol(data)) {
		map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=temp2[,7+i]), colour = NA) + coord_equal() + scale_fill_gradient2(low="#a50026", mid="white", high="#006837", midpoint=50, name='Score') + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[7+i])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
		
		assign(paste0("map",i),map, envir = .GlobalEnv)
	}
}


# create a lot of plots automatically with fill from factorial variable
plot.appendix.factor <- function(data) {

	temp <- data.frame(WPIdata$wid, data)
	colnames(temp)[1] <- c('id')
	temp2 <- merge(voronoi.f,temp, by='id')

	for (i in 1:ncol(data)) {
		map <- ggplot(temp2, aes(x = long, y = lat, group=group)) + geom_polygon(aes_string(fill=factor(temp2[,7+i])), colour = NA) + coord_equal() + theme_bw() + geom_polygon(data = LaoAdmin1.f, aes(x = long, y = lat, group = group), colour = 'black', size=0.5, fill = NA) + coord_equal() + ggtitle(colnames(temp2[7+i])) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) + scale_fill_discrete(name='Cluster')
		
		assign(paste0("map",i),map, envir = .GlobalEnv)
	}
}