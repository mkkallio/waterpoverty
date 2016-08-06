
for (i in 1:12) {
	if (i == 1) {
		plot(WL_2009_VV[,i], Q_2009_VV[,i], pch=16, xlim=c(0,700), ylim=c(0,10))
		plot(WL_2010_VV[,i], Q_2010_VV[,i], pch=16, add=T)
		plot(WL_2011_VV[,i], Q_2011_VV[,i], pch=16, add=T)
		plot(WL_2012_VV[,i], Q_2012_VV[,i], pch=16, add=T)
		plot(WL_2013_VV[,i], Q_2013_VV[,i], pch=16, add=T)
		plot(WL_2014_VV[,i], Q_2014_VV[,i], pch=16, add=T)
	} else {
		plot(WL_2009_VV[,i], Q_2009_VV[,i], add=T, pch=16)
		plot(WL_2009_VV[,i], Q_2009_VV[,i], pch=16, add=T)
		plot(WL_2010_VV[,i], Q_2010_VV[,i], pch=16, add=T)
		plot(WL_2011_VV[,i], Q_2011_VV[,i], pch=16, add=T)
		plot(WL_2012_VV[,i], Q_2012_VV[,i], pch=16, add=T)
		plot(WL_2013_VV[,i], Q_2013_VV[,i], pch=16, add=T)
		plot(WL_2014_VV[,i], Q_2014_VV[,i], pch=16, add=T)
	
	}
}



}







m1_dry <- raster("WPIoutput/dryprec/m1_avg_dry_prec_mmd.asc")
m2_dry <- raster("WPIoutput/dryprec/m2_avg_dry_prec_mmd.asc")
m3_dry <- raster("WPIoutput/dryprec/m3_avg_dry_prec_mmd.asc")
mkg_dry <- raster("WPIoutput/dryprec/mkg_avg_dry_prec_mmd.asc")
m1_dry@crs <- CRS("+init=epsg:32647")
m2_dry@crs <- CRS("+init=epsg:32647")
m3_dry@crs <- CRS("+init=epsg:32647")
mkg_dry@crs <- CRS("+init=epsg:32648")
m1_dry <- projectRaster(m1_dry, crs=CRS("+init=epsg:32648"))
m2_dry <- projectRaster(m2_dry, crs=CRS("+init=epsg:32648"))
m3_dry <- projectRaster(m3_dry, crs=CRS("+init=epsg:32648"))

rm <- merge (mkg_dry, m1_dry, m2_dry, m3_dry)
rm <- mosaic (mkg_dry, m1_dry, m2_dry, m3_dry, fun=mean)

##########################
##########################

mkg_ll <- raster("landslide/mkg_landslidesf.asc")
m1_ll <- raster("landslide/m1_landslidesf.asc")
m2_ll <- raster("landslide/m2_landslidesf.asc")
m3_ll <- raster("landslide/m3_landslidesf.asc")
mkg_ll@crs <- CRS("+init=epsg:32648")
m1_ll@crs <- CRS("+init=epsg:32647")
m2_ll@crs <- CRS("+init=epsg:32647")
m3_ll@crs <- CRS("+init=epsg:32647")
mkg_ll <- projectRaster(mkg_ll, crs=CRS("+init=epsg:32647"))
rf <- writeRaster(mkg_ll, filename="landslide/mkg_ll.tif", format="GTiff", overwrite=TRUE)
rf <- writeRaster(m1_ll, filename="landslide/m1_ll.tif", format="GTiff", overwrite=TRUE)
rf <- writeRaster(m2_ll, filename="landslide/m2_ll.tif", format="GTiff", overwrite=TRUE)
rf <- writeRaster(m3_ll, filename="landslide/m3_ll.tif", format="GTiff", overwrite=TRUE)

##########################
##########################

mkg_ll <- raster("wetsurf/mkg_surf_water_av_wet.asc")
m1_ll <- raster("wetsurf/m1_surf_water_av_wet.asc")
m2_ll <- raster("wetsurf/m2_surf_water_av_wet.asc")
m3_ll <- raster("wetsurf/m3_surf_water_av_wet.asc")
mkg_ll@crs <- CRS("+init=epsg:32648")
m1_ll@crs <- CRS("+init=epsg:32647")
m2_ll@crs <- CRS("+init=epsg:32647")
m3_ll@crs <- CRS("+init=epsg:32647")
mkg_ll <- projectRaster(mkg_ll, crs=CRS("+init=epsg:32647"))
rf <- writeRaster(mkg_ll, filename="wetsurf/mkg_wetsurf.tif", format="GTiff", overwrite=TRUE)
rf <- writeRaster(m1_ll, filename="wetsurf/m1_wetsurf.tif", format="GTiff", overwrite=TRUE)
rf <- writeRaster(m2_ll, filename="wetsurf/m2_wetsurf.tif", format="GTiff", overwrite=TRUE)
rf <- writeRaster(m3_ll, filename="wetsurf/m3_wetsurf.tif", format="GTiff", overwrite=TRUE)

###########################
###########################

m1_dry <- raster("dryprec/m1_avg_dry_prec_mmd.asc")
m2_dry <- raster("dryprec/m2_avg_dry_prec_mmd.asc")
m3_dry <- raster("dryprec/m3_avg_dry_prec_mmd.asc")
mkg_dry <- raster("dryprec/mkg_avg_dry_prec_mmd.asc")
m1_dry@crs <- CRS("+init=epsg:32647")
m2_dry@crs <- CRS("+init=epsg:32647")
m3_dry@crs <- CRS("+init=epsg:32647")
mkg_dry@crs <- CRS("+init=epsg:32648")
mkg_dry <- projectRaster(mkg_dry, crs=CRS("+init=epsg:32647"))
rf <- writeRaster(mkg_dry, filename="dryprec/mkg_dryprec.tif", format="GTiff", overwrite=TRUE)
rf <- writeRaster(m1_dry, filename="dryprec/m1_dryprec.tif", format="GTiff", overwrite=TRUE)
rf <- writeRaster(m2_dry, filename="dryprec/m2_dryprec.tif", format="GTiff", overwrite=TRUE)
rf <- writeRaster(m3_dry, filename="dryprec/m3_dryprec.tif", format="GTiff", overwrite=TRUE)