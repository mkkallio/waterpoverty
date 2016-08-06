#### process precipitation stations

######################### process prec timeseries data to get consecutive drought days
droughtdays <- function(data) {
	days <- nrow(data)
	ddays <- 0
	curdays <- 0
	maxdays <- 0
	
	yearddays <- vector()
	yeardays <- 0
	yearcurdays <- 0
	yearmaxdays <- 0
	yearmax <- vector()
	
	
	
	for (i in 1:nrow(data)) {
		if (data[i,2] < 1) {
			ddays <- ddays+1
			curdays <- curdays+1
			
			
			yeardays <- yeardays+1
			yearcurdays <- yearcurdays+1
			if (curdays > maxdays) {
				maxdays <- curdays
			}	
			if (yearcurdays > yearmaxdays) {
				yearmaxdays <- yearcurdays
			}
			
		} else {
			curdays <- 0
			yearcurdays <- 0
		}
		
		#if the date is Nov 15 (beginning of dry season), record yearly max and ddays.
		droughtstart <- ifelse(grepl("1115", data[i,1]), TRUE, FALSE)
		if (droughtstart == TRUE) {
			yearmax <- c(yearmax, yearmaxdays)
			yearmaxdays <- 0
			
			yearddays <- c(yearddays, yeardays)
			yeardays <- 0
		}
	}
	
	
	
	tabledata <- c(days, ddays, maxdays, mean(yearmax), mean(yearddays))
	#colnames(tabledata) <- c('Number of days','Days with no rain','max consecutive drought days', 'Average max consecutive drought days', 'average days with no rain')
	return (tabledata)
}




#read data
prec_stations <- read.table('prec_stations.csv')

#loop through the stations, read data, and assess the droughts
maxdd <- data.frame()

for (i in 1:nrow(prec_stations)) {
	filename <- as.character(prec_stations[i,4])
	prec <- read.table(paste('prec_files/',filename, sep=""), skip=9)
	prec <- cbind(prec[,1],as.numeric(prec[,2]))
	output <- droughtdays(prec)
	if (i == 1) {
		maxdd <- rbind(output)
	} else {
		maxdd <- rbind(maxdd, output)
	}
}
colnames(maxdd) <- c('Number of days','Days with no rain','max consecutive drought days', 'Average max consecutive drought days', 'average days with no rain')
rownames(maxdd) <- prec_stations[,1]

