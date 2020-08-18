#--------------------------------------------------------------------------------------------
# Functions for PopCPLmodel package
#--------------------------------------------------------------------------------------------	
checkData <- function(data){
	
	# data: data.frame of 14C dates. Requires 'age' and 'sd'.

	# helper function to check format of data, and throw warnings / errors if required

	x <- 'good'
	if(class(data)!='data.frame'){warning('data must be a data.frame');return('bad')}
	if(sum(names(data)%in%c('age','sd'))!=2){warning("data must include 'age' and 'sd'");return('bad')}
	if(!is.numeric(data$age)){ warning('age must be numeric');return('bad')}	
	if(!is.numeric(data$sd)){warning('sd must be numeric');return('bad')}		
	if(min(data$age)<0){warning('some ages are impossibly young');return('bad')}			
	if(max(data$age)>50000){warning('some ages are impossibly old');return('bad')}	
	if(min(data$sd)<0){warning('some sd are impossibly small');return('bad')}	

return(x)}
#--------------------------------------------------------------------------------------------
checkDatingType <- function(data){

	# used by a couple of functions, so worth avoiding repetition

	# assume all dates are C14 if no datingType is provided
	if('datingType'%in%names(data)){
		if(sum(!data$datingType%in%c('C14','nonC14'))!=0){ warning("Column 'datingType' must comprise only 'C14' and/or 'nonC14'");return(NULL)}	
		}
	if(!'datingType'%in%names(data)){
		data$datingType <- 'C14'
		warning('data did not contain datingType, so all dates are assumed to be C14')
		}
return(data)}
#--------------------------------------------------------------------------------------------
makeCalArray <- function(calcurve,calrange,inc=5){

	# calcurve: the object intcal13 loaded from intcal13.RData, or any other calibration curve
	# calrange: vector of two values giving a calendar range to analyse (BP). Narrowing the range from the default c(0,50000) reduces memory and time.
	# inc: increments to interpolate calendar years. 
	
	# builds a matrix of probabilities representing the calibration curve
	# rows of the matrix represent c14 years (annual resolution)
	# columns of the matrix use the calcurve C14 date and error to form Gaussian distributions (5 year resolution)
	# therefore it is rather memory intensive, and takes a while, but is only required once for any number of dates

	# extract the requested section
	calmin <- min(calrange)
	calmax <- max(calrange)

	# an extra 250 years is required to avoid edge effects
	calmin.extra <- max((calmin - 250),0)
	calmax.extra <- min((calmax + 250),max(calcurve$cal))

	# interpolate the calcurve to a 5 yr cal resolution
	cal <- seq(calmin.extra,calmax.extra,by=inc)
	c14.interp <- approx(x=calcurve$cal,y=calcurve$C14,xout=cal)$y
	error.interp <- approx(x=calcurve$cal,y=calcurve$error,xout=cal)$y

	# sensible c14 range, at 1 yr resolution
	c14 <- min(c14.interp):max(c14.interp)

	R <- length(c14)
	C <- length(cal)
	probs <- array(0,c(R,C))	
	for(i in 1:C)probs[,i] <- dnorm(c14,c14.interp[i],error.interp[i]) 

	row.names(probs) <- c14
	colnames(probs) <- cal

	# store a bunch of objects
	CalArray <- list(
		probs = probs,
		calcurve = calcurve,
		cal = cal[cal>=calmin & cal<=calmax],
		calrange = calrange,
		inc = inc
		)

	# give it an attribute, so other functions can check it
	attr(CalArray, 'creator') <- 'makeCalArray'

return(CalArray)}
#--------------------------------------------------------------------------------------------
plotCalArray <- function(CalArray){
	
	# CalArray: matrix created by makeCalArray(). Requires row and col names corresponding to c14 and cal years respectively
	# Generates an image plot of the stacked Gaussians that define the calibration curve

	# check argument
	if(attr(CalArray, 'creator')!= 'makeCalArray') stop('CalArray was not made by makeCalArray()' )

	P <- CalArray$probs
	c14 <- as.numeric(row.names(P))
	cal <- as.numeric(colnames(P))
	image(cal,c14,t(P)^0.1,xlab='Cal BP',ylab='C14',xlim=rev(range(cal)),col = heat.colors(20),las=1, cex.axis=0.7, cex.lab=0.7)
	}
#--------------------------------------------------------------------------------------------	
plotPD <- function(x){
	years <- as.numeric(row.names(x))
	plot(NULL, type = "n", bty = "n", xlim = rev(range(years)), ylim=c(0,max(x)*1.2),las = 1, cex.axis = 0.7, cex.lab = 0.7, ylab='PD',xlab='calBP')
	for(n in 1:ncol(x)){
		prob <- x[,n]
 		polygon(x = c(years, years[c(length(years), 1)]), y = c(prob, 0, 0), col = "grey", border = 'steelblue')
		}
	if(ncol(x)>1)text(x=colSums(x*years)/colSums(x), y=apply(x, 2, max), labels=names(x),cex=0.7, srt=90)
}

#--------------------------------------------------------------------------------------------	
chooseCalrange <- function(data,calcurve){

	# data: data.frame of dates. Requires 'age' and 'sd' and datingType
	# calcurve: the object 'intcal13' loaded from intcal13.RData, or any other calibration curve

	calmin <- min <- calmax <- max <- NA

	# choose a reasonable calrange for the C14 data
	C14.data <- subset(data,datingType=='C14')
	if(nrow(C14.data)>0){
		c14min <- min(C14.data$age - 5*pmax(C14.data$sd,20))
		c14max <- max(C14.data$age + 5*pmax(C14.data$sd,20))
		calmin <- min(calcurve$cal[calcurve$C14>c14min])
		calmax <- max(calcurve$cal[calcurve$C14<c14max])
		}

	# choose a reasonable calrange for the nonC14 data
	nonC14.data <- subset(data,datingType=='nonC14')
	if(nrow(nonC14.data)>0){
		min <- min(nonC14.data$age - 5*pmax(nonC14.data$sd,20))
		max <- max(nonC14.data$age + 5*pmax(nonC14.data$sd,20))
		}

	calrange <- c(min(calmin,min,na.rm=T),max(calmax,max,na.rm=T))
return(calrange)}
#--------------------------------------------------------------------------------------------	
binner <- function(data, width, calcurve){
	# Arguments:
	#	data:	data.frame containing at least the following columns :"age", "site", "datingType"
	#	width:  any time interval in c14 time, default = 200 c14 years
	#	calcurve: the object 'intcal13' loaded from intcal13.RData, or any other calibration curve

	# argument checks
	if(!is.numeric(width))stop('width must be numeric')
	if(width<1)stop('width must be > 1')
	if(!is.data.frame(calcurve))stop('calcurve format must be data frame with cal, C14 and error')
	if(sum(names(calcurve)%in%c('cal','C14','error'))!=3)stop('calcurve format must be data frame with cal, C14 and error')

	# approximate nonC14 dates ito C14 time, so they can also be binned
	data.C14 <- subset(data,datingType=='C14')
	data.nonC14 <- subset(data,datingType=='nonC14')
	data.C14$c14age <- data.C14$age
	if(nrow(data.nonC14)>0)data.nonC14$c14age <- approx(x=calcurve$cal, y=calcurve$C14, xout=data.nonC14$age)$y
	data <- rbind(data.C14,data.nonC14)
	data <- data[order(data$site,data$c14age),]
	data$phase <- NA

	# binning
	end <- 0
	for(s in unique(data$site)){
		site.data <- subset(data,site==s)
		bins <- c()
		gaps <- c(0,diff(site.data$c14age))
		bin <- 1
		n <- nrow(site.data)
		for(i in 1:n){
			if(gaps[i]>width)bin <- bin+1
			bins[i] <- paste(s,bin,sep='.')
			}
		
		start <- end + 1
		end <- start + n - 1
		data$phase[start:end] <- bins
		}
return(data)}
#--------------------------------------------------------------------------------------------	
internalCalibrator <- function(data, CalArray){

	# generate a summed probability distribution (SPD) of calibrated 14C dates
	# This is acheived quickly by converting the uncalibrated dates using a prior, summing, then calibrating once
	# This is equivalent to calibrating each date, then summing, but hugely faster
	# calibrates across the full range of CalArray. Truncation is performed outside this function

	# data: data.frame of 14C dates. Requires 'age' and 'sd'.
	# CalArray: object created by makeCalArray(). Requires row and col names corresponding to c14 and cal years respectively

	# check argument
	if(attr(CalArray, 'creator')!= 'makeCalArray') stop('CalArray was not made by makeCalArray()' )

	if(nrow(data)==0){
		result <- data.frame(calBP=CalArray$cal,prob=0)
		return(result)
		}
	
	# generate prior (based on calibration curve, some c14 dates are more likely than others)
	c14.prior <- rowSums(CalArray$prob)

	# all c14 likelihoods (Gaussians in c14 time)
	c14 <- as.numeric(row.names(CalArray$prob)) 
	all.dates <- t(array(c14,c(length(c14),nrow(data)))) 
	all.c14.lik <- dnorm(all.dates, mean=as.numeric(data$age), sd=as.numeric(data$sd))

	#  all c14 probabilities (bayes theorem requires two steps, multiply prior by likelihood, then divide by integral)
	all.c14.prob <- c14.prior * t(all.c14.lik)
	all.c14.prob <- t(all.c14.prob) / colSums(all.c14.prob,na.rm=T)
	all.c14.prob[is.nan(all.c14.prob)] <- 0 # the division above will cause NaN if 0/0

	# in the circumstance that some of the date falls outside the calibration curve range
	all.c14.prob <- all.c14.prob * rowSums(all.c14.lik)

	# combined c14 prob
	c14.prob <- colSums(all.c14.prob)

	# calibrate to cal
	# division by prior to ensure the probability of all calendar dates (for a given c14) sum to 1
	# ie, use the calibration curve to generate a probability for calendar time (for every c14)
	# this combines the c14 probability distribution with the calendar probabilities given a c14 date. 
	cal.prob <- as.numeric(crossprod(as.numeric(c14.prob),CalArray$prob/c14.prior))

	# truncate to the required range
	result <- data.frame(calBP=as.numeric(colnames(CalArray$prob)),prob=cal.prob)
	result <- subset(result, calBP>=min(CalArray$calrange) & calBP<=max(CalArray$calrange))

return(result)}
#--------------------------------------------------------------------------------------------
summedCalibrator <- function(data, CalArray, normalise = TRUE){

	# 1. performs a few checks
	# 2. separates data into C14 for calibration using internalCalibrator(), and nonC14 dates
	# 3. combines PDs to produce an SPD
	# 4. reduces the SPD to the orginal required range and applies normalisation if required

	# check arguments
	if(checkData(data)=='bad')stop()
	if(attr(CalArray, 'creator')!= 'makeCalArray') stop('CalArray was not made by makeCalArray()' )

	if(nrow(data)==0){
		result <- data.frame(rep(0,length(CalArray$cal)))
		row.names(result) <- CalArray$cal
		names(result) <- NULL
		return(result)
		}

	data <- checkDatingType(data)

	# C14
	C14.data <- subset(data, datingType=='C14')
	C14.PD <- internalCalibrator(C14.data, CalArray)$prob
	C14.PD <- C14.PD/CalArray$inc # PD needs adjusting by inc, but not for nonC14, as done automatically by dnorm()

	# nonC14
	nonC14.data <- subset(data, datingType=='nonC14')
	n <- nrow(nonC14.data)
	nonC14.PD <- colSums(matrix(dnorm(rep(CalArray$cal,each=n), nonC14.data$age, nonC14.data$sd),n,length(CalArray$cal)))

	# combine and adjust
	PD <- nonC14.PD + C14.PD
	if(normalise)PD <- PD / nrow(data) 	# normalise by number of samples, not by total PD. This means total normalised PD can be less than 1, in cases where CalArray is badly specified to the dataset, or visaversa

	result <- data.frame(PD)
	row.names(result) <- CalArray$cal
	names(result) <- NULL
return(result)}
#--------------------------------------------------------------------------------------------	
phaseCalibrator <- function(data, CalArray, width = 200){
	
	# generates a normalised SPD for every phase, phasing data through binner if required

	# argument checks
	if(!is.numeric(width))stop('width must be numeric')
	if(width<1)stop('width must be > 1')
	if(attr(CalArray, 'creator')!= 'makeCalArray') stop('CalArray was not made by makeCalArray()' )
	if(nrow(data)==0)return(NULL)
 	if(checkData(data)=='bad')stop()
	data <- checkDatingType(data)

	# ensure dates are phased
	if(!'phase'%in%names(data)){
		if(!'site'%in%names(data))stop("data must contain 'phase' or 'site'")
		calcurve <- CalArray$calcurve
		data <- binner(data=data, width=width, calcurve=calcurve)
		warning("data did not contain 'phase', so phases were generated automatically")
		}

	phases <- sort(unique(data$phase))
	phase.SPDs <- array(0,c(length(CalArray$cal),length(phases)))

	for(p in 1:length(phases)){
		phase.data <- subset(data,phase==phases[p])
		if(p==2)options(warn=(-1)) # only need to report for first iteration
		phase.SPDs[,p] <- summedCalibrator(phase.data, CalArray, normalise = TRUE)[,1]
		}
	options(warn=0) # back to default

	phase.SPDs <- as.data.frame(phase.SPDs); names(phase.SPDs) <- phases; row.names(phase.SPDs) <- CalArray$cal
	
return(phase.SPDs)}
#--------------------------------------------------------------------------------------------	
summedCalibratorWrapper <- function(data,calcurve=intcal13){

	# data: data.frame of 14C dates. Requires 'age' and 'sd' and optional datingType
	# calcurve: the object intcal13 loaded from intcal13.RData, or any other calibration curve

	# function to easily plot a calibrated Summed Probability Distribution from 14C dates
	# takes care of choosing a sensible date and interpolation increments range automatically

	if(nrow(data)==0)return(NULL)
 	if(checkData(data)=='bad')stop()
	data <- checkDatingType(data)

	calrange <- chooseCalrange(data,calcurve)

	int <- 5
	if(diff(calrange)>10000)int <- 10
	if(diff(calrange)>25000)int <- 20
	CalArray <- makeCalArray(calcurve,calrange,int)
	SPD <- summedCalibrator(data,CalArray)
	plotPD(SPD)
return(SPD)	}
#--------------------------------------------------------------------------------------------	
summedPhaseCalibrator <- function(data, calcurve, calrange, inc=5, width=200){
	CalArray <- makeCalArray(calcurve=calcurve, calrange=calrange, inc=inc)
	x <- phaseCalibrator(data=data, CalArray=CalArray, width=width)
	SPD <- as.data.frame(rowSums(x))

	# normalise
	SPD <- SPD/sum(SPD)
	SPD <- SPD/CalArray$inc
return(SPD)}
#--------------------------------------------------------------------------------------------	
uncalibrateCalendarDates <- function(dates, calcurve){
	# dates: vector of calendar dates (point estimates)
	# randomly samples dates the calcurve error, at the corresponding cal date
	# returns a vector of point estimates on 14C scale 

	simC14.means <- approx(x=calcurve$cal,y=calcurve$C14,xout=dates)$y 
	simC14.errors <- approx(x=calcurve$cal,y=calcurve$error,xout=dates)$y 
	i <- !is.na(simC14.means) & !is.na(simC14.errors)
	simC14Samples <- rnorm(n=sum(i),mean=simC14.means[i],sd=simC14.errors[i])
return(round(simC14Samples))}
#--------------------------------------------------------------------------------------------	
