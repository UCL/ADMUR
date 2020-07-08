#--------------------------------------------------------------------------------------------
# Functions for PopCPLmodel package
#--------------------------------------------------------------------------------------------
#' @export
makeCalArray <- function(calcurve,calrange,inc=5){

	# calcurve: the object intcal13 loaded from intcal13.RData, or any other calibration curve
	# calrange: vector of two values giving a calendar range to analyse (BP). Narrowing the range from the default c(0,50000) reduces memory and time.
	
	# builds a matrix of probabilities representing the calibration curve
	# rows of the matrix represent c14 years (annual resolution)
	# columns of the matrix use the calcurve C14 date and error to form Gaussian distributions (5 year resolution)
	# therefore it is rather memory intensive, and takes a while, but is only required once for any number of dates

	# extract the requested section
	calmin <- min(calrange)
	calmax <- max(calrange)

	# interpolate the calcurve to a 5 yr cal resolution
	cal <- seq(calmin,calmax,by=inc)
	c14.interp <- approx(x=calcurve$cal,y=calcurve$C14,xout=cal)$y
	error.interp <- approx(x=calcurve$cal,y=calcurve$error,xout=cal)$y

	# sensible c14 range, at 1 yr resolution
	c14 <- min(c14.interp):max(c14.interp)

	R <- length(c14)
	C <- length(cal)
	CalArray <- array(0,c(R,C))	
	for(i in 1:C)CalArray[,i] <- dnorm(c14,c14.interp[i],error.interp[i]) 

	row.names(CalArray) <- c14
	colnames(CalArray) <- cal
return(CalArray)}
#--------------------------------------------------------------------------------------------
#' @export
plotCalArray <- function(CalArray){
	
	# CalArray: matrix created by makeCalArray(). Requires row and col names corresponding to c14 and cal years respectively

	# Generates an image plot of the stacked Gaussians that define the calibration curve

	c14 <- as.numeric(row.names(CalArray))
	cal <- as.numeric(colnames(CalArray))
	image(cal,c14,t(CalArray)^0.1,xlab='Cal BP',ylab='C14',xlim=rev(range(cal)),col = heat.colors(20))
	}
#--------------------------------------------------------------------------------------------	
#' @export
summedCalibrator <- function(CalArray,data){

	# function to generate a summed probability distribution (SPD) of calibrated dates
	# This is acheived quickly by converting the uncalibrated dates using a prior, summing, then calibrating once
	# This is equivalent to calibrating each date, then summing, but hugely faster

	# CalArray: matrix created by makeCalArray(). Requires row and col names corresponding to c14 and cal years respectively
	# data: data.frame of 14C dates. Requires 'age' and 'sd'.

	if(check.data(data)=='bad')stop()

	# pull out the c14 and cal vectors
	c14 <- as.numeric(row.names(CalArray))
	cal <- as.numeric(colnames(CalArray))

	# generate prior (based on calibration curve, some c14 dates are more likely than others)
	c14.prior <- rowSums(CalArray)

	# all c14 likelihoods (Gaussians in c14 time)
	all.dates <- t(array(c14,c(length(c14),nrow(data)))) 
	all.c14.lik <- dnorm(all.dates, mean=as.numeric(data$age), sd=as.numeric(data$sd))

	#  all c14 probabilities (bayes theorem requires two steps, multiply prior by likelihood, then divide by integral)
	all.c14.prob <- c14.prior * t(all.c14.lik)
	all.c14.prob <- t(all.c14.prob) / colSums(all.c14.prob,na.rm=T)

	# in the bizarre circumstance that some of the date falls outside the calibration curve range
	all.c14.prob <- all.c14.prob * rowSums(all.c14.lik)

	# combined c14 prob
	c14.prob <- colSums(all.c14.prob)

	# calibrate to cal
	# division by prior to ensure the probability of all calendar dates (for a given c14) sum to 1
	# ie, use the calibration curve to generate a probability for calendar time (for every c14)
	# this combines the c14 probability distribution with the calendar probabilities given a c14 date. 
	cal.prob <- as.numeric(crossprod(as.numeric(c14.prob),CalArray/c14.prior))

	# cal is stored at a resolution of 5 yrs, so PD needs adjusting accordingly 
	inc <- diff(cal)[1] #  pulls this from the CalArray
	cal.prob <- cal.prob/inc 

result <- data.frame(calBP=cal,prob=cal.prob)
return(result)}
#--------------------------------------------------------------------------------------------	
#' @export
plot.SPD <- function(SPD){
	
	# SPD: the output of summedCalibrator()
	
	# performs a very basic plot of the SPD, but automatically trims to a sensible range

	plot(SPD,type='n',xlim=rev(range(SPD$calBP)))
	polygon(x=c(SPD$calBP,SPD$calBP[c(nrow(SPD),1)]), y=c(SPD$prob,0,0),col='grey', border=F )
	}
#--------------------------------------------------------------------------------------------	
#' @export
choose.calrange <- function(data,calcurve){

	# data: data.frame of 14C dates. Requires 'age' and 'sd'.
	# calcurve: the object 'intcal13' loaded from intcal13.RData, or any other calibration curve

	if(check.data(data)=='bad')stop()

	# chooses a reasonable calrange from the data
	c14min <- min(data$age - 5*pmax(data$sd,20))
	c14max <- max(data$age + 5*pmax(data$sd,20))

	calmin <- min(calcurve$cal[calcurve$C14>c14min])
	calmax <- max(calcurve$cal[calcurve$C14<c14max])

	calrange <- c(calmin,calmax)
return(calrange)}
#--------------------------------------------------------------------------------------------	
#' @export
SPD.wrapper <- function(data,calcurve=intcal13){

	# data: data.frame of 14C dates. Requires 'age' and 'sd'.
	# calcurve: the object intcal13 loaded from intcal13.RData, or any other calibration curve

	# function to easily plot a calibrated Summed Probability Distribution from 14C dates
	# takes care of choosing a sensible date and interpolation increments range automatically

	if(check.data(data)=='bad')stop()

	calrange <- choose.calrange(data,calcurve)
	int <- 5
	if(diff(calrange)>10000)int <- 10
	if(diff(calrange)>25000)int <- 20
	CalArray <- makeCalArray(intcal13,calrange,int)
	SPD <- summedCalibrator(CalArray,data)
	plot.SPD(SPD)
	}
#--------------------------------------------------------------------------------------------	
#' @export
check.data <- function(data){
	
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


