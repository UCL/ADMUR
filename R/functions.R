#--------------------------------------------------------------------------------------------
# R functions for ADMUR package
#--------------------------------------------------------------------------------------------	

#--------------------------------------------------------------------------------------------	
# global variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c('datingType','site','calBP','phase','intcal20'))
#--------------------------------------------------------------------------------------------

checkData <- function(data){
	# data: data.frame of 14C dates. Requires 'age' and 'sd'.
	# helper function to check format of data, and throw warnings

	x <- 'good'
	if(class(data)!='data.frame'){warning('data must be a data.frame');return('bad')}
	if(sum(names(data)%in%c('age','sd'))!=2){warning("data must include 'age' and 'sd'");return('bad')}
	if(!is.numeric(data$age)){warning('age must be numeric');return('bad')}	
	if(!is.numeric(data$sd)){warning('sd must be numeric');return('bad')}		
	if(min(data$age)<0){warning('some ages are negative');return('bad')}			
	if(min(data$sd)<1){warning('some sd are impossibly small');return('bad')}	

return(x)}
#--------------------------------------------------------------------------------------------
checkDatingType <- function(data){
	# used by a couple of functions, so worth avoiding repetition

	# assume all dates are 14C if no datingType is provided
	if(!'datingType'%in%names(data)){
		data$datingType <- '14C'
		warning('data did not contain datingType, so all dates are assumed to be 14C')
		}
	# avoid misspellings of '14C'
	bad <- c('14c','C14','c14','14.C','14.c','c.14','C.14','c-14','C-14','14-C')
	i <- data$datingType%in%bad
	if(sum(i)>0){
		warning("Misspelling of '14C' in datingType are assumed to need calibrating")
		data$datingType[i] <- '14C'
		}
		
return(data)}
#--------------------------------------------------------------------------------------------
makeCalArray <- function(calcurve,calrange,inc=5){

	# calcurve: intcal20 or any other calibration curve
	# calrange: vector of two values giving a calendar range to analyse (BP). Narrowing the range from the default c(0,50000) reduces memory and time.
	# inc: increments to interpolate calendar years. 
	
	# builds a matrix of probabilities representing the calibration curve
	# rows of the matrix represent c14 years (annual resolution)
	# columns of the matrix use the calcurve 14C date and error to form Gaussian distributions
	# therefore it is rather memory intensive, and takes a while, but is only required once for any number of dates

	# extract the requested section
	calmin <- min(calrange)
	calmax <- max(calrange)

	# an extra 200 years is required to avoid edge effects
	calmin.extra <- max((calmin - 200),0)
	calmax.extra <- calmax + 200

	# include an extra data row in the calibration curve, to 'feather' extremely old dates onto a 1-to-1 mapping of 14C to cal time
	calcurve <- rbind(data.frame(cal=60000,C14=60000,error=calcurve$error[1]),calcurve)

	# interpolate the calcurve to the required resolution
	cal <- seq(calmin.extra,calmax.extra,by=inc)
	c14.interp <- approx(x=calcurve$cal,y=calcurve$C14,xout=cal)$y
	error.interp <- approx(x=calcurve$cal,y=calcurve$error,xout=cal)$y

	# assume a one-to-one mapping of cal and c14 beyond 60000
	i <- is.na(c14.interp)
	c14.interp[i] <- cal[i]
	error.interp[i] <- calcurve$error[1]

	# pick a sensible c14 range, at 1 yr resolution
	min.c14 <- round(min(c14.interp - 4*error.interp))
	if(min.c14<0)min.c14 <- 0
	max.c14 <- round(max(c14.interp + 4*error.interp))
	c14 <- min.c14:max.c14

	# fill the array
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
	colfunc <- colorRampPalette(c('white','steelblue'))
	image(cal,c14,t(P)^0.1,xlab='Cal BP',ylab='14C',xlim=rev(range(cal)),col = colfunc(20),las=1, cex.axis=0.7, cex.lab=0.7)
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

	# choose a reasonable calrange for the 14C data
	C14.data <- subset(data,datingType=='14C')
	if(nrow(C14.data)>0){
		c14min <- min(C14.data$age - 5*pmax(C14.data$sd,20))
		c14max <- max(C14.data$age + 5*pmax(C14.data$sd,20))
		calmin <- min(calcurve$cal[calcurve$C14>c14min])
		calmax <- max(calcurve$cal[calcurve$C14<c14max])
		}

	# choose a reasonable calrange for the nonC14 data
	nonC14.data <- subset(data,datingType!='14C')
	if(nrow(nonC14.data)>0){
		min <- min(nonC14.data$age - 5*pmax(nonC14.data$sd,20))
		max <- max(nonC14.data$age + 5*pmax(nonC14.data$sd,20))
		}

	calrange <- c(min(calmin,min,na.rm=T),max(calmax,max,na.rm=T))
return(calrange)}
#--------------------------------------------------------------------------------------------	
binner <- function(data, width, calcurve){

	#	data:	data.frame containing at least the following columns :"age", "site", "datingType"
	#	width:  any time interval in c14 time, default = 200 c14 years
	#	calcurve: the object 'intcal13' loaded from intcal13.RData, or any other calibration curve

	# argument checks
	if(!is.numeric(width))stop('width must be numeric')
	if(width<1)stop('width must be > 1')
	if(!is.data.frame(calcurve))stop('calcurve format must be data frame with cal, C14 and error')
	if(sum(names(calcurve)%in%c('cal','C14','error'))!=3)stop('calcurve format must be data frame with cal, C14 and error')
	if('phase'%in%names(data))warning('Data was already phased, so should not have been handed to binner(). Check for internal bug')

	# approximate nonC14 dates ito C14 time, so they can also be binned
	data.C14 <- subset(data,datingType=='14C')
	data.nonC14 <- subset(data,datingType!='14C')
	data.C14$c14age <- data.C14$age
	if(nrow(data.nonC14)>0)data.nonC14$c14age <- approx(x=calcurve$cal, y=calcurve$C14, xout=data.nonC14$age)$y
	data <- rbind(data.C14,data.nonC14)
	if(length(unique(data$site))>1)data <- data[order(data$site,data$c14age),]
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
	# This is equivalent to calibrating each date, then summing, but faster
	# calibrates across the full range of CalArray. 

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

	# all c14 likelihoods (Either Gaussians  or lognormals in c14 time)
	c14 <- as.numeric(row.names(CalArray$prob)) 
	all.dates <- t(array(c14,c(length(c14),nrow(data)))) 
	# gaussian
	# all.c14.lik <- dnorm(all.dates, mean=as.numeric(data$age), sd=as.numeric(data$sd))
	# or log normals
	m <- as.numeric(data$age)
	v <- as.numeric(data$sd)^2
	mu <- log(m^2/sqrt(v+m^2))
	sig <- sqrt(log(v/m^2+1))
	all.c14.lik <- dlnorm(all.dates, meanlog=mu, sdlog=sig)

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
summedCalibrator <- function(data, CalArray, normalise = 'standard', checks = TRUE){

	# performs a few checks
	# separates data into C14 for calibration using internalCalibrator(), and nonC14 dates
	# combines PDs to produce an SPD
	# reduces the SPD to the orginal required range and applies normalisation if required

	if(nrow(data)==0){
		result <- data.frame(rep(0,length(CalArray$cal)))
		row.names(result) <- CalArray$cal
		names(result) <- NULL
		return(result)
		}

	# check arguments
	if(checks){
		if(checkData(data)=='bad')stop()
		if(attr(CalArray, 'creator')!= 'makeCalArray') stop('CalArray was not made by makeCalArray()' )
		if(!normalise %in% c('none','standard','full')) stop('normalise must be none, standard or full')
		data <- checkDatingType(data)
		}

	# C14
	C14.data <- subset(data, datingType=='14C')
	C14.PD <- internalCalibrator(C14.data, CalArray)$prob
	C14.PD <- C14.PD/CalArray$inc # PD needs dividing by inc to convert area to height (PMF to PDF). Not required for nonC14, as dnorm() already generated PDF

	# nonC14
	nonC14.data <- subset(data, datingType!='14C')
	n <- nrow(nonC14.data)
	nonC14.PD <- colSums(matrix(dnorm(rep(CalArray$cal,each=n), nonC14.data$age, nonC14.data$sd),n,length(CalArray$cal)))

	# combine
	PD <- nonC14.PD + C14.PD

	# No normalisation. Results in a PD with an area equal to the total number of samples minus any probability mass outside the date range. For example in cases where CalArray is badly specified to the dataset, or visaversa.
	# Rarely required.
	if(normalise=='none'){
		result <- data.frame(PD)
		}

	# Standard normalisation adjusts for the number of samples. Results in a PD with a total area of 1 minus any probability mass outside the date range. For example in cases where CalArray is badly specified to the dataset, or visaversa.
	# should be used with phaseCalibrator().
	if(normalise=='standard'){
		PD <- PD / nrow(data) 
		result <- data.frame(PD)
		}

	# Full normalisation results in a true PDF with a total area equal to 1.
	# Should be used when all dates are of equal importance, and the resulting SPD is the final product. For example when generating simulated SPDs.
	if(normalise=='full'){
		PD <- PD / (sum(PD) * CalArray$inc)
		result <- data.frame(PD)
		}

	row.names(result) <- CalArray$cal
	names(result) <- NULL
return(result)}
#--------------------------------------------------------------------------------------------	
phaseCalibrator <- function(data, CalArray, width = 200, remove.external = FALSE){
	
	# generates a normalised SPD for every phase, phasing data through binner if required
	# remove.external: exludes phases (columns) with less than half their probability mass outside the date range. Useful for modelling.

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
		phase.SPDs[,p] <- summedCalibrator(phase.data, CalArray, normalise = 'standard', checks = FALSE)[,1]
		}

	phase.SPDs <- as.data.frame(phase.SPDs); names(phase.SPDs) <- phases; row.names(phase.SPDs) <- CalArray$cal

	# remove phases that are mostly outside the date range
	if(remove.external){
		keep.i <- colSums(phase.SPDs)>=(0.5 / CalArray$inc)
		phase.SPDs <- phase.SPDs[,keep.i, drop=FALSE]
		}

return(phase.SPDs)}
#--------------------------------------------------------------------------------------------	
summedCalibratorWrapper <- function(data, calcurve=intcal20, plot=TRUE){

	# data: data.frame of 14C dates. Requires 'age' and 'sd' and optional datingType
	# calcurve: the object intcal13 loaded from intcal13.RData, or any other calibration curve

	# function to easily plot a calibrated Summed Probability Distribution from 14C dates
	# takes care of choosing a sensible date and interpolation increments range automatically

	if(nrow(data)==0)return(NULL)
 	if(checkData(data)=='bad')stop()
	data <- checkDatingType(data)

	calrange <- chooseCalrange(data,calcurve)

	inc <- 5
	if(diff(calrange)>10000)inc <- 10
	if(diff(calrange)>25000)inc <- 20
	CalArray <- makeCalArray(calcurve,calrange,inc)
	SPD <- summedCalibrator(data,CalArray, normalise='standard')
	if(plot)plotPD(SPD)
return(SPD)	}
#--------------------------------------------------------------------------------------------	
summedPhaseCalibrator <- function(data, calcurve, calrange, inc=5, width=200){
	CalArray <- makeCalArray(calcurve=calcurve, calrange=calrange, inc=inc)
	x <- phaseCalibrator(data=data, CalArray=CalArray, width=width, remove.external = FALSE)
	SPD <- as.data.frame(rowSums(x))

	# normalise
	SPD <- SPD/(sum(SPD) * CalArray$inc)
	names(SPD) <- NULL
return(SPD)}
#--------------------------------------------------------------------------------------------	
uncalibrateCalendarDates <- function(dates, calcurve){

	# dates: vector of calendar dates (point estimates)
	# randomly samples dates the calcurve error, at the corresponding cal date
	# returns a vector of point estimates on 14C scale 

	# include an extra data row in the calibration curve, to 'feather' extremely old dates onto a 1-to-1 mapping of 14C to cal time
	calcurve <- rbind(data.frame(cal=60000,C14=60000,error=calcurve$error[1]),calcurve)

	simC14.means <- approx(x=calcurve$cal,y=calcurve$C14,xout=dates)$y 
	simC14.errors <- approx(x=calcurve$cal,y=calcurve$error,xout=dates)$y 
	simC14Samples <- numeric(length(dates))
	i <- !is.na(simC14.means) & !is.na(simC14.errors)
	simC14Samples[i] <- round(rnorm(n=sum(i),mean=simC14.means[i],sd=simC14.errors[i]))
	simC14Samples[!i] <- round(dates[!i])
return(simC14Samples)}
#--------------------------------------------------------------------------------------------	
loglik <- function(PD, model){

	# relative likelihood of a perfectly precise date is the model PDF
	# therefore the relative likelihood of a date with uncertainty is an average of the model PDF, weighted by the date probabilities.
	# Numerically this is the scalar product: sum of (model PDF x date PDF).

	years <- as.numeric(row.names(PD))
	inc <- (years[2]-years[1])

	# ensure the date ranges exactly match. If not, interpolate model pdf to match PD.
	check <- identical(row.names(PD),model$year)
	if(!check){
		x <- as.numeric(model$year)
		y <- model$pdf
		y.out <- approx(x=x, y=y, xout=years)$y
		model <- data.frame(pdf = y.out, year = years)
		model$pdf <- model$pdf/(sum(model$pdf)*inc)
		}

	# ensure model PD is provided as a discretised PDF
	model$pdf <- model$pdf/(sum(model$pdf)*inc)

	# convert the date PD pdfs to discrete PMFs to perform a weighted average
	PMF <- PD * inc

	# likelihoods weighted by the observational uncertainty
	weighted.PD <- PMF * model$pdf

	# sum all possibilities for each date (a calibrated date's probabilities are OR) to give the relative likelihood for each date. 
	liks <- colSums(weighted.PD)

	# calculate the overall log lik given all the dates
	loglik <- sum(log(liks))

	if(is.nan(loglik))loglik <- -Inf
return(loglik)}
#--------------------------------------------------------------------------------------------	
parametersToPDFcoords <- function(x.par,y.par){

	# converts x.par and y.par parameters to pdf x y coordinates
	# length of y.par should be 1 longer than length of x.par
	# both x.par and y.par vectors should be values between 0 and 1
	# returns x and y values between 0 and 1

	# step 1 and 2 picks each subsequent y value independently from a gamma, whilst x-values are picked using the stick breaking beta process
	# additional constraints are required on the x-sampling to ensure even sampling across the remainder of x (s.a) and remainder of pdf area (s.b)
	# step 3 is the penultimate step, and requires x value to be sampled before y, to ensure the area after it isn't impossible
	# step 3 then requires the y value to be beta constrained by some maximum value
	# finally step 4 exactly calculates the y value, to ensure total area = 1

	if(length(y.par)!= (1+length(x.par)))stop('y.par must be 1 parameter longer than x.par')
	if(sum(c(x.par,y.par)<0 | c(x.par,y.par)>1))stop('x.par and y.par must be between 0 and 1')
	K <- length(y.par)+1
	y <- x <- area <- stick <- numeric(K) 

	# avoid y.par reaching the boundary of 1, where qgamma is Infinite, with a tiny constant
	lim <- 1 - 1e-10
	y.par[y.par>lim] <- lim

	# step 1
	if(K==2)y[1] <- qbeta(y.par[1],1,1)*2
	if(K>2)y[1] <- qgamma(y.par[1],1,1)
	x[1] <- 0
	area[1] <- 0
	stick[1] <- 0

	# step 2
	if(K>3)for(k in 2:(K-2)){
		y[k] <- qgamma(y.par[k],1,1)
		s.a <- 1-x[k-1]
		s.b <-  2*(1-sum(area))/(y[k]+y[k-1])
		s <- min(c(s.a,s.b))
		stick[k] <- qbeta(x.par[k-1],1,K-k)*s
		x[k] <- stick[k] + x[k-1]
		area[k] <- (0.5)*(x[k]-x[k-1])*(y[k]+y[k-1])
		}

	# step 3
	if(K>2)for(k in K-1){
		s.a <- 1-x[k-1]
		s.b <- 2*(1-sum(area))/y[k-1]
		s <- min(c(s.a,s.b))
		stick[k] <- qbeta(x.par[k-1],1,K-k)*s
		x[k] <- stick[k] + x[k-1]
		y.s <- ( 2*(1-sum(area)) +(x[k-1]*y[k-1]) - (x[k]*y[k-1]) ) / (1 - x[k-1])
		y[k] <- qbeta(y.par[k],1,1)*y.s
		area[k] <- (0.5)*(x[k]-x[k-1])*(y[k]+y[k-1])
		}

	# step 4
	x[K] <- 1
	y[K] <- 2*(1-sum(area))/(1-x[K-1]) - y[K-1]
	stick[K] <- 1-x[K-1]
	area[K] <- (0.5)*(x[K]-x[K-1])*(y[K]+y[K-1])

	res <- data.frame(x=x,y=y,area=area,stick=stick)
return(res)}
#--------------------------------------------------------------------------------------------
convertPars <- function(pars, years, type){

	# Important: the model must be returned as a PDF. I.e, the total area must sum to 1.

	# sanity checks
	if(!type%in%c('CPL','exp','uniform','norm','lognorm'))stop('unknown model type. Only CPL, exp, uniform, norm, lognorm currently handled')
	if('data.frame'%in%class(pars))pars <- as.matrix(pars)
	if('integer'%in%class(years))years <- as.numeric(years)
	if(!'numeric'%in%class(years))stop('years must be a numeric vector')

	if(type=='uniform'){
		if(!is.null(pars))stop('A uniform model must have NULL parameters')
		res <- data.frame( year=range(years), pdf=dunif(range(years),min(years),max(years)) )
		}

	if(type=='CPL'){

		# if a single parameter set, generates a few extras
		if('numeric'%in%class(pars))res <- convertParsCPL(pars,years)

		# if a matrix of parameters, only generates the x,y coords
		if(!'numeric'%in%class(pars)){
			N <- nrow(pars)
			C <- (ncol(pars)+1)/2 +1
			yr <- pdf <- as.data.frame(matrix(,N,C))
			names(yr) <- paste('yr',1:C,sep='')
			names(pdf) <- paste('pdf',1:C,sep='')
			for(n in 1:N){
				x <- convertParsCPL(pars[n,],years)
				yr[n,] <- x$year
				pdf[n,] <- x$pdf
				}
			res <- cbind(yr,pdf)
			}
		}

	if(type=='exp'){
		inc <- years[2]-years[1]
		# if a single parameter set, generates a two-column data frame
		if('numeric'%in%class(pars)){
			if(length(pars)!=1)stop('An exponential model must have exactly one parameter')
			approx.pdf <- pars*exp(pars*years - pars*max(years)) # an intermediate fiddle to avoid computing nutty numbers beyond floating point limits
			res <- data.frame(year = years, pdf = approx.pdf/(sum(approx.pdf)*inc))
			}
		# if matrix of parameters, each row is a converted parameter set
		if(!'numeric'%in%class(pars)){
			if(ncol(pars)!=1)stop('An exponential model must have exactly one parameter')
			N <- nrow(pars)
			C <- length(years)
			res <- as.data.frame(matrix(,N,C))
			names(res) <- years
			for(n in 1:N){
				approx.pdf <- pars[n,]*exp(pars[n,]*years - pars[n,]*max(years))
				res[n,] <- approx.pdf/(sum(approx.pdf)*inc)
				}
			}
		}

	if(type=='norm'){
		inc <- years[2]-years[1]
		# if a single parameter set, generates a two-column data frame
		if('numeric'%in%class(pars)){
			if(length(pars)!=2)stop('An norm model must have exactly two parameters')
			approx.pdf <- dunif(years, pars[1], pars[2])
			res < data.frame(year = years, pdf = approx.pdf/(sum(approx.pdf)*inc))		
			}
		# if matrix of parameters, each row is a converted parameter set
		if(!'numeric'%in%class(pars)){
			if(ncol(pars)!=2)stop('An norm model must have exactly two parameters')
			N <- nrow(pars)
			C <- length(years)
			res <- as.data.frame(matrix(,N,C))
			names(res) <- years
			for(n in 1:N){
				approx.pdf <- dunif(years, pars[n,1], pars[n,2])
				res[n,] <- approx.pdf/(sum(approx.pdf)*inc)
				}
			}
		}
return(res)}
#--------------------------------------------------------------------------------------------
convertParsCPL <- function(pars, years){

	# must be odd, as (2n-1 parameters where n=number of pieces)
 	cond <- ((length(pars)+1) %% 2) == 0
	if(!cond)stop('A CPL model must have an odd number of parameters')

	if(length(pars)==1){
		x.par <- c()
		y.par <- pars
		}

	if(length(pars)!=1){
		x.par <- pars[1:((length(pars)-1)/2)]
		y.par <- pars[(length(x.par)+1):length(pars)]
		}

	d <- parametersToPDFcoords(x.par,y.par)
	d$year <- d$x* (max(years)-min(years)) + min(years)

	# generates the true pdf at the hinges
	d$pdf <- d$y / diff(range(d$year))

return(d)}
#--------------------------------------------------------------------------------------------
objectiveFunction <- function(pars, PDarray, type){

	# sanity check a few arguments
	if(!type%in%c('CPL','exp','uniform','norm','lognorm'))stop('unknown model type. Only CPL, exp, uniform, norm, lognorm currently handled')
	if(type=='exponential' & length(pars)!=1)stop('exponential model requires just one rate parameter')
	if(type=='uniform' & !is.null(pars))stop('A uniform model must have NULL parameters')
	if(type=='norm' & !is.null(pars))stop('A norm model must have two parameters, mean and sd')
	if(!is.data.frame(PDarray))stop('PDarray must be a data frame')

	years <- as.numeric(row.names(PDarray))
	inc <- years[2]-years[1]

	if(type=='CPL'){
		pdf <- convertPars(pars,years,type=type)
		model.pdf <- approx(x=pdf$year,y=pdf$pdf,xout=years,ties='ordered',rule=2)$y # why is this required?
		}

	if(type=='exp'){
		model.pdf <- convertPars(pars,years,type=type)$pdf
		}

	if(type=='uniform'){
		model.pdf <- convertPars(pars,years,type=type)$pdf
		}

	if(type=='norm'){
		model.pdf <- convertPars(pars,years,type=type)$pdf
		}
 
	# sometimes a tiny adjustment is required, due to the discretisation
	model.pdf <- model.pdf/(sum(model.pdf)*inc) # not sure this is required if everything is now run through convertPars

	# calculate loglik
	model <- data.frame(pdf=model.pdf, year=years)
	loglik <- loglik(PDarray, model)

return(-loglik)}
#--------------------------------------------------------------------------------------------
proposalFunction <- function(pars, jumps, type){
	moves <- rnorm(length(pars),0,jumps)
	new.pars <- pars + moves

	# bunch of constraints for impossible parameters (usually floating point bullshit)
	if(type=='CPL'){
		new.pars[new.pars<0] <- 0
		new.pars[new.pars>0.999999] <- 0.999999
		}
	if(type=='exp'){
		if(new.pars==0)new.pars <- runif(1,-1e-20,1e-20)
		}
	if(type=='norm'){
		new.pars[new.pars<=0] <- 1e-20
		}
return(new.pars)}
#--------------------------------------------------------------------------------------------
mcmc <- function(PDarray, startPars, type, N = 30000, burn = 2000, thin = 5, jumps = 0.02){ 

	if(!type%in%c('CPL','exp','norm'))stop('unknown model type. Only CPL, exp, norm')

	# starting parameters
	pars <- startPars

	all.pars <- matrix(,N,length(startPars))

	# mcmc loop
	accepted <- rep(0,N)
	for(n in 1:N){
		all.pars[n,] <- pars
		llik <- -objectiveFunction(pars,PDarray,type)
		prop.pars <- proposalFunction(pars, jumps, type)
		prop.llik <- -objectiveFunction(prop.pars,PDarray,type)
		ratio <- min(exp(prop.llik-llik),1)
		move <- sample(c(T,F),size=1,prob=c(ratio,1-ratio))
		if(move){
			pars <- prop.pars
			accepted[n] <- 1
			}
		if(n%in%seq(0,N,by=1000))print(paste(n,'iterations of',N))
		}

	ar <- sum(accepted[burn:N])/(N-burn)

	# thinning
	i <- seq(burn,N,by=thin)
	res <- all.pars[i,]

return(list(res=res,all.pars=all.pars, acceptance.ratio=ar))}
#--------------------------------------------------------------------------------------------
simulateCalendarDates <- function(model, n){

	# sanity check a few arguments
	if(!is.data.frame(model))stop('model must be a data frame')
	cond <- sum(c('year','pdf')%in%names(model))
	if(cond!=2)stop('model must include year and pdf')

	x <- range(model$year) + c(-150,150)
	years.wide <- min(x):max(x)
	pdf.wider <- approx(x=model$year,y=model$pdf,xout=years.wide,ties='ordered',rule=2)$y 
	dates <- sample(years.wide, replace=T, size=n, prob=pdf.wider)
return(dates)}
#--------------------------------------------------------------------------------------------
estimateDataDomain <- function(data, calcurve){

	thresholds <- c(60000,20000,4000)
	incs <- c(100,20,1)
	min.year <- 0
	max.year <- 60000
	for(n in 1:length(incs)){
		if((max.year - min.year) > thresholds[n])return(c(min.year, max.year))
		if((max.year - min.year) <= thresholds[n]){

			CalArray <- makeCalArray(calcurve, calrange = c(min.year, max.year), inc = incs[n])
			SPD <- summedCalibrator(data, CalArray)
			cum <- cumsum(SPD[,1])/sum(SPD)
			min.year <- CalArray$cal[min(which(cum>0.000001)-1)]
			max.year <- CalArray$cal[max(which(cum<0.999999)+2)]
			}
		}
return(c(min.year, max.year))}
#--------------------------------------------------------------------------------------------
SPDsimulationTest <- function(data, calcurve, calrange, pars, type, inc=5, N=20000){

	# 1. generate observed data SPD
	print('Generating SPD for observed data')
	CalArray <- makeCalArray(calcurve, calrange, inc) # makeCalArray, used for obs and each simulation
	x <- phaseCalibrator(data, CalArray, width=200, remove.external = FALSE)
	SPD.obs <- as.data.frame(rowSums(x))
	SPD.obs <- SPD.obs/(sum(SPD.obs) * CalArray$inc)
	SPD.obs <- SPD.obs[,1]

	# 2. various sample sizes and effective sample sizes

	# number of dates in the entire dataset
	n.dates.all <- nrow(data)

	# effective number of dates that contribute to the date range. Some dates may be slightly outside, giving non-integer.
	tmp <- summedCalibrator(data, CalArray, normalise = 'none')
	n.dates.effective <- round(sum(tmp)*inc,1)

	# number of phases in entire dataset
	if(!'phase'%in%names(data))data <- binner(data, width=200, calcurve)
	n.phases.all <- length(unique(data$phase))

	# effective number of phases that contribute to the date range. Some phases may be slightly outside, giving non-integer.
	n.phases.effective <- round(sum(x)*inc,1)

	# number of phases that are mostly internal to date range, used for likelihoods
	n.phases.internal <- sum(colSums(x)>=(0.5 /inc))

	# 3. convert best pars to a model
	print('Converting model parameters into a PDF')
	model <- convertPars(pars, years=CalArray$cal, type)

	# 4. Generate N simulations
	print('Generating simulated SPDs under the model')
	SPD.sims <- matrix(,length(SPD.obs),N) # blank matrix

	# how many phases to simulate, increasing slightly to account for sampling across a range 300 yrs wider 
	np <- round(sum(x)*inc * (1+300/diff(calrange)))

	# Generate simulations
	for(n in 1:N){
		cal <- simulateCalendarDates(model=model, n=np)
		age <- uncalibrateCalendarDates(cal, calcurve)
		d <- data.frame(age = age, sd = sample(data$sd, replace=T, size=length(age)), datingType = '14C')
		SPD.sims[,n] <- summedCalibrator(d, CalArray, normalise = 'full')[,1]
	
		# house-keeping
		if(n>1 & n%in%seq(0,N,length.out=11))print(paste(n,'of',N,'simulations completed'))
		}

	# 5. Construct various timeseries summaries
	
	# calBP years
	calBP <- CalArray$cal

	# expected simulation
	expected.sim <- rowMeans(SPD.sims)

	# local standard deviation
	SD <- apply(SPD.sims,1,sd)

	# CIs
	CI <- t(apply(SPD.sims,1,quantile,prob=c(0.025,0.125,0.25,0.75,0.875,0.975)))
	
	# model
	mod <- approx(x=model$year,y=model$pdf,xout=calBP,ties='ordered',rule=2)$y 
	
	# index of SPD.obs above (+1) and below(-1) the 95% CI
	upper.95 <- CI[,dimnames(CI)[[2]]=="97.5%"]
	lower.95 <- CI[,dimnames(CI)[[2]]=="2.5%"]
	index <- as.numeric(SPD.obs>=upper.95)-as.numeric(SPD.obs<=lower.95) #  -1,0,1 values

	# 6. calculate summary statistic for each sim and obs; and GOF p-value
	print('Generating summary statistics')

	# for observed
	# summary stat (SS) is simply the proportion of years outside the 95%CI
	SS.obs <- sum(SPD.obs>upper.95 | SPD.obs<lower.95) / length(SPD.obs)

	# for each simulation
	SS.sims <- numeric(N)
	for(n in 1:N){
		SPD <- SPD.sims[,n]
		SS.sims[n] <-sum(SPD>upper.95 | SPD<lower.95) / length(SPD.obs)
		}

	# calculate p-value
	pvalue <- sum(SS.sims>=SS.obs)/N

	# 7. summarise and return
	timeseries <- cbind(data.frame(calBP=calBP, expected.sim=expected.sim, local.sd=SD, model=mod, SPD=SPD.obs, index=index),CI)

return(list(timeseries=timeseries, 
	pvalue=pvalue, 
	observed.stat=SS.obs, 
	simulated.stat=SS.sims, 
	n.dates.all=n.dates.all, 
	n.dates.effective=n.dates.effective,
	n.phases.all=n.phases.all,
	n.phases.effective=n.phases.effective,
	n.phases.internal=n.phases.internal))}
#--------------------------------------------------------------------------------------------
relativeDeclineRate <- function(x, y, generation, N){

	if('numeric'%in%class(x)){
		x <- sort(x, decreasing=T)
		y <- sort(y, decreasing=T)
		X <- seq(x[1],x[2], length.out=N)
		Y <- seq(y[1],y[2], length.out=N)
		k <- exp(log(Y[2:N]/Y[1:(N-1)])/((X[1:(N-1)]-X[2:N])/25))
		res <- 100*(mean(k)-1)
		}

	if(!'numeric'%in%class(x)){
		x <- t(apply(x, MARGIN=1, FUN=sort, decreasing=T))
		y <- t(apply(y, MARGIN=1, FUN=sort, decreasing=T))
		C <- nrow(x)
		X <- Y <- matrix(,N, C)
		for(c in 1:C){
			X[,c] <- seq(x[c,1],x[c,2],length.out=N)
			Y[,c] <- seq(y[c,1],y[c,2],length.out=N)
			}
		km <- matrix(,N-1,C)
		for(c in 1:C)km[,c] <- exp(log(Y[2:N,c]/Y[1:(N-1),c])/((X[1:(N-1),c]-X[2:N,c])/25))
		res <- 100*(colMeans(km)-1)
		}
return(res)	}
#----------------------------------------------------------------------------------------------
relativeRate <- function(x, y, generation=25, N=5000){

	if('numeric'%in%class(x)){
		grad <- diff(y)/diff(x)
		if(grad==0)return(0)
		res <- relativeDeclineRate(x, y, generation, N)
		if(grad<0)res <- res*(-1)
		}

	if(!'numeric'%in%class(x)){
		grad <- apply(x, MARGIN=1, FUN=diff)/apply(y, MARGIN=1, FUN=diff)
		res <- relativeDeclineRate(x, y, generation, N)
		res[grad<0] <- res[grad<0]*(-1) 
		}
return(res)}
#----------------------------------------------------------------------------------------------
plotSimulationSummary <- function(summary, title=NULL, legend.x=NULL, legend.y=NULL){

	X <- summary$timeseries$calBP
	Y <- summary$timeseries$SPD
	ymax <- max(Y,summary$timeseries$model,summary$timeseries$'97.5%')*1.05
	P <- paste(', p =',round(summary$pvalue,3))
	if(round(summary$pvalue,3)==0)P <- ', p < 0.001'
	xticks <- seq(max(X),min(X),by=-1000)
	if(is.null(title))title <- paste('Samples N = ',round(summary$n.dates.effective),', bins N = ',round(summary$n.phases.effective),P,sep='')
	if(is.null(legend.x))legend.x <- max(X)*0.75
	if(is.null(legend.y))legend.y <- ymax*0.8

	plot(NULL,xlim=rev(range(X)),ylim=c(0,ymax), main='', xlab='calBP', ylab='PD', xaxt='n',las=1, cex.axis=0.6)
	axis(1,at=xticks,labels=paste(xticks/1000,'kyr'),cex.axis=0.7)
	text(title,x=mean(X),y=ymax*0.9,cex=1)


	polygon(x=c(X,rev(X)) ,y=c(summary$timeseries$`2.5%`,rev(summary$timeseries$`97.5%`)) ,col='grey90',border=F)
	polygon(x=c(X,rev(X)) ,y=c(summary$timeseries$`12.5%`,rev(summary$timeseries$`87.5%`)) ,col='grey70',border=F)
	polygon(x=c(X,rev(X)) ,y=c(summary$timeseries$`25%`,rev(summary$timeseries$`75%`)) ,col='grey50',border=F)

	upperpoly <- which(summary$timeseries$index != 1) 
	lowerpoly <- which(summary$timeseries$index != -1) 
	upper.y <- lower.y <- Y
	upper.y[upperpoly] <- summary$timeseries$model[upperpoly]
	lower.y[lowerpoly] <- summary$timeseries$model[lowerpoly]
	polygon(c(X,rev(X)),c(upper.y,rev(summary$timeseries$model)),border=NA, col=scales::alpha('firebrick',alpha=0.6))
	polygon(c(X,rev(X)),c(lower.y,rev(summary$timeseries$model)),border=NA, col=scales::alpha('firebrick',alpha=0.6))

	lines(x=X, y=summary$timeseries$model, col='steelblue',lty=3, lwd=2)
	lines(y=Y,x=X,lty=2)

	smooth <- round(200/mean(diff(X)))			
	Y.smooth <- zoo::rollmean(Y,smooth)
	X.smooth <- zoo::rollmean(X,smooth)
	lines(y=Y.smooth,X.smooth,lty=1,lwd=2)

	legend(legend=c('SPD (200 yrs rolling mean)','SPD','Null model','50% CI','75% CI','95% CI','Outside 95% CI'),
	x = legend.x,
	y = legend.y,
	cex = 0.7,
	bty = 'n',
	lty = c(1,2,3,NA,NA,NA,NA),
	lwd = c(2,1,2,NA,NA,NA,NA),
	col = c(1,1,'steelblue',NA,NA,NA,NA),
	fill = c(NA,NA,NA,'grey50','grey70','grey90','firebrick'), 
	border = NA,
	xjust = 1,
	x.intersp = c(1,1,1,-0.5,-0.5,-0.5,-0.5))
	}
#----------------------------------------------------------------------------------------------
