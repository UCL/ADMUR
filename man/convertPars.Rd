\name{convertPars} 
\alias{convertPars} 
\title{Converts parameters to x,y coordinates (date and pdf) that describe a model} 
\description{
	Converts either a vector of parameters, or a matrix of many parameter sets to model x,y coordinates (date and pdf)
	\loadmathjax }
\usage{convertPars(pars, years, type, timeseries = NULL)}
\arguments{
  \item{pars}{
	Either a vector of one parameter set, or a matrix of several parameter sets (one set per row). 
	}
  \item{years}{
	A vector of years.
	}
  \item{type}{
	Choose from  the following currently available \code{\link[=getModelChoices]{models}}. Composite models can be achieved using a vector of more than one type. For example, c('norm','power') will be a composite model, where the first two parameters are the mean and SD, the 3rd and 4th parameters determine the power distribution component, for example if modelling taphonomy.
	}
  \item{timeseries}{
	A data frame containing names x and y (date and pdf) must be provided as the timeseries, only if type is 'timeseries'.
	If 'type' is anything else, timeseries is not required (default = NULL).
	}
}
\details{
Converts model parameters into a timeseries. For example, a parameter search will yield either a single set of parameters, or a matrix with one parameter set per row (such as the 'res' value from \code{\link[=mcmc]{mcmc}}). Either can be handed directly to this function.
The structure of the output differs depending on if converting a vector or matrix.
	}
	
\examples{
	#  a random 6-CPL parameter set
	pars <- runif(11)
	x <- convertPars( pars=pars, years=5500:7500, type='CPL')

	#  a matrix of 5 random 6-CPL parameter sets
	pars <- matrix( runif(11*5), 5, 11 )
	x <- convertPars( pars=pars, years=5500:7500, type='CPL')

	#  a random exponential parameter
	pars <- runif(1, -0.01, 0.01)
	x <- convertPars( pars=pars, years=5500:7500, type='exp')

	#  a matrix of 5 random exponential parameter sets
	pars <- matrix( runif(5, -0.01, 0.01), 5, 1 )
	x <- convertPars( pars=pars, years=5500:7500, type='exp')

	#  a random Gaussian parameter pair (mean, sd)
	pars <- runif(2, c(6000,200), c(7000,1000))
	x <- convertPars( pars=pars, years=5500:7500, type='norm')

	# a combination model of a Gaussian (parameters = mean, sd), 
	# and a power model assumed to be a taphonomic effect (parameters = b,c).
	pars <- runif(4, c(6000,200,0,-3), c(7000,1000,20000,0))
	x <- convertPars( pars=pars, years=5500:7500, type=c('norm','power'))

	# 5 combination models of a Gaussian (parameters = mean, sd),
	# and a power model assumed to be a taphonomic effect (parameters = b,c).
	pars <- t(matrix(runif(4*5, c(6000,200,0,-3), c(7000,1000,20000,0)),4,5))
	x <- convertPars( pars=pars, years=5500:7500, type=c('norm','power'))

	# a single random Cauchy parameter pair (location, scale)
	pars <- runif(2, c(6000,200), c(7000,1000))
	x <- convertPars( pars=pars, years=5500:7500, type='cauchy')

	# a combination model of a Cauchy (parameters = location, scale),
	# and a power model assumed to be a taphonomic effect (parameters = b,c).
	pars <- runif(4, c(6000,200,0,-3), c(7000,1000,20000,0))
	x <- convertPars( pars=pars, years=5500:7500, type=c('norm','power'))

	# a single random logistic parameter pair (k, x0)
	pars <- runif(2, c(0,6000), c(0.01,6500))
	x <- convertPars( pars=pars, years=5500:7500, type='logistic')

	# a combination model of a logistic (parameters = k, x0),
	# and a power model assumed to be a taphonomic effect (parameters = b,c).
	pars <- runif(4, c(0,6000,0,-3), c(0.01,6500,20000,0))
	x <- convertPars( pars=pars, years=5500:7500, type=c('logistic', 'power'))

	# a single random sinewave parameter set (f,p,r)
	f <- 1/runif(1,200,1000)
	p <- runif(1,0,2*pi)
	r <- runif(1,0,1)
	x <- convertPars( pars=c(f,p,r), years=5500:7500, type='sine')

	# a combination model of a sinewave (parameters = f,p,r),
	# and a power model assumed to be a taphonomic effect (parameters = b,c).

	f <- 1/runif(1,200,1000)
	p <- runif(1,0,2*pi)
	r <- runif(1,0,1)
	b <- runif(1,0,20000)
	c <- runif(1,-3,0)
	x <- convertPars( pars=c(f,p,r,b,c), years=5500:7500, type=c('sine','power'))
	
	# although a uniform distribution has no parameters, a pdf can still be generated:
	x <- convertPars(pars=NULL, years=5500:7500, type='uniform')

	# and a uniform combined with a power model assumed to be a taphonomic effect (parameters = b,c)
	pars <- runif(2, c(0,-3), c(20000,0))
	x <- convertPars(pars=pars, years=5500:7500, type=c('uniform','power'))
	}
