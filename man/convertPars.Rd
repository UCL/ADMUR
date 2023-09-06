\name{convertPars} 
###
pars, years, type, timeseries=NULL
###
\alias{convertPars} 
\title{Converts parameters to x,y coordinates (date and pdf) that describe a model} 
\description{
	Converts either a vector of parameters, or a matrix of many parameter sets to model x,y coordinates (date and pdf)
	\loadmathjax }
\usage{convertPars(pars, years, type, taphonomy=FALSE)}
\arguments{
  \item{pars}{
	Either a vector of one parameter set, or a matrix of several parameter sets (one set per row). 
	}
  \item{years}{
	A vector of years.
	}
  \item{type}{
	Choose from 'CPL', 'exp', 'norm', 'cauchy', 'sine', 'uniform', 'logistic', 'power', 'timeseries'.
	}
  \item{timeseries}{
	If type is 'timeseries', a data frame containing names x and y must be provided as the timeseries, where x is time and y is the density.
	}
}
\details{
Parameter searches will yield either a single set of parameters, or a matrix with one parameter set per row (such as the 'res' value from \code{\link[=mcmc]{mcmc}}) . Either can be handed directly to this function.

The structure of the output differs depending on if converting a vector or matrix.

All models are truncated, such that the total area between \mjseqn{x_{min}} and \mjseqn{x_{max}} (the date range of 'years') equals 1.  

'CPL' is a Continuous Piecewise Linear model. Pars must be of odd length, each between 0 and 1. A n-CPL model has \mjseqn{2n-1} parameters (\mjseqn{n-1} x-parameters and \mjseqn{n} y-parameters) that are mapped to \mjseqn{n+1} PD coordinates (x,y pairs) using a modified Stick Breaking Dirichlet Process. The first and last x-coordinate are set as \mjseqn{x_{min}} and \mjseqn{x_{max}}, and the remaining internal x-coordinates are converted from their respective x-parameters using the Beta distribution CDF (where \mjseqn{\alpha = 1} and \mjseqn{\beta =} the number of pieces still to be broken). The y-parameters (between 0 and 1) are converted to y-coordinates in two steps. Firstly, they are mapped from the parameter range (0,1) to the coordinate range (0, \mjseqn{\infty}) using the formula \mjseqn{\frac{1}{(1 - y)^2} - 1}, and the single remaining y-coordinate is set as \mjseqn{\frac{1}{(1 - 0.5)^2} - 1}. Secondly, they are normalised by the total area under the curve, calculated as the sum of the areas under all \mjseqn{n} pieces: \mjsdeqn{Area = \sum_{i=1}^{n} (\frac{y_i + y_{i+1}}{2})(x_{i+1} - x_i) }

'exp' is a truncated exponential model of the form \mjseqn{f(x)=ae^{rx}} where \mjseqn{x =} years. The single parameter is used as the rate exponent \mjseqn{r} which gives growth through time if \mjseqn{r > 0}, decline if \mjseqn{r < 0}, and constant if \mjseqn{r=0}. The PDF is as follows. Note the \mjseqn{a} parameter cancels out: \mjsdeqn{\frac{-re^{-r x}}{e^{-rx_{max}} - e^{-rx_{min}}}}  

'logistic' is a truncated logistic model. The two parameters are used as the rate \mjseqn{k} and centre \mjseqn{x_0} where the PDF is: \mjsdeqn{\frac{k}{(e^{-k(x_0 - x)} + 1)\ln{\Bigl(\frac{e^{-k(x_0 - x_{min})} + 1}{e^{-k(x_0 - x_{max})} + 1}}\Bigr)}}

'norm' is a truncated Gaussian model. The two parameters are used as \mjseqn{\mu} and \mjseqn{\sigma} in the formula for a truncated Normal distribution, the PDF of which is calculated in two steps. Firstly, the PDF of an ordinary Normal distribution is calculated. Secondly, it is normalised by the area within the date range.

'cauchy' is a truncated Cauchy model. The two parameters are used as \mjseqn{x_0} (location) and \mjseqn{\gamma} (scale) in the formula for a truncated Cauchy distribution. The PDF is as follows where \mjseqn{x =} years: \mjsdeqn{\frac{1}{\gamma[1+(\frac{x-x_0}{\gamma})^2][\arctan(\frac{x_0-x_{min}}{\gamma})-\arctan(\frac{x_0-x_{max}}{\gamma})]}}  

'power' is a truncated Power function model of the form \mjseqn{f(x)=a(b+x)^c} where \mjseqn{x =} years.  The PDF is as follows. Note the \mjseqn{a} parameter cancels out: \mjsdeqn{\frac{(c+1)(b+x)^c}{(b+x_{max})^{(c+1)} - (b+x_{min})^{(c+1)}}}

'sine' is a truncated sinewave model. The three parameters are used as specified in \code{\link[=sinewavePDF]{sinewavePDF}}

'uniform' is a uniform model requiring no parameters. I.e. the argument pars must be NULL or c(), and trivially the PDF is: \mjsdeqn{\frac{1}{x_{max} - x_{min}}}

'timeseries' is a custom model of any complexity, typically derived from an independent source. For example, a timeseries of the proportion of forest to grassland through time, derived from pollen data. This requires a single parameter \mjseqn{r} to scale PDF.
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

	# a combination model of a Gaussian (parameters = mean, sd) and a power model assumed to be a taphonomic effect (parameters = b,c).
	pars <- runif(4, c(6000,200,0,-3), c(7000,1000,20000,0))
	x <- convertPars( pars=pars, years=5500:7500, type=c('norm','power'))

	# 5 combination models of a Gaussian (parameters = mean, sd) and a power model assumed to be a taphonomic effect (parameters = b,c).
	pars <- t(matrix(runif(4*5, c(6000,200,0,-3), c(7000,1000,20000,0)),4,5))
	x <- convertPars( pars=pars, years=5500:7500, type=c('norm','power'))

	# a single random Cauchy parameter pair (location, scale)
	pars <- runif(2, c(6000,200), c(7000,1000))
	x <- convertPars( pars=pars, years=5500:7500, type='cauchy')

	# a combination model of a Cauchy (parameters = location, scale) and a power model assumed to be a taphonomic effect (parameters = b,c).
	pars <- runif(4, c(6000,200,0,-3), c(7000,1000,20000,0))
	x <- convertPars( pars=pars, years=5500:7500, type=c('norm','power'))

	# a single random logistic parameter pair (k, x0)
	pars <- runif(2, c(0,6000), c(0.01,6500))
	x <- convertPars( pars=pars, years=5500:7500, type='logistic')

	# a combination model of a logistic (parameters = k, x0) and a power model assumed to be a taphonomic effect (parameters = b,c).
	pars <- runif(4, c(0,6000,0,-3), c(0.01,6500,20000,0))
	x <- convertPars( pars=pars, years=5500:7500, type=c('logistic', 'power'))

	# a single random sinewave parameter set (f,p,r)
	f <- 1/runif(1,200,1000)
	p <- runif(1,0,2*pi)
	r <- runif(1,0,1)
	x <- convertPars( pars=c(f,p,r), years=5500:7500, type='sine')

	# a combination model of a sinewave (parameters = f,p,r) and a power model assumed to be a taphonomic effect (parameters = b,c).

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
