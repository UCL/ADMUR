\name{convertPars} 
\alias{convertPars} 
\title{Converts parameters to x,y coordinates (date and pdf) that describe a model} 
\description{
	Converts either a vector of parameters, or a matrix of many parameter sets to model x,y coordinates (date and pdf)
	\loadmathjax }
\usage{convertPars(pars, years, type)}
\arguments{
  \item{pars}{
	Either a vector of one parameter set, or a matrix of several parameter sets (one set per row). 
	}
  \item{years}{
	A vector of years.
	}
  \item{type}{
	Choose from 'CPL', 'exp', 'norm', 'cauchy', 'sine', 'uniform', 'logistic', 'power'.
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

'cauchy' is a truncated Cauchy model. The two parameters are used as \mjseqn{x_0} (location) and \mjseqn{\gamma} (scale) in the formula for a truncated Cauchy distribution. The PDF is as follows where \mjseqn{x =} years: \mjsdeqn{\frac{x_0}{\gamma[1+(x\frac{x_0}{\gamma})^2][\arctan(x_{max}\frac{x_0}{\gamma})-\arctan(x_{min}\frac{x_0}{\gamma})]}}  

'power' is a truncated Power law model of the form \mjseqn{f(x)=a(b+x)^c} where \mjseqn{x =} years. The PDF is as follows. Note the \mjseqn{a} parameter cancels out: \mjsdeqn{\frac{(c+1)(b+x)^c}{(b+x_{max})^{(c+1)} - (b+x_{min})^{(c+1)}}}

'sine' is a truncated sinewave model. The three parameters are used as specified in \code{\link[=sinewavePDF]{sinewavePDF}}

'uniform' is a uniform model requiring no parameters. I.e. the argument pars must be NULL or c(), and trivially the PDF is: \mjsdeqn{\frac{1}{x_{max} - x_{min}}}

	}
\examples{
	# convert a single random 6-CPL parameter set
	x <- convertPars( pars=runif(11), years=5500:7500, type='CPL')

	# convert a matrix of 5 random 6-CPL parameter sets
	pars <- matrix( runif(11*5), 5, 11 )
	x <- convertPars( pars=pars, years=5500:7500, type='CPL')

	# convert a single random exponential parameter
	x <- convertPars( pars=runif(1, -0.01, 0.01), years=5500:7500, type='exp')
	
	# convert a matrix of 5 random exponential parameter sets
	pars <- matrix( runif(5, -0.01, 0.01), 5, 1 )
	x <- convertPars( pars=pars, years=5500:7500, type='exp')

	# convert a single random Gaussian parameter pair (mean, sd)
	x <- convertPars( pars=runif(2)*10000, years=5500:7500, type='norm')

	# convert a matrix of 5 random Gaussian parameter pairs (mean, sd)
	pars <- matrix( runif(10)*10000, 5, 2 )
	x <- convertPars( pars=pars, years=5500:7500, type='norm')

	# convert a single random Cauchy parameter pair (location, scale)
	x <- convertPars( pars=runif(2)*10000, years=5500:7500, type='cauchy')

	# convert a matrix of 5 random Cauchy parameter pairs (location, scale)
	pars <- matrix( runif(10)*10000, 5, 2 )
	x <- convertPars( pars=pars, years=5500:7500, type='cauchy')

	# convert a single random logistic parameter pair (k, x0)
	pars <- c(runif(1,0,0.01),runif(1,6000,6500))
	x <- convertPars( pars=pars, years=5500:7500, type='logistic')

	# convert a matrix of 5 random logistic parameter pairs(k, x0)
	pars <- matrix( c(runif(5,0,0.01),runif(5,6000,6500)), 5, 2 )
	x <- convertPars( pars=pars, years=5500:7500, type='logistic')

	# convert a single random power law parameter pair (b, c)
	pars <- c(runif(1,2000,4000),runif(1,-1.2,-1.7))
	x <- convertPars( pars=pars, years=5500:7500, type='power')

	# convert a matrix of 5 random power law parameter pairs(b, c)
	pars <- matrix( c(runif(5,2000,4000),runif(5,-1.2,-1.7)), 5, 2 )
	x <- convertPars( pars=pars, years=5500:7500, type='power')

	# convert a single random sinewave parameter set (f,p,r)
	f <- 1/runif(1,200,1000)
	p <- runif(1,0,2*pi)
	r <- runif(1,0,1)
	x <- convertPars( pars=c(f,p,r), years=5500:7500, type='sine')

	# convert a matrix of 5 random sinewave parameter sets (f,p,r)
	f <- 1/runif(5,200,1000)
	p <- runif(5,0,2*pi)
	r <- runif(5,0,1)
	x <- convertPars( pars=cbind(f,p,r), years=5500:7500, type='sine')
	
	# although a uniform distribution has no parameters, a pdf can still be generated:
	x <- convertPars(pars=NULL, years=5500:7500, type='uniform')
	}
