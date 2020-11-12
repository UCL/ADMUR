\name{convertPars} 
\alias{convertPars} 
\title{Converts parameters to x,y coordinates (date and pdf) that describe a model} 
\description{Converts either a vector of a single parameter set, or a matrix set of parameters to model x,y coordinates (date and pdf)}
\usage{convertPars(pars, years, type)}
\arguments{
  \item{pars}{
	Either a single vector of one parameter combination, or a matrix of several parameter combinations (one set per row). 
	}
  \item{years}{
	A vector of years.
	}
  \item{type}{
	Choose 'CPL' for a Continuous Piecewise Linear model, 'exp' for an exponential model, or 'uniform' for a uniform model.
	}
}
\details{
Parameter searches will yield either a single vector or parameters or a matrix of many parameters (such as the 'res' value from \code{\link[=mcmc]{mcmc}}) . Either can be handed directly to this function.

If type is 'CPL' a vector comprising a parameter set with an odd length (each between 0 and 1) must be provided, or a matrix of parameters (one parameter set per row) with an odd number of columns. An Area Breaking Process (a modified Stick Breaking Dirichlet Process) is then used to map parameters to their corresponding PD coordinates.  This uses the Beta distribution CDF (with alpha = 1 and beta = the number of pieces still to be broken) to convert an x-parameter into its equivalent x-coordinate value, and the Gamma CDF (with alpha = 1 and beta = 1) to convert a y-parameter into its equivalent coordinate value between 0 and +infinity.

If type is 'exp', a single parameter must be provided (-ve or +ve to allow for growth or decay), typically between -0.01 and 0.01 to keep the magnitude of computed values within floating point limits. Alternatively a single column matrix of parameters can be provided. This function uses the single parameter as the rate exponent r in the formula e^rt, where t = years. Note the parameter A in more familiar formula Ae^rt is not required since this is an exponential model PDF (zero outside the date range, total area equals 1).

If type is 'uniform', then no parameters are required, and pars must be NULL or c(). 

Structure of the output differs depending on if converting a single vector or a matrix.
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
	
	# although a uniform distribution has no parameters, a pdf can still be generated:
	x <- convertPars(pars=NULL, years=5500:7500, type='uniform')
	}
