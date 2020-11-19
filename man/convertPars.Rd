\name{convertPars} 
\alias{convertPars} 
\title{Converts parameters to x,y coordinates (date and pdf) that describe a model} 
\description{
	Converts either a vector of a single parameter set, or a matrix set of parameters to model x,y coordinates (date and pdf)
	\loadmathjax }
\usage{convertPars(pars, years, type)}
\arguments{
  \item{pars}{
	Either a single vector of one parameter combination, or a matrix of several parameter combinations (one set per row). 
	}
  \item{years}{
	A vector of years.
	}
  \item{type}{
	Choose 'CPL' for a Continuous Piecewise Linear model, 'exp' for an exponential model, 'norm' for a Gaussian (normal) model, 'cauchy' for a cauchy model, 'sine' for a sinusoidal model or 'uniform' for a uniform model.
	}
}
\details{
Parameter searches will yield either a single vector or parameters or a matrix of many parameters (such as the 'res' value from \code{\link[=mcmc]{mcmc}}) . Either can be handed directly to this function.

Structure of the output differs depending on if converting pars that are a single vector or a matrix of many pars.

If type is 'CPL' a vector comprising a parameter set with an odd length (each between 0 and 1) must be provided, or a matrix of parameters (one parameter set per row) with an odd number of columns. An Area Breaking Process (a modified Stick Breaking Dirichlet Process) is then used to map parameters to their corresponding PD coordinates.  This uses the Beta distribution CDF (with \mjseqn{\alpha = 1} and \mjseqn{\beta =} the number of pieces still to be broken) to convert an x-parameter into its equivalent x-coordinate value, and the Gamma CDF (with \mjseqn{\alpha = 1} and \mjseqn{\beta = 1}) to convert a y-parameter into its equivalent coordinate value between 0 and \mjseqn{\infty}.

If type is 'exp', a single parameter must be provided, or a single column matrix of parameters. Each single parameter is used as the rate exponent \mjseqn{r} where the PDF \mjsdeqn{f(x) = \frac{re^{r x}}{e^{rx_{max}} - e^{rx_{min}}}}  where \mjseqn{x =} years, and \mjseqn{x_{min}} and \mjseqn{x_{max}} determine the date range.

If type is 'norm', then two parameters (\mjseqn{\mu} and \mjseqn{\sigma} respectively) are used in the formula for a truncated Normal distribution, such that the total area between \mjseqn{x_{min}} and \mjseqn{x_{max}} (the date range) equals 1.

If type is 'uniform', then no parameters are required, pars must be NULL or c(), and trivially the PDF \mjseqn{f(x) = \frac{1}{x_{max} - x_{min}}} where \mjseqn{x =} years, and \mjseqn{x_{min}} and \mjseqn{x_{max}} determine the date range.

If type is 'cauchy', then two parameters (location and scale respectively) are used in the formula for a truncated cauchy distribution, such that the total area between \mjseqn{x_{min}} and \mjseqn{x_{max}} (the date range) equals 1.

IF type is 'sine', then three parameters are used as specified in \code{\link[=sinewavePDF]{sinewavePDF}}
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
