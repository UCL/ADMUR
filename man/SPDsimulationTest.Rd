\name{SPDsimulationTest} 
\alias{SPDsimulationTest} 
\title{Goodness of Fit test, using SPD simulation} 
\description{Calculates the data p-value given a model.}
\usage{SPDsimulationTest(data, calcurve, calrange, pars, type, S=1000)}
\arguments{
  \item{data}{
	A dataframe of 14C dates. Requires 'age' and 'sd', and at least one of 'site' and 'phase'. Optional 'datingType' comprising '14C' and/or anything else.
	}
\item{calcurve}{
	A calibration curve object. Choose from intcal20 (default), shcal20, intcal13 or shcal13.
	}
\item{calrange}{
	A vector of two calendar dates BP, giving the calendar range of CalArray. Can be in either order.
	}
\item{pars}{
	A single vector of one parameter combination. 
	}
\item{type}{
	Choose 'CPL' for a Continuous Piecewise Linear model, 'exp' for an exponential model, or 'uniform' for a uniform model.
	}
\item{S}{
	The number of simulations to generate.
	}
}
\value{
	A single numeric p value
	}
\details{
	The GOF calculates the model likelihood under the provided (observed) data set. It then generates S similar (same effective sample size) simulated datasets and calculates the model likelihood under each. A 1-tailed test is then performed, such that the proportion of simulated data sets under which the model has a lower (or equal) likelihood as under the observed data set is calculated.

	Therefore the p-value generated represents the probability of the observed data or data more extreme  using the definition that a lower likelihood means more extreme.
	
	The default S = 1000 can be increased if greater precision is required, however this can be very time costly.
	}
\examples{
	# the trivial example of showing that a single date cannot be rejected under a uniform model:
	data <- data.frame(age=6500, sd=50, phase=1, datingType='14C')
	GOF(data, calcurve=intcal20, calrange=c(6000,9000), pars=NULL, type='uniform')
	}
