\name{SPDsimulationTest} 
\alias{SPDsimulationTest} 
\title{Goodness of Fit test, using SPD simulation} 
\description{Calculates the data p-value given a model}
\usage{SPDsimulationTest(data, calcurve, calrange, pars, type, inc=5, N=20000, timeseries = NULL)}
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
	Choose from 'CPL', 'exp', 'norm', 'cauchy', 'sine', 'uniform', 'logistic', 'power', 'timeseries'.
	}
\item{inc}{
	Increments to interpolate calendar years. Default = 5
	}
\item{N}{
	The number of simulations to generate.
	}
\item{timeseries}{
	Only if 'type' = 'timeseries', a data frame should be provided containing columns x and y that define the timeseries as year and value respectively.
	}
}
\value{
	Returns a list. See details.
	}
\details{
	The returned list provides various summary statistics and timeseries of the observed and simulated data:
	
	timeseries: a data frame containing various CIs and: 
	
	calBP: a vector of calendar years BP. 
	
	expected.sim: a vector of the expected simulation (mean average of all N simulations). 
	
	local.sd: a vector of the local (for each year) standard deviation of all N simulations.
	
	model: a vector of the model PDF.
	
	SPD: a vector of the observed SPD PDF, generated from data.
	
	index: a vector of -1,0,+1 corresponding to the SPD points that are above, within or below the 95\% CI of all N simulations.
	
	pvalue: the proportion of N simulated SPDs that have more points outside the 95\% CI than the observed SPD has.
	
	observed.stat: the summary statistic for the observed data (number of points outside the 95\% CI).
	
	simulated.stat: a vector of summary statistics (number of points outside the 95\% CI), one for each simulated SPD.
	
	n.dates.all: the total number of dates in the whole data set. Trivially, the number of rows in data.
	
	n.dates.effective: the effective number of dates within the date range. Will be non-integer since a proportion of some dates will be outside the date range.
	
	n.phases.all: the total number of phases in the whole data set. 
	
	n.phases.effective: the effective number of phases within the date range. Will be non-integer since a proportion of some phases will be outside the date range.
	
	n.phases.internal: an integer subset of n.phases.all that have more than 50\% of their total probability mass within the date range.

	The default N = 20000 can be increased if greater precision is required, however this can be very time costly.
	}
\examples{
\donttest{
# trivial example showing a single date can never be rejected under a uniform model:
data <- data.frame(age=6500, sd=50, phase=1, datingType='14C')
x <- SPDsimulationTest(data, 
	calcurve=intcal20, 
	calrange=c(6000,9000), 
	pars=NULL, 
	type='uniform')
print(x$pvalue)
}
}
