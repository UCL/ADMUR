\name{simulateCalendarDates} 
\alias{simulateCalendarDates} 
\title{Converts calendar dates to 14C dates} 
\description{Randomly samples calendar dates from a model, including dates slightly outside the model date range to avoid edge effects}
\usage{simulateCalendarDates(model, n)}
\arguments{
  \item{model}{
	A data frame including columns 'year' and 'pdf' 
	}
  \item{n}{
	The number of dates to sample.
	}
}
\value{
	Returns a vector of calendar dates
	}
\details{
Samples n random calendar dates from a model pdf. This model must be defined in terms of a PDF vector and the corresponding calendar years. This can be provided at any preferred temporal resolution. For example, an exponential model can be provided with the PDF in annual intervals, whilst CPL model needs only the hinge points. \code{\link[=convertPars]{convertPars}} will convert parameters into the required model format.
	}
\examples{
	# under a uniform model
	model <- convertPars(pars=NA, years=5000:6000,type='uniform')
	sims <- simulateCalendarDates(model, 1000)
	range(sims)
	
	# simulate under an exponential model
	model <- convertPars(pars=0.001, years=5000:6000,type='exp')
	sims <- simulateCalendarDates(model, 1000)
	range(sims)
	
	# under a CPL model
	model <- convertPars(pars=runif(5), years=5000:6000,type='CPL')
	sims <- simulateCalendarDates(model, 1000)
	range(sims)
	}
