\name{simulateCalendarDates} 
\alias{simulateCalendarDates} 
\title{Converts calendar dates to 14C dates} 
\description{Randomly samples calendar dates from a model, including dates slightly outside the model date range to avoid edge effects.}
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
	uncalibrateCalendarDates(c(4500,5000), shcal20)

	# note the date outside the calcurve range has a 1 to 1 mapping between cal and c14 time
	uncalibrateCalendarDates(c(4500,70000), intcal20)

	# however, a soft fade is performed between the end of the calcurve and 60000
	uncalibrateCalendarDates(c(4500,58000), intcal20)
	}
