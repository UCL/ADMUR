\name{summedCalibratorWrapper} 
\alias{summedCalibratorWrapper} 
\title{Quick calibration of dates, without the need to choose a date range or generate a CalArray.} 
\description{Wrapper function that easily generates and plots an SPD, at the cost of some user control.}
\usage{summedCalibratorWrapper(data, calcurve = intcal20, plot = TRUE)}
\arguments{
  \item{data}{
	A dataframe of 14C dates. Requires 'age' and 'sd'.
	}
  \item{calcurve}{
	A calibration curve object. Choose from intcal20 (default), shcal20, intcal13 or shcal13. 
	}
  \item{plot}{
	By default (TRUE) will plot the SPD.
	}
}
\value{
	Returns a single-column data frame of SPD probabilities. Row names are the calendar years.
	}
\details{
Function to easily plot a calibrated Summed Probability Distribution from 14C dates. Automatically chooses a sensible date range and interpolation increments. Uses these to generate CalArray internally.

	}
\examples{
	# SPD of two 14C dates, calibrated through intcal20 (default)
	data <- data.frame(
		age=c(6562,7144),
		sd=c(44,51)
		)
	x <- summedCalibratorWrapper(data)

	# one date is not 14C
	data <- data.frame(
		age = c(6562,7144),
		sd = c(44,51),
		datingType = c('14C','TL')
		)
	x <- summedCalibratorWrapper(data)	}
