\name{uncalibrateCalendarDates} 
\alias{uncalibrateCalendarDates} 
\title{Converts calendar dates to 14C dates} 
\description{Randomly samples a 14C date from the calibration curve error ribbon, at the corresponding calendar date}
\usage{uncalibrateCalendarDates(dates, calcurve)}
\arguments{
  \item{dates}{
	A vector of calendar dates. 
	}
  \item{calcurve}{
	A calibration curve object. Choose from intcal20 (default), shcal20, intcal13 or shcal13.
	}
}
\value{
	Returns a vector of 14C dates
	}
\details{
Conceptually this can be thought of as the reverse process of calibrating a 14C date into calendar time, however 'uncalibrating' is a misnomer as the full calibrated PD is not used. Instead, it uses a vector of calendar point estimates, and randomly samples 14C dates from the calibration curve error ribbon, at the corresponding calendar dates. Therefore values will differ each time. 
	}
\examples{
	uncalibrateCalendarDates(c(4500,5000), shcal20)

	# note the date outside the calcurve range is unchanged
	uncalibrateCalendarDates(c(4500,500000), intcal20)
	}
