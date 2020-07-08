\name{makeCalArray} 
\alias{makeCalArray} 
\title{Makes a calibration curve probability array} 
\description{Generates CalArray, a 2D probability array of the calibration curve. }
\usage{makeCalArray(calcurve, calrange, inc=5)}
\arguments{
  \item{calcurve}{
	A calibration curve object. Choose from intcal13 or shcal13.
	}
  \item{calrange}{
	A vector of two cal dates, giving the calendar range of CalArray. Can be in either order.
	}
}
\value{
	Returns a 2D array. 
	}
\details{
	Generates an array of probabilities mapping the calibration curve and its error ribbon.

	Each column represents a Gaussian distribution constructed from the C14 date and error of the calibration curve assigned to each Cal year (typically a column every 5 cal years).

	Row names of CalArray are C14 dates, column names are cal dates.

	This function is memory and time costly if used to construct the entire 50,000 year range of the calibration curve at a resolution of 5 cal years, therefore typically used only for a constrained date range, by specifying calrange.

	This array only needs constructing once to generate a Summed Probability Distribution of any number of calibrated dates, allowing very efficient downstream calibration.
	}
\examples{
	# generate a CalArray of the intcal13 curve covering 5500 calBP to 7000 calBP
	x <- makeCalArray(intcal13, c(5500,7000) )

	# generate a CalArray of the shcal13 curve covering 5500 calBP to 7000 calBP
	x <- makeCalArray(shcal13, c(7000,5500) )
	}
