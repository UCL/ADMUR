\name{makeCalArray} 
\alias{makeCalArray} 
\title{Makes a calibration curve probability array} 
\description{Generates CalArray containing a 2D probability array of the calibration curve. }
\usage{makeCalArray(calcurve, calrange, inc=5)}
\arguments{
  \item{calcurve}{
	A calibration curve object. Choose from intcal20 (default), shcal20, intcal13 or shcal13.
	}
  \item{calrange}{
	A vector of two calendar dates BP, giving the calendar range of CalArray. Can be in either order.
	}
  \item{inc}{
	Increments to interpolate calendar years. Default = 5
	}
}
\value{
	Returns a list including:
	\item{probs}{a 2D probability array of the calibration curve}
	\item{calcurve}{the calibration curve as provided as an argument}
	\item{cal}{a numeric vector of calendar years}
	\item{inc}{the resolution of the array in calendar years}
	}
\details{
	Generates an array of probabilities mapping the calibration curve and its error ribbon.

	Each column represents a Gaussian PDF constructed from the C14 date and error of the calibration curve (typically a column every 5 cal years).

	Row names of CalArray are 14C dates, column names are cal dates.

	This function is memory and time costly if used to construct the entire 50,000 year range of the calibration curve at a resolution of 1 cal years, therefore typically used only for a constrained date range, by specifying calrange.

	This array only needs constructing once to generate a Summed Probability Distribution of any number of calibrated dates, allowing efficient downstream calibration.
	}
\examples{
	# generate a CalArray of the intcal20 curve covering 5500 calBP to 7000 calBP
	x <- makeCalArray(intcal13, c(5500,7000), inc=1 )
	}
