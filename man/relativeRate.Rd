\name{relativeRate} 
\alias{relativeRate} 
\title{Calculates the relative growth (or decline) rate per generation.} 
\description{Calculates the generational growth/decline rate for a linear piece of a CPL model, or between any two x,y coordinate pairs.}
\usage{relativeRate(x, y, generation = 25, N = 5000)}
\arguments{
  \item{x}{
	A numeric vector of length 2, giving the start and end date of linear piece, or a 2 column matrix such that each row is a start and end pair.
	}
  \item{y}{
	The corresponding y values such as PDs, or population size (numeric vector of length 2), or a matrix such that each row is a start and end pair. 
	}
  \item{generation}{
	Years per generation. Default = 25.
	}
  \item{N. Default = 5000}{
	Number of sections to average the growth rate across.. 
	}	
}
\value{
	Returns a numeric vector of values between 0 and 100 representing a 'relative percentage rate per generation'. Negative values indicate a decline through time, positive indicate growth.
	}

\details{
	The 'relative rate' (growth or decline) of a straight line between two x,y coordinate pairs is the expected generational growth rate across this line. It is calculated always relative to the larger y-value, providing a symmetric measure. E.g., the absolute percentage changes from 80 to 100 to 80 are calculated as +20% and -20%, rather than the conventional +25% and -20%.
	The expected rate is the mean average of the conventional rates for N equal sections of the line, as N approaches infinity. 
	
	}
\examples{

	x <- c(5600,5500)
	y <- c(80,75)

	# conventional growth/decline rate per 25 yr generation
	100 * (1 - exp(log(y[2]/y[1])/((x[2]-x[1])/25)))

	# relative growth/decline rate per 25 yr generation
	relative.rate(x,y)

	x <- c(5600,5500)
	y <- c(480,75)

	# conventional growth/decline rate per 25 yr generation
	100 * (1 - exp(log(y[2]/y[1])/((x[2]-x[1])/25)))

	# relative growth/decline rate per 25 yr generation
	relative.rate(x,y)

	x <- c(5600,5500)
	y <- c(480,0)

	# conventional growth/decline rate per 25 yr generation
	100 * (1 - exp(log(y[2]/y[1])/((x[2]-x[1])/25)))

	# relative growth/decline rate per 25 yr generation
	relative.rate(x,y)
	}
