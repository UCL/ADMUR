\name{sinewavePDF} 
\alias{sinewavePDF} 
\title{PDF of a sinusoidal curve that is always positive.} 
\description{Probability density function for a sinusoidal curve constrained to have minima greater or equal to zero.}
\usage{sinewavePDF(x, min, max, f, p, r)}
\arguments{
	\item{x}{
	Numeric vector
	}
	\item{min, max}{
	Lower and upper x limits of the distribution
	}
	\item{f}{
	Numeric, frequency (cycles per unit x).
	}
	\item{p}{
	Numeric between 0 and 2 pi, giving the cycle position (in radians) at x = 0.
	}
	\item{r}{
	Numeric between 0 and 1, determining distance between x-axis and sine wave minima.
	}
}
\details{
	The usual function to describe a sine wave is f(x) = A * sin(2*pi*f*x + p), where:
	- A is amplitude;
	- p is the cycle position (in radians) at x = 0, and therefore must be between 0 and 2 pi
	- f is the frequency (cycles per year).
	However, we are describing a sine wave that is never negative (minimum f(x) is >=0), which can conceptually be considered as a sine wave plus a uniform distribution with a height A + k, where k >=0.
	Since the pdf is f(x) divided by the area below the curve, A and k can be simplified to a single parameter r that determines the relative proportions of the uniform and sinusoidal components, such that:
	- when r = 0 the amplitude of the sine wave component is zero, and the curve is uniform.
	- when r = 1 the uniform component is zero, and the minima of the sine wave touches zero.
	}
\examples{
	# A sinewave with a period of 700 years
	x <- seq(1500,4500, length.out=1000)
	y <- sinewavePDF(x, min=2000, max=4000, f=1/700, p=runif(1,0,2*pi), r=runif(1))
	}