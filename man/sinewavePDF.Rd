\name{sinewavePDF} 
\alias{sinewavePDF} 
\title{PDF of a sinusoidal curve that is always positive} 
\description{Probability density function for a sinusoidal curve that is entirely above the x-axis.}
\usage{sinewavePDF(x, min, max, f, p, r)}
\arguments{
	\item{x}{
	Numeric vector
	}
	\item{min, max}{
	Lower and upper \eqn{x} limits of the distribution
	}
	\item{f}{
	Numeric frequency (cycles per unit \eqn{x}).
	}
	\item{p}{
	Numeric between \eqn{0} and \eqn{2\pi}, giving the cycle position (in radians) at \eqn{x = 0}.
	}
	\item{r}{
	Numeric between 0 and 1, determining distance between x-axis and sine wave minima.
	}
}
\details{
	The usual function to describe a sine wave is \eqn{f(x) = A sin(2\pifx + p)}, where:

	\eqn{A} is the amplitude;

	\eqn{f} is the frequency (cycles per year);

	\eqn{p} is the cycle position (in radians) at \eqn{x = 0}, and therefore must be between 0 and \eqn{2\pi}.

	However, we are describing a sine wave that is never negative (minimum \eqn{f(x)} is \eqn{>= 0}), which can conceptually be considered as a sine wave stacked on top of a uniform distribution with a height \eqn{A + k}, where \eqn{k >= 0}.
	Since the pdf is \eqn{f(x)} divided by the area below the curve, A and k can be simplified to a single parameter r that determines the relative proportions of the uniform and sinusoidal components, such that:

	when \eqn{r = 0} the amplitude of the sine wave component is zero, and the curve is uniform.

	when \eqn{r = 1} the uniform component is zero, and the minima of the sine wave touches zero.

	Therefore the formula for the pdf is:

	\eqn{pdf(x) = ( sin(2\pifx + p) + 1 - log(a) ) / ( (max - min)(1 - log(a)) + (1/2\pif)( cos(2\pifmin - p) - cos(2\pifmax - p) ) )}

	}
\examples{
	# A sinewave with a period of 700 years
	x <- seq(1500,4500, length.out=1000)
	y <- sinewavePDF(x, min=2000, max=4000, f=1/700, p=0, r=0.2)
	plot(x,y,type='l')
	}
