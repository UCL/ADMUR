\name{sinewavePDF} 
\alias{sinewavePDF} 
\title{PDF of a truncated sinusoidal curve} 
\description{
		Probability density function for a truncated sinusoidal curve.
		\loadmathjax }
\usage{sinewavePDF(x, min, max, f, p, r)}
\arguments{
	\item{x}{
	Numeric vector
	}
	\item{min, max}{
	Lower and upper \mjseqn{x} limits of the distribution
	}
	\item{f}{
	Numeric frequency (cycles per unit \mjseqn{x}).
	}
	\item{p}{
	Numeric between \mjseqn{0} and \mjseqn{2\pi}, giving the cycle position (in radians) at \mjseqn{x = 0}.
	}
	\item{r}{
	Numeric between 0 and 1, determining how flat the distribution is.
	}
}
\details{
	The usual function to describe a sine wave is \mjseqn{f(x) = A\sin(2\pi f x + p)}, where \mjseqn{A} is the amplitude, \mjseqn{f} is the frequency (cycles per year), and \mjseqn{p} is the cycle position (in radians) at \mjseqn{x = 0}, and therefore oscillates above and below the x-axis.

	However, a sinusoidal PDF must by definition always be non-negative, which can conceptually be considered as a sine wave stacked on top of a uniform distribution with a height \mjseqn{A + k}, where \mjseqn{k >= 0}.
	Since the PDF is \mjseqn{f(x)} divided by the area below the curve, A and k simplify to a single parameter \mjseqn{r} that determines the relative proportions of the uniform and sinusoidal components, such that:

	when \mjseqn{r = 0} the amplitude of the sine wave component is zero, and the overall PDF is just a uniform distribution between min and max.

	when \mjseqn{r = 1} the uniform component is zero, and the minima of the sine wave touches zero. This does not necessarily mean the PDF minimum equals zero, since a minimum point of the sine wave may not occur with PDF domain (truncated between min and max).

	Therefore the formula for the PDF is:

	\mjsdeqn{\frac{1 + \sin(2\pi f x + p) - \log(r)}{(x_{max} - x_{min})(1 - \log(r)) + (\frac{1}{2\pi f})[\cos(2\pi f x_{min} - p) - \cos(2\pi f x_{max} - p)]}} 
	where \mjseqn{x =} years, and \mjseqn{x_{min}} and \mjseqn{x_{max}} determine the truncated date range.
	}
\examples{
	# A sinewave with a period of 700 years
	x <- seq(1500,4500, length.out=1000)
	y <- sinewavePDF(x, min=2000, max=4000, f=1/700, p=0, r=0.2)
	plot(x,y,type='l')
	}
