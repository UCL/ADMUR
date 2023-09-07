\name{CPLparsToHinges} 
\alias{CPLparsToHinges} 
\title{Converts CPL parameters (0 to 1) into hinge (x,y) coordinates (date and pdf) that describe a model} 
\description{
	Converts either a vector of parameters, or a matrix of many parameter sets to CPL-model hinges (date and pdf coordinates)
\loadmathjax }
\usage{CPLparsToHinges(pars, years)}
\arguments{
  \item{pars}{
	Either a vector of one parameter set, or a matrix of several parameter sets (one set per row). 
	}
  \item{years}{
	A vector of years.
	}
}
\details{
The CPL model requires pars to be of odd length, each between 0 and 1. A n-CPL model has \mjseqn{2n-1} parameters (\mjseqn{n-1} x-parameters and \mjseqn{n} y-parameters) that are mapped to \mjseqn{n+1} PD coordinates (x,y pairs) using a modified Stick Breaking Dirichlet Process. The first and last x-coordinate are set as \mjseqn{x_{min}} and \mjseqn{x_{max}}, and the remaining internal x-coordinates are converted from their respective x-parameters using the Beta distribution CDF (where \mjseqn{\alpha = 1} and \mjseqn{\beta =} the number of pieces still to be broken). The y-parameters (between 0 and 1) are converted to y-coordinates in two steps. Firstly, they are mapped from the parameter range (0,1) to the coordinate range (0, \mjseqn{\infty}) using the formula \mjseqn{\frac{1}{(1 - y)^2} - 1}, and the single remaining y-coordinate is set as \mjseqn{\frac{1}{(1 - 0.5)^2} - 1}. Secondly, they are normalised by the total area under the curve, calculated as the sum of the areas under all \mjseqn{n} pieces: \mjsdeqn{Area = \sum_{i=1}^{n} (\frac{y_i + y_{i+1}}{2})(x_{i+1} - x_i) }
}

\examples{
	# convert a single random 6-CPL parameter set
	x <- CPLparsToHinges(pars=runif(11), years=5500:7500)
	}
