\name{convertPars} 
\alias{convertPars} 
\title{Converts parameters to x,y coordinates that describe a model PDF} 
\description{Converts a single vector of x and y parameters between 0 and 1 to x,y coordinates that describe a model PDF}
\usage{convertPars(pars, years)}
\arguments{
  \item{pars}{
	A single vector of parameters, each between 0 and 1. Must be an odd length since parameters correspond to: (y,x,...y)
	}
  \item{years}{
	A vector of years.
	}
}
\value{
	Returns a data frame describing the x,y coordinates of the corresponding CPL model hinge points, x-stick lengths, and cumulative areas at each hinge.
	}
\details{
Employs an Area breaking Process (a modified Stick Breaking Dirichlet Process) to map parameters to their corresponding PDF coordinates.  This uses the Beta distribution CDF (with alpha = 1 and beta = the number of pieces still to be broken) to convert an x-parameter into its equivalent x-coordinate value, and the Gamma CDF (with alpha = 1 and beta = 1) to convert a y-parameter into its equivalent coordinate value between 0 and +infinity.
	}
\examples{
	# generate a random model PDF 
	convertPars(pars = runif(11),years = 5500:7500)
	}
