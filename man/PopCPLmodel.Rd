\name{PopCPLmodel} 
\alias{PopCPLmodel} 
\title{PopCPLmodel R package} 
\docType{package}
\description{
	Tools to directly model underlying dynamics of 14C datasets, using a Continuous Piecewise Linear (CPL) model framework.
	}
\details{
	Package to model population dynamics from anthropogenic 14C datasets, providing a more robust and useful approach than Summed Probability Distribution testing, which typically can only reject (or fail to reject) a null hypothesis.

	Directly models the underlying population dynamics using a Continuous Piecewise Linear (CPL) model framework.

	Estimates the dates and magnitude of demographic events (hinge points), both the Maximum likelihood (using DEoptimR search alogrithm) and Credible Intervals (using MCMC Metropolis Hastings Algorithm)

	Performs Goodness of Fit tests, and model comparison.
	}
\references{
	'Directly modelling population dynamics in the South American Arid Diagonal using 14C dates' by Adrian Timpson, Ramiro Barberena, Mark G. Thomas, César Méndez and Katie Manning, published in Philosophical Transactions of the Royal Society B, 2020
	}
