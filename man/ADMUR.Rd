\name{ADMUR} 
\alias{ADMUR} 
\title{ADMUR R package} 
\docType{package}
\description{
ADMUR: Ancient Demographic Modelling Using Radiocarbon

Tools to directly model underlying dynamics of radiocarbon date datasets using a Continuous Piecewise Linear (CPL) model framework, or Summed Probability Distributions (SPD) within a simulation framework. 
	}
\details{
	Package to model population dynamics from anthropogenic datasets of dates, including radiocarbon dates and other date such as thermoluminescence. Provides tools to perform two modelling approaches:

1. Summed Probability Distribution generation, and simulation testing. Permits the rejection of a null hypothesis.

2. Directly models the underlying population dynamics using a Continuous Piecewise Linear (CPL) model framework. 

The CPL model framework includes tools to: Estimate the dates and magnitude of demographic events (hinge points), both the Maximum likelihood (using DEoptimR search algorithm) and Credible Intervals (using MCMC Metropolis Hastings Algorithm); Perform Goodness of Fit tests; perform model comparison.
	}
\references{
	'Directly modelling population dynamics in the South American Arid Diagonal using 14C dates' by Adrian Timpson, Ramiro Barberena, Mark G. Thomas, César Méndez and Katie Manning, published in Philosophical Transactions of the Royal Society B, 2020. https://doi.org/10.1098/rstb.2019.0723
	}
