\name{ADMUR} 
\alias{ADMUR} 
\title{ADMUR R package} 
\docType{package}
\description{
ADMUR: Ancient Demographic Modelling Using Radiocarbon

Tools to directly model the underlying dynamics of prehistoric population change using radiocarbon date datasets.
Model comparison tools for various population models, including a Continuous Piecewise Linear (CPL) model framework, common models such as Gaussian, Cauchy, Sigmoid etc.
Summed Probability Distributions (SPD) within a simulation framework. 
	}
\details{
	Package to model population dynamics from anthropogenic datasets of dates, including radiocarbon dates and other date such as thermoluminescence. Provides tools to perform and compare three modelling approaches:

1. Summed Probability Distribution (SPD) generation, and simulation testing. Permits the rejection of a null hypothesis.

2. Directly modelling the underlying population dynamics using a Continuous Piecewise Linear (CPL) model framework. 

3. Directly modelling the underlying population dynamics using common parameteric models. 

Tools to: Estimate the dates and magnitude of demographic events (hinge points), both the Maximum likelihood (using DEoptimR search algorithm) and Credible Intervals (using MCMC Metropolis Hastings Algorithm); Perform Goodness of Fit tests; perform model comparison.
	}
\references{
	'Directly modelling population dynamics in the South American Arid Diagonal using 14C dates' by Adrian Timpson, Ramiro Barberena, Mark G. Thomas, César Méndez and Katie Manning, published in Philosophical Transactions of the Royal Society B, 2020. https://doi.org/10.1098/rstb.2019.0723
	
	'ADMUR: Ancient Demographic Modelling Using Radiocarbon. Adrian Timpson. University College London. 
	Research Department of Genetics, Environment and Evolution (GEE), Darwin Building, Gower Street, London, WC1E 6BT. 2020 
	https://github.com/UCL/ADMUR
	}
