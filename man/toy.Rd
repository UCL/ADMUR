\name{toy} 
\docType{data}
\alias{toy} 
\title{Toy population history and model} 
\description{
	List containing 'population' and 'model', both dataframe timeseries of a toy population. 'Population' provides counts of individuals in the time range 8kyr to 5kyr BP in 1 year intervals, whilst 'model' provides a PDF across the narrower time range of interest of 7.5kyr to 5.5kyr BP. The model PD outside this range is zero. The broader 'true' population range permits simulation of radiocarbon dates without causing edge effects.
	}
\usage{
	toy
	}
\format{
	A list of two data frames.
	}
\references{
	Timpson, A., Barberena, R., Thomas, M.G., Méndez, C., Manning, K. 2020 Directly modelling population dynamics in the South American Arid Diagonal using 14C dates. Philosophical Transactions of the Royal Society B.
	}