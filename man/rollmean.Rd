\name{rollmean} 
\alias{rollmean} 
\title{Rolling mean of a vector of values} 
\description{Computes the rolling mean of a vector of values x. I.e., the mean of a sliding window k values wide}
\usage{rollmean(x, k)}
\arguments{
  \item{x}{
	A vector of values. 
	}
  \item{k}{
	Integer width of the rolling window.
	}
}
\value{
	A vector of rolling means
	}
\details{
Equivalent function to the rollmean function in the zoo package, but much faster. 
	}
\examples{
	x <- sample(1:1000000)
	k <- 10000
	rm <- rollmean(x,k)
	}
