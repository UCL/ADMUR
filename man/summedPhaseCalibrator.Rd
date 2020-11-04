\name{summedPhaseCalibrator} 
\alias{summedPhaseCalibrator} 
\title{Generates a summed probability distribution (SPD) after phasing dates} 
\description{Generates a Summed Probability Distribution (SPD) after phasing dates.}
\usage{summedPhaseCalibrator(data, calcurve, calrange, inc=5, width=200)}
\arguments{
  \item{data}{
	A dataframe of 14C dates. Requires 'age' and 'sd', and at least one of 'site' and 'phase'. Optional 'datingType' comprising '14C' and/or anything else.
	}
  \item{calcurve}{
	A calibration curve object. Choose from intcal20 (default), shcal20, intcal13 or shcal13.
	}
  \item{calrange}{
	A vector of two cal dates, giving the calendar range of CalArray. Can be in either order.
	}
  \item{inc}{
	Increments to interpolate calendar years. Default = 5.
	}
  \item{width}{
	A timespan in 14C years used to automatically bin dates if they have not been phased, i.e., 'phase' is missing from the data. Default = 200.
	}
}
\value{
	Returns a single-column data frame of SPD probabilities. Row names are the calendar years.
	}
\details{
Wrapper function to generate an overall SPD for phased dates. Internally this first generates an SPD for each phase. Data may be phased already, alternatively if 'phase' is not provided, this function will automatically bin dates into phases, see \code{\link[=phaseCalibrator]{phaseCalibrator}}. Each phase's distribution is then summed, and the final SPD is normalised to unity.

Optionally 'datingType' can be provided in the data. Only '14C' will be calibrated in the usual way, anything else is assumed to be provided in calendar time. If 'datingType' is not provided, all dates are assumed to be 14C.
	}
\examples{
	data <- subset(SAAD, site \%in\% c('Carrizal','Pacopampa'))
	SPD <- summedPhaseCalibrator(data, shcal20, c(2000,6000))
	plotPD(SPD)
	}
