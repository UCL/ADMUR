\name{checkData} 
\alias{checkData} 
\title{Checks a dataset for obvious clangers} 
\description{Performs some rudimentary sanity checks on a radiocarbon (or other) dataset}
\usage{checkData(data)}
\arguments{
  \item{data}{
	A dataframe of 14C dates. Requires 'age' and 'sd', and at least one of 'site' and 'phase'. Optional 'datingType' to include '14C' and anything else.
	}
}
\value{
	NULL
	}
\details{
Performs some rudimentary checks on the radiocarbon dataset, ensuring structure is as required, ages and sds look vaguely sensible etc. 
This is no substitute for poor data hygiene, and the analyst should of course have a toolkit of many other checks, e.g., to avoid duplicate labcodes.
	}
\examples{
	checkData(SAAD)
	}
