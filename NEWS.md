<a href="https://github.com/UCL"><img src="tools/logos/logo_UCL.png" alt="UCL Research Software Development" height="70"/></a>
<a href="https://www.ucl.ac.uk/biosciences/departments/genetics-evolution-and-environment/research/molecular-and-cultural-evolution-lab"><img src="tools/logos/logo_MACElab.png" alt="UCL Research Software Development" height="70"/></a>
<a href="https://www.shh.mpg.de"><img src="tools/logos/logo_MPI.png" alt="Max Planck Institute for the Science of Human History" height="70"/></a>
<a href="https://www.shh.mpg.de/1143811/pan-ev"><img src="tools/logos/logo_PanEv.png" alt="Pan African Evolution ResearchGroup" height="70"/></a>

[![Build Status](https://travis-ci.org/AdrianTimpson/ADMUR.svg?branch=master)](https://travis-ci.org/AdrianTimpson/ADMUR)

# ADMUR build history

### Version: 1.0.3.9000

## 2021-03-19
* svg replaced with png to resolve request from Pof Brian Ripley.

## 2021-01-22

### Version: 1.0.2.9001
* svg now imported from grDevices in NAMESPACE, following request from Prof Brian Ripley.
* 14C datasets from taphonomic papers added (Surovell, Bluhm, Bryson etc).
* Substantial update to guide, detailing taphonomy.

## 2021-01-21

### Version: 1.0.2.9000
* Taphonomy option now included: major update.
* Taphonomy parameter incorporated into convertPars(), objectiveFunction(), proposalFunction(), mcmc()
* New CPLparsToHinges() helper function purely to handle converting CPL parameters into hinges

## 2020-12-16

### Version: 1.0.1.9004
* Toy data sets 1,2,3 changed to a more recent date range, to better demonstrate the power law model.
* Power Law model added to guide, and all guide models rerun under 'Other models in ADMUR'.
* MCMC extended to handle other models.

## 2020-12-15

### Version: 1.0.1.9003
* Integrated full formula for truncated Cauchy model into calculations and manual.
* Added Power law model

## 2020-12-14

### Version: 1.0.1.9002
* Typo corrected in Guide - thanks to Ricardo Fernandes.
* Bug in relativeRate() corrected. Generation time was fixed on 25 yrs.
* Further detail in guide on likelihood calculations.

## 2020-11-27

### Version: 1.0.1.9001
* Added a logistic model.
* Added three toy datasets.
* New section in guide, exploring further models.

## 2020-11-21

### Version: 1.0.1.9000
* Substantial improvement in the area breaking algorithm, total rebuild of convertParsCPL().
Be aware that the raw parameter values now differ from v1.0.0 for a given model.

## 2020-11-18

### Version: 1.0.0.9003
* Added a Cauchy model, as a Gaussian model is the devil's work. Real data from single events have long tails.
* Added a sigmoidal model. Earth-related systems (e.g. climate) should be expected to oscillate.

## 2020-11-13

### Version: 1.0.0.9002
* Added a Gaussian model.
* Added checkData()

## 20-11-11 

### Version: 1.0.0.9001
Resubmission to CRAN as version 1.0.0 following the fix and resubmit request. Many thanks to Gregor Seyer for hugely helpful comments.
* All instances of \dontrun{} in the manual examples changed to \donttest{}, as these take > 5 secs. 
* Changes to warning settings removed from summedCalibrator(). Instead, to avoid repetitive warnings in a loop, warnings are now a logical argument.
* All instances of par() changes within functions have been removed. 
* All instances of par() changes in vignettes are reset to user's par().

## 20-11-06 

### Version: 1.0.0.9000

Initial submission to CRAN as version 1.0.0


