[![Build Status](https://travis-ci.org/AdrianTimpson/ADMUR.svg?branch=master)](https://travis-ci.org/AdrianTimpson/ADMUR)
[![UCL Research Software Development](UCLlogo.png&s=70)](https://github.com/UCL)

gap

<img src="UCLlogo.png" alt="UCL Research Software Development" height="70">
<img src="MACELABlogo.png" alt="Molecular And Cultural Evolution Laboratory" height="70">

# ADMUR build history

## 2020-11-18

### Version: 1.0.0.9003

* Resubmission of v.1.0.0.9001 accepted by CRAN

* Added a Cauchy model, as a Gaussian model is the devil's work. Real data from single events have long tails.

* Added a sigmoidal model. Earth-related systems (e.g. climate) should be expected to oscillate.

## 2020-11-13

### Version: 1.0.0.9002

* Added a Gaussian model to existing options of n-CPL, exponential, uniform.

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


