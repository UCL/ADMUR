# ADMUR build history

## 20-11-13

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


