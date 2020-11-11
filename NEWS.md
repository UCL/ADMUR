# ADMUR 1.0.0

## 20-11-11 

Repairs following the fix and resubmit request from the CRAN. Many thanks to Gregor Seyer for hugely helpful comments.


* All instances of \dontrun{} in the manual examples changed to \donttest{}, as these take > 5 secs. 

* Changes to warning settings removed from summedCalibrator(). Instead, to avoid repetitive warnings in a loop, warnings are now a logical argument.

* All instances of par() changes within functions have been removed. 

* All instances of par() changes in vignettes are reset to user's par().


## 20-11-06 
Initial submission to CRAN


