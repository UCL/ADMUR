# 2020-11-11

## This is a resubmission. In this version I have:

* changed all instances of \dontrun{} to \donttest{}, as these take > 5 secs. 

* removed all changes to options(warn). Was previously used to suppress repetitive warnings if summedCalibrator() was run in a loop. I have instead added 'checks' as an argument to this function, giving user choice to suppress.

* removed all changes to par() within functions.

* ensured par() changes in vignettes are reset to user's par().

Many thanks to Gregor Seyer for hugely helpful comments.

## This is a new submission

### Test environments
* local Windows 10, R 4.0.2
* Travis CI
* win-builder (release)

### R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Adrian Timpson <a.timpson@ucl.ac.uk>'
	
New submission

### Downstream dependencies
I have also run R CMD check on downstream dependencies of ADMUR 
All packages passed with no ERRORs or WARNINGs and Status: OK

### Further comments
* The build checks identify the following correct URL: https://doi.org/10.1098/rstb.2019.0723 as 'possibly invalid', since this paper is due to be published on 2020-11-30

* The build checks identify: Possibly mis-spelled words in DESCRIPTION:
  SPD (11:303, 11:322)

This intended initialisation is previously fully specified (Summed Probability Distribution).