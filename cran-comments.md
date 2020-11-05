## Test environments
* local Windows 10, R 4.0.2
* Travis CI
* win-builder (release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Adrian Timpson <a.timpson@ucl.ac.uk>'
	
New submission

## Downstream dependencies
I have also run R CMD check on downstream dependencies of ADMUR 
All packages passed with no ERRORs or WARNINGs and Status: OK

## Further comments
* The build checks identify the following correct URL: https://doi.org/10.1098/rstb.2019.0723 as 'possibly invalid', since this paper is due to be published on 2020-11-30

* The build checks identify: Possibly mis-spelled words in DESCRIPTION:
  SPD (11:303, 11:322)

This intended initialisation is previously fully specified (Summed Probability Distribution).