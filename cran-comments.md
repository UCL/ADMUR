# ADMUR version 1.0.2

## Round 1: 2021-01-26 This is a new submission.

### Test environments
* local Windows 10, R 4.0.3
* Travis CI
* R-hub builder: Fedora Linux, R-devel, clang, gfortran
* R-hub builder: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* R-hub builder: Ubuntu Linux 16.04 LTS, R-release, GCC
* win-builder (release)
* win-builder (devel)

### R CMD check results
* There were no ERRORs, no WARNINGs, no NOTES

### Downstream dependencies
I have run R CMD check on downstream dependencies of ADMUR 
All packages passed with no ERRORs or WARNINGs and Status: OK

#### What changed
* svg() now imported from grDevices in NAMESPACE and DESCRIPTION, following request from Prof Brian Ripley on 2021-01-16.
* 14C datasets from taphonomic papers added (Surovell, Bluhm, Bryson etc).
* Substantial update to guide, detailing taphonomy.
* Taphonomy parameter incorporated into convertPars(), objectiveFunction(), proposalFunction(), mcmc().
* New CPLparsToHinges() helper function purely to handle converting CPL parameters into hinges.

# ADMUR version 1.0.1

## Round 2: 2020-11-29 This is a resubmission. 
In this version I have:
* reduced tarball size from 7.8 MB to 4.1 MB
Many thaks to Uwe Ligges for helpful advice.

## Round 1: 2020-11-28 This is a new submission.
Please accept my apologies for the short period since last submission (23 days). Much development has been done in this time. I anticipate a much longer period before next submission.

### Test environments
* local Windows 10, R 4.0.3
* Travis CI
* R-hub builder: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* R-hub builder: Fedora Linux, R-devel, clang, gfortran
* R-hub builder: Ubuntu Linux 16.04 LTS, R-release, GCC
* win-builder (release)
* win-builder (devel)

### R CMD check results
* There were no ERRORs or WARNINGs. 
* There was 1 NOTE: Maintainer: 'Adrian Timpson <a.timpson@ucl.ac.uk>' Found the following (possibly) invalid URLs: URL: https://doi.org/10.1098/rstb.2019.0723 

### Downstream dependencies
I have also run R CMD check on downstream dependencies of ADMUR 
All packages passed with no ERRORs or WARNINGs and Status: OK

### Further comments
* The possibly invalid URL is actually correct, the paper is in press and will go live on 2020-11-30

#### What changed
* Added a logistic model.
* Added three toy datasets.
* New section in guide, exploring further models.
* Substantial improvement in the area breaking algorithm, total rebuild of convertParsCPL().
* Added a Cauchy model.
* Added a sigmoidal model.
* Added a Gaussian model.
* Added checkData()
* Cleaner mathematical formulae in manuals, using mathjaxr

# ADMUR version 1.0.0

## Round 2: 20-11-11 This is a resubmission. 
In this version I have:
* changed all instances of \dontrun{} to \donttest{}, as these take > 5 secs. 
* removed all changes to options(warn). Was previously used to suppress repetitive warnings if summedCalibrator() was run in a loop. I have instead added 'checks' as an argument to this function, giving user choice to suppress.
* removed all changes to par() within functions.
* ensured par() changes in vignettes are reset to user's par().
Many thanks to Gregor Seyer for hugely helpful comments.

### R CMD check results
There were no ERRORs or WARNINGs. 
There was 1 NOTE:
checking CRAN incoming feasibility ... NOTE
Maintainer: 'Adrian Timpson <a.timpson@ucl.ac.uk>'

## Round 1: 2020-11-06 This is a new submission.

### Test environments
* local Windows 10, R 4.0.2
* Travis CI
* win-builder (release)

### R CMD check results
There were no ERRORs or WARNINGs. 
There was 1 NOTE:
checking CRAN incoming feasibility ... NOTE
Maintainer: 'Adrian Timpson <a.timpson@ucl.ac.uk>'
New submission

### Downstream dependencies
I have also run R CMD check on downstream dependencies of ADMUR 
All packages passed with no ERRORs or WARNINGs and Status: OK

### Further comments
* The build checks identify the following correct URL: https://doi.org/10.1098/rstb.2019.0723 as 'possibly invalid', since this paper is due to be published on 2020-11-30
* The build checks identify: Possibly mis-spelled words in DESCRIPTION: SPD (11:303, 11:322)
This intended initialisation is previously fully specified (Summed Probability Distribution).

