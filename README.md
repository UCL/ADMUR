<a href="https://github.com/UCL"><img src="tools/logos/logo_UCL.png" alt="UCL Research Software Development" height="70"/></a>
<a href="https://www.ucl.ac.uk/biosciences/departments/genetics-evolution-and-environment/research/molecular-and-cultural-evolution-lab"><img src="tools/logos/logo_MACElab.png" alt="UCL Research Software Development" height="70"/></a>
<a href="https://www.shh.mpg.de"><img src="tools/logos/logo_MPI.png" alt="Max Planck Institute for the Science of Human History" height="70"/></a>
<a href="https://www.shh.mpg.de/1143811/pan-ev"><img src="tools/logos/logo_PanEv.png" alt="Pan African Evolution ResearchGroup" height="70"/></a>

[![Build Status](https://travis-ci.org/AdrianTimpson/ADMUR.svg?branch=master)](https://travis-ci.org/AdrianTimpson/ADMUR)

# ADMUR
## Ancient Demographic Modelling Using Radiocarbon

Statistical tools to directly model underlying population dynamics using date datasets (radiocarbon and other). 

### Population modelling

Various model structures can be compared in a robust formal model comparison framework. Continuous Piecewise Linear (CPL) models are infinitely flexible, and can recover complex population dynamics if enough data is available. Other simpler models include: Uniform, Exponential, Gaussian, Cauchy, Sinusoidal, Logistic and Power law. Taphonomic loss included optionally as a power function. 

### Bayesian Parameter estimation

Posterior parameter estimates of population models, using model likelihoods and a weak uniform prior. 

### SPDs and simulation based testing

Package also calibrates 14C samples, generates Summed Probability Distributions (SPD), and performs SPD simulation analysis to generate a Goodness-of-fit test for the best selected model. 
Continuous Piecewise Linear (CPL) models that are flexible to estimate any complex population dynamics

## Installation
Install from CRAN, then load
``` r
install.packages('ADMUR')
library('ADMUR')
```

## Guide

Refer to the vignette 'guide' for detailed support and examples.

``` r
vignette('guide', package = 'ADMUR')
```

## Contact

Please contact a.timpson@ucl.ac.uk  in the first instance to make suggestions, report bugs or request help.

## References

This package accompanies the following paper:

Timpson A., Barberena R., Thomas M. G., Mendez C., Manning K. 2020. "Directly modelling population dynamics in the South American Arid Diagonal using 14C dates",Philosophical Transactions B. <https://doi.org/10.1098/rstb.2019.0723>.

Citations available as follows:
``` r
citation(package='ADMUR')
```

---

ADMUR was written in collaboration with:
- University College London
    - Department of Genetics, Evolution and Environment
    - Molecular and Cultural Evolution Laboratory
    - UCL Research Software Development
- Max Planck Institute for the Science of Human History
    - Department of Archaeology
    - Pan African Evolution ResearchGroup

Special thanks to Yoan Diekmann for his influential inferential input.

Also thanks to the following who have reported bugs, requested additional functionality, offered constructive criticism, or provided other advice:
- Gregor Seyer 
- Uwe Ligges
- Prof Brian Ripley
- Enrico Crema
- Ricardo Fernandes
- Mark G. Thomas
---