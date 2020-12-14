<a href="https://github.com/UCL"><img src="tools/logos/logo_UCL.png" alt="UCL Research Software Development" height="70"/></a>
<a href="https://www.ucl.ac.uk/biosciences/departments/genetics-evolution-and-environment/research/molecular-and-cultural-evolution-lab"><img src="tools/logos/logo_MACElab.png" alt="UCL Research Software Development" height="70"/></a>
<a href="https://www.shh.mpg.de"><img src="tools/logos/logo_MPI.png" alt="Max Planck Institute for the Science of Human History" height="70"/></a>
<a href="https://www.shh.mpg.de/1143811/pan-ev"><img src="tools/logos/logo_PanEv.png" alt="Pan African Evolution ResearchGroup" height="70"/></a>

[![Build Status](https://travis-ci.org/AdrianTimpson/ADMUR.svg?branch=master)](https://travis-ci.org/AdrianTimpson/ADMUR)

# ADMUR
## Ancient Demographic Modelling Using Radiocarbon

Tools to directly model underlying population dynamics using chronological datasets (radiocarbon and other) with a variety of models, including Continuous Piecewise Linear (CPL) model framework, and model comparison framework using BIC. Package also calibrates 14C samples, and generates Summed Probability Distributions (SPD).  CPL modelling directly estimates the most likely population trajectory given a dataset, using SPD simulation analysis to generate a Goodness-of-fit test for the best selected model.

Please contact a.timpson@ucl.ac.uk  in the first instance to make suggestions, report bugs or request help.

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

---