
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit/)
[![R-CMD-check](https://github.com/sbfnk/fitR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sbfnk/fitR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sbfnk/fitR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/sbfnk/fitR?branch=main)

# fitR: Functions for model fitting and inference

The functions contained in this package have been developed in support
of a course on [Model fitting and inference for infectious disease
dynamics](http://sbfnk.github.io/mfiidd/) run annually at the [London
School of Hygiene & Tropical Medicine](https://www.lshtm.ac.uk).

The main purpose of this package is to be a teaching tool, and the
implementation of inference algorithms in this package is not
particularly efficient of stable. For vastly superior alternative
options for fitting models to data in R we recommend to consider,
amongst others

- [rstan](https://github.com/stan-dev/rstan), an interface to
  [Stan](https://mc-stan.org/).
- [rbi](https://github.com/sbfnk/RBi), an interface to
  [LibBi](https://libbi.org/).
- [pomp](https://github.com/kingaa/pomp), for partially observed Markov
  processes.

## Installation

The easiest way to install `fitR` is to use the `remotes` package:

``` r
# install.packages("remotes")
library(remotes)
install_github("sbfnk/fitR")
```
