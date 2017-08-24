pim package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build\_Status](https://travis-ci.org/CenterForStatistics-UGent/pim.svg?branch=master)](https://travis-ci.org/CenterForStatistics-UGent/pim) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pim)](http://cran.r-project.org/package=pim)

Overview
--------

pim is a package providing probabilistix index models in R, and is available on CRAN. The stable release can be installed using `install.packages("pim")`. The development version in this repo can be installed using the `devtools` package.

    devtools::install_github("CenterForStatistics-UGent/pim")

Older versions
--------------

The current version of the pim package works differently from the example code used in the original publications. The current package provides extensive help pages and a vignette you can use as a reference. With `vignette("pim")` you'll open a PDF with this reference material.

If you insist on trying the old version, you can find this on the [R-Forge repository](https://r-forge.r-project.org/R/?group_id=1120). The R-Forge repository is maintained solely for legacy reasons, but no longer actively used for development. Unless you have a very good reason to do so, we strongly encourage you to use the stable version on CRAN or the development version here on github.

References
----------

The methodology is described in the following paper:

Thas O, De Neve J, Clement L and Ottoy J (2012). “Probabilistic Index Models.” *Journal of the Royal Statistical Society B*, **74**(4), pp. 623-671. [doi: 10.1111/j.1467-9868.2011.01020.x](http://dx.doi.org/10.1111/j.1467-9868.2011.01020.x)

Please refer to the methodological paper when using the pim package for research. How to refer to the pim package itself, is shown by

    citation("pim")
