
<!-- README.md is generated from README.Rmd. Please edit that file -->
driftR <img src="man/figures/logo.png" align="right" />
=======================================================

[![Travis-CI Build Status](https://travis-ci.org/shaughnessyar/driftR.svg?branch=master)](https://travis-ci.org/shaughnessyar/driftR) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/shaughnessyar/driftR?branch=master&svg=true)](https://ci.appveyor.com/project/shaughnessyar/driftR) [![codecov](https://codecov.io/gh/shaughnessyar/driftR/branch/master/graph/badge.svg)](https://codecov.io/gh/shaughnessyar/driftR) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/driftR)](https://cran.r-project.org/package=driftR)

`driftR` is to provide tidy corrections of continuous water-quality monitoring data for intramental drift.

Installation
------------

`driftR` is not available from CRAN yet. In the meantime, you can install the development version of `driftR` from Github with devtools:

``` r
# install.packages("devtools")
devtools::install_github("shaughnessyar/driftR")
```

Background
----------

The `driftR` package implements a series of equations used in [Dr. Elizabeth Hasenmueller's](https://hasenmuellerlab.weebly.com) hydrology and geochemistry research. These equations correct continuous water-quality monitoring data for incramental drift that occurs over time. There are two forms of corrections included in the package - a one-point calibration and a two-point calibration. One-point and two-point callibration values are suited for different types of measurements. The package is currently written for the easiest use with YSI Sonde products.

Usage
-----

As shown, continuous water-quality instrument drift over time, so it becomes necessary to correct the data to maintain accuracy. `driftR` provides four verbs for applying these corrections in a consistent, reproducible manner: *read*, *factor*, *correct*, and *drop*. These verbs are designed to be implemented in that order, though there may be multiple applications of *correct* for a given data set. All of the core functions for `driftR` have the `dr_` prefix, making it easy to use them interactively in RStudio. The following example shows a simple workflow for applying these verbs to some hypothetical data:

``` r
# load the driftR package
library(driftR)

# import data exported from a Sonde 
df <- dr_readSonde(file = "data.csv", define = TRUE)

# calculate correction factor
# results stored in new vector corrFac
df <- dr_factor(df, corrFactor = corrFac, 
                dateVar = Date, 
                timeVar = Time, 
                format = "MDY")

# apply one-point calibration to SpConde;
# results stored in new vector SPConde_Corr
df <- dr_correctOne(df, sourceVar = SpConde, 
                    cleanVar = SPConde_Corr, 
                    calVal = 1.07, 
                    calStd = 1, 
                    factorVar = corrFac)

# apply two-point calibration to pH;
# results stored in new vector ph_Corr
df <- dr_correctTwo(df, sourceVar = pH, 
                    cleanVar = pH_Corr, 
                    calValLow = 7.01, 
                    calStdLow = 7,
                    calValHigh = 11.8, 
                    calStdHigh =  10, 
                    factorVar = corrFac)

# drop observations to account for instrument equilibration
df <- dr_drop(df, head=10, tail=5)
```

All of the core functions return data frames and make use of the tidy evaluation pronoun `.data`, so using them in concert with the pipe (`%>%`) is straightforward:

``` r
# load the driftR package
library(driftR)

# import data exported from a Sonde 
df <- dr_readSonde(file = "data.csv", define = TRUE)

# caclulate correction factors, apply corrections, and drop observations
df <- df %>%
  dr_factor(corrFactor = corrFac, 
            dateVar = Date, 
            timeVar = Time, 
            format = "MDY") %>%
  dr_correctOne(sourceVar = SpConde, 
                cleanVar = SPConde_Corr, 
                calVal = 1.07, 
                calStd = 1, 
                factorVar = corrFac) %>%
  dr_correctTwo(sourceVar = pH, 
                cleanVar = pH_Corr, 
                calValLow = 7.01, 
                calStdLow = 7,
                calValHigh = 11.8, 
                calStdHigh =  10, 
                factorVar = corrFac)%>%
  dr_drop(head=10, tail=5)
```

See the [package website](https://shaughnessyar.github.io/driftR/) for details on these functions and a [detailed vignette](https://shaughnessyar.github.io/driftR/articles/driftR.html) describing their use. An [additional vignette]() describes `driftR`'s use of tidy evaluation and how to implement pipes into data cleaning in greater detail. Finally, we provide a [third vignette]() designed for users of non-YSI Sonde instruments who wish to use `driftR` with their data.

About the Authors
-----------------

Andrew Shaughnessy led the development of this package. He is a senior at [Saint Louis University](https://www.slu.edu) studying Chemistry and Environmental Science.

[Christopher Prener, Ph.D.](https://chris-prener.github.io) assisted in the development of this package. He is an Assistant Professor in the Department of Sociology and Anthropology at [Saint Louis University](https://www.slu.edu). He has a broad interest in computational social science as well as the development of `R` packages to make research more reproducibile and to generalize research code.

[Elizabeth Hasenmueller, Ph.D.](https://hasenmuellerlab.weebly.com) developed the original equations that this package implements and provided the example data. She is an Assistant Professor in the Department of Earth and Atmospheric Science at [Saint Louis University](https://www.slu.edu).

Contributor Code of Conduct
---------------------------

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/shaughnessyar/driftR/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.
