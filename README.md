<!-- README.md is generated from README.Rmd. Please edit that file -->

calibration
===========

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/moveability/calibration.svg?branch=master)](https://travis-ci.org/moveability/calibration)
<!-- badges: end -->

The goal of calibration is to …

Installation
------------

You can install the released version of calibration from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("calibration")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("moveability/calibration")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(calibration)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub!

[![Build
Status](https://travis-ci.org/moveability/calibration.svg)](https://travis-ci.org/moveability/calibration)
[![Project Status: Concept - Minimal or no implementation has been done
yet.](http://www.repostatus.org/badges/0.1.0/concept.svg)](http://www.repostatus.org/#concept)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/calibration)](http://cran.r-project.org/web/packages/calibration)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/calibration)

calibration is …

The following functions are implemented:

The following data sets are included:

### News

-   Version released

### Installation

``` r
devtools::install_github("moveability/calibration")
```

### Usage

``` r
library(calibration)

# current verison
packageVersion("calibration")
```

### Test Results

``` r
library(calibration)
library(testthat)

date()

test_dir("tests/")
```
