
<!-- README.md is generated from README.Rmd. Please edit that file -->

# labmate

<!-- badges: start -->

[![R-CMD-check](https://github.com/heli-xu/labmate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/heli-xu/labmate/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of labmate is to help you with data management and analysis in
a lab setting, automating and speeding up some of the processes. Many
assays and measurements used in labs require the same data processing
steps every time the experiments are repeated. As some of the
experiments with complex designs generate quite cumbersome spreadsheets,
processing and adding them to existing data (based on grouping
information) could be very time consuming. I used a lot of these
functions when I was working in the lab, and it saved me a lot of time,
especially on the day before lab meeting…

## Installation

You can install the development version of labmate from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("heli-xu/labmate")
```

## Example

This is a basic example which shows you how to use `group_mean` to
summarise the means of the measurements for each group of samples.

``` r
library(labmate)
## basic example code
group_mean(PlantGrowth, group, weight) 
#> # A tibble: 3 × 3
#>   group     n  mean
#>   <fct> <int> <dbl>
#> 1 ctrl     10  5.03
#> 2 trt1     10  4.66
#> 3 trt2     10  5.53
```
