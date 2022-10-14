
<!-- README.md is generated from README.Rmd. Please edit that file -->

# labNoGame

<!-- badges: start -->

[![R-CMD-check](https://github.com/heli-xu/labNoGame/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/heli-xu/labNoGame/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The name of labNoGame loosely means “lab numbers game” (or lab is no
game?), which is basically some of the numbers we have to deal with a
lot in a lab setting from measurements (usually repeated over and over).
The goal of labNoGame is to help you work with data organizing and
management, automating and hopefully speeding up some of the almost
robotic processes.

## Installation

You can install the development version of labNoGame from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("heli-xu/labNoGame")
```

## Example

This is a basic example which shows you how to use `group_mean` to
summarise the means of the measurements for each group of samples.

``` r
library(labNoGame)
## basic example code
group_mean(PlantGrowth, group, weight) 
#> # A tibble: 3 × 3
#>   group     n  mean
#>   <fct> <int> <dbl>
#> 1 ctrl     10  5.03
#> 2 trt1     10  4.66
#> 3 trt2     10  5.53
```
