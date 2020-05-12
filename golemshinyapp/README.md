
<!-- README.md is generated from README.Rmd. Please edit that file -->

# golemshinyapp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of golemshinyapp is to provide an interface to the pesame
algorithm.

## Installation

You can install the released version of golemshinyapp from github with:

``` r
library(devtools)
devtools::install_github("alekseyenko/pesame/golemshinyapp")
```

## Example

Run this as a shiny app

``` r
library(golemshinyapp)
#> Warning: replacing previous import 'Hmisc::summarize' by 'dplyr::summarize' when
#> loading 'golemshinyapp'
#> Warning: replacing previous import 'Hmisc::src' by 'dplyr::src' when loading
#> 'golemshinyapp'
#> Warning: replacing previous import 'dplyr::collapse' by 'glue::collapse' when
#> loading 'golemshinyapp'
#> Warning: replacing previous import 'dplyr::group_rows' by
#> 'kableExtra::group_rows' when loading 'golemshinyapp'
#> Warning: replacing previous import 'Hmisc::subplot' by 'plotly::subplot' when
#> loading 'golemshinyapp'
#> Warning: replacing previous import 'DT::dataTableOutput' by
#> 'shiny::dataTableOutput' when loading 'golemshinyapp'
#> Warning: replacing previous import 'DT::renderDataTable' by
#> 'shiny::renderDataTable' when loading 'golemshinyapp'
#> Warning: replacing previous import 'shiny::runExample' by 'shinyjs::runExample'
#> when loading 'golemshinyapp'
#> Warning: replacing previous import 'Hmisc::html' by 'shinyjs::html' when loading
#> 'golemshinyapp'
## basic example code
```
