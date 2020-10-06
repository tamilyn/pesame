---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# PESAME: Predictive Effect Size Analysis in Multivariate Ensembles

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of golemshinyapp is to provide an interface to the pesame algorithm.

## Installation

You can install the released version of golemshinyapp from github with:

``` r
library(devtools)
devtools::install_github("alekseyenko/pesame")
```

## Example

Run this as a shiny app

```{r example}
library(pesame)
pesame::run_app()
```

