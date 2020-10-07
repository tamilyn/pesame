
<!-- README.md is generated from README.Rmd. Please edit that file -->


# PESAME: Predictive Effect Size Analysis in Multivariate Ensembles

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The app provides non-parametric Mann Whitney tests functionality, coupled with adjustment for multiple comparisons, and predictive effect size estimation using ROC AUC.

## Installation

You can install the released version of pesame from github with:

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

## Run the app using Docker

On Mac run the following commands in the terminal to start the app and open the browser:

```
docker run -p 3838:3838 alekseyenko/pesame &
sleep 10
open http://0.0.0.0:3838
```

To stop the app when done run the following in the terminal

```
docker kill `docker ps | grep alekseyenko/pesame | cut -d' ' -f1`
```
