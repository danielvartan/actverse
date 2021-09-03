
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actverse <a href='https://gipso.github.io/actverse'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/actverse)](https://CRAN.R-project.org/package=actverse)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/gipso/actverse/workflows/R-CMD-check/badge.svg)](https://github.com/gipso/actverse/actions)
[![codecov](https://codecov.io/gh/gipso/actverse/branch/main/graph/badge.svg?token=2bnHxbdw4M)](https://codecov.io/gh/gipso/actverse)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green)](https://choosealicense.com/licenses/mit/)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](https://gipso.github.io/actverse/CODE_OF_CONDUCT.html)
<!-- badges: end -->

## Overview

`actverse` is an R package that provides a complete and consistent
toolkit to process, analyze and visualize actigraphy data in R.

> Please note that this package is currently on the development stage
> and have not yet been [peer
> reviewed](https://devguide.ropensci.org/softwarereviewintro.html).

## Prerequisites

You only need to have some familiarity with the [R programming
language](https://www.r-project.org/) to use the `actverse` main
functions.

In case you don’t feel comfortable with R, we strongly recommend
checking Hadley Wickham and Garrett Grolemund free and online book [R
for Data Science](https://r4ds.had.co.nz/) and the Coursera course from
the John Hopkins University [Data Science: Foundations using
R](https://www.coursera.org/specializations/data-science-foundations-r)
(free for audit students).

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pkg_install("gipso/actverse")
```

## Citation

If you use `actverse` in your research, please consider citing it. We
put a lot of work to build and maintain a free and open-source R
package. You can find the `actverse` citation below.

``` r
citation("actverse")
#> 
#> To cite {actverse} in publications use:
#> 
#>   Vartanian, D., Matias, V. A., Serrano, C. A. M., Benedito-Silva, A.
#>   A., Pedrazzoli, M. (2021). {actverse}: An R Package for actigraphy
#>   data analysis. Retrieved from https://gipso.github.io/actverse/.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Unpublished{,
#>     title = {{actverse}: An R Package for actigraphy data analysis},
#>     author = {Daniel Vartanian and Vinicius Alves Matias and Cassio Almeida Mattos Serrano and Ana Amelia Benedito-Silva and Mario Pedrazzoli},
#>     year = {2021},
#>     url = {https://gipso.github.io/actverse/},
#>     note = {Lifecycle: experimental},
#>   }
```

## Contributing

`actverse` is a community project, everyone is welcome to contribute.
Take a moment to review our [Guidelines for
Contributing](https://gipso.github.io/actverse/CONTRIBUTING.html).

Please note that `actverse` is released with a [Contributor Code of
Conduct](https://gipso.github.io/actverse/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
