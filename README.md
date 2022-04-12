
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actverse

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/gipso/actverse/workflows/R-CMD-check/badge.svg)](https://github.com/gipso/actverse/actions)
[![Codecov test
coverage](https://codecov.io/gh/gipso/actverse/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gipso/actverse?branch=main)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green)](https://choosealicense.com/licenses/mit/)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](https://gipso.github.io/actverse/CODE_OF_CONDUCT.html)
<!-- badges: end -->

## Overview

`actverse` is an R package that provides a complete and consistent
toolkit to process, analyze and visualize actigraphy data in R. The aim
of `actverse` is to facilitate the work of sleep and chronobiology
scientists with actigraphy data while also helping with research
reproducibility.

`actverse` adheres to the [tidyverse
principles](https://tidyverse.tidyverse.org/articles/manifesto.html) and
integrates with the [tidyverse ecosystem](https://www.tidyverse.org/).
That’s why we choose to work with the
[`tsibble`](https://tsibble.tidyverts.org/index.html) standard for time
series, that also follow the same principles.

## Prerequisites

You only need to have some familiarity with the [R programming
language](https://www.r-project.org/) to use the `actverse` main
functions.

In case you don’t feel comfortable with R, we strongly recommend
checking Hadley Wickham and Garrett Grolemund’s free and online book [R
for Data Science](https://r4ds.had.co.nz/) and the Coursera course from
the John Hopkins University [Data Science: Foundations using
R](https://www.coursera.org/specializations/data-science-foundations-r)
(free for audit students).

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("gipso/actverse")
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
#>   A., & Pedrazzoli, M. (2022). {actverse}: an R Package for actigraphy
#>   data analysis. https://gipso.github.io/actverse/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Unpublished{,
#>     title = {{actverse}: an R Package for actigraphy data analysis},
#>     author = {Daniel Vartanian and Vinicius Alves Matias and Cassio Almeida Mattos Serrano and Ana Amelia Benedito-Silva and Mario Pedrazzoli},
#>     year = {2022},
#>     url = {https://gipso.github.io/actverse/},
#>     note = {Lifecycle: experimental},
#>   }
```

## Contributing

We welcome contributions, including bug reports. Take a moment to review
our [Guidelines for
Contributing](https://gipso.github.io/actverse/CONTRIBUTING.html).

Please note that `actverse` is released with a [Contributor Code of
Conduct](https://gipso.github.io/actverse/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
