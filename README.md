
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

`actverse` is an R package that provides a complete toolkit to process,
analyze and visualize actigraphy data in R. Its aim is to facilitate the
work of sleep and chronobiology scientists with actigraphy data and help
with research reproducibility.

`actverse` adheres to the [tidyverse
principles](https://tidyverse.tidyverse.org/articles/manifesto.html) and
integrates with the [tidyverse ecosystem](https://www.tidyverse.org/).

Our plan is to submit a first version of the `actverse` package to the
[rOpenSci](https://ropensci.org/) software peer-review process in August
2022.

## Prerequisites

You need to have some familiarity with the [R programming
language](https://www.r-project.org/) and with the
[`tsibble`](https://tsibble.tidyverts.org/index.html) package to use
`actverse` main functions.

In case you don’t feel comfortable with R, we strongly recommend
checking Hadley Wickham and Garrett Grolemund’s free and online book [R
for Data Science](https://r4ds.had.co.nz/) and the Coursera course from
the John Hopkins University [Data Science: Foundations using
R](https://www.coursera.org/specializations/data-science-foundations-r)
(free for audit students).

Please refer to the
[`tsibble`](https://tsibble.tidyverts.org/index.html) package
documentation to learn more about it. `tsibble` is an essential package
to deal with time series in R. We also recommend that you read the
[Dates and times](https://r4ds.had.co.nz/dates-and-times.html) chapter
from Wickham & Grolemund’s book [R for Data
Science](https://r4ds.had.co.nz/) and the [tsibble
objects](https://otexts.com/fpp3/tsibbles.html) subchapter from Rob J.
Hyndman & George Athanasopoulos’ book [Forecasting: Principles and
Practice](https://otexts.com/fpp3/).

## Installation

Our development process is based on an agile approach. That means that
each feature is a separate project and it’s integrated to the main
branch as soon as it’s reviewed and validated.

You can install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("gipso/actverse")
```

## Usage

The R ecosystem has a [vast
number](https://cran.r-project.org/view=TimeSeries) of time series
standards and we had to choose one of them while developing `actverse`.
A standard for time objects is a must, because [time can have many
representations](https://youtu.be/eelVqfm8vVc) and can be rooted in
different numerical systems.

We believe that the best time series standard available for packages
that adheres to the [tidyverse
principles](https://tidyverse.tidyverse.org/articles/manifesto.html) is
the [`tsibble`](https://tsibble.tidyverts.org/index.html). As the name
suggests, `tsibble` is an adaptation of the
[tidyverse](https://www.tidyverse.org/)
[`tibble`](https://tibble.tidyverse.org/) object for time series.

Most `actverse` functions will require that your data be in the
[`tsibble`](https://tsibble.tidyverts.org/index.html) standard. Adapting
your data is a simple process and can make a big difference when dealing
with time series. Please refer to
[`tsibble`](https://tsibble.tidyverts.org/index.html) documentation to
learn how to do it. After your data is set to start, just use the
`actverse` functions below.

We also recommend seeing the [`tsbox`](https://www.tsbox.help/) package,
an R package that propose to be an “universal translator” (🖖) for R
time series standards.

### Read/Write

-   `read_acttrust()`: Read, tidy, and validate an
    [ActTrust](https://www.condorinst.com.br/acttrust-actigrafo/) file.
-   `write_acttrust()`: Adapt and write a `tsibble` to a readable
    [ActTrust](https://www.condorinst.com.br/acttrust-actigrafo/) file.

Example:

``` r
file <- get_from_zenodo(
    doi = "10.5281/zenodo.4898822", path = tempdir(),
    file = "processed.txt"
)

data <- read_acttrust(file, tz = "America/Sao_Paulo")

data
#> # A tsibble: 51,806 x 17 [1m] <America/Sao_Paulo>
#>    timestamp             pim   tat   zcm orientation wrist_temperature
#>    <dttm>              <dbl> <dbl> <dbl>       <dbl>             <dbl>
#>  1 2021-04-24 04:14:00  7815   608   228           0              26.9
#>  2 2021-04-24 04:15:00  2661   160    64           0              27.2
#>  3 2021-04-24 04:16:00  3402   243    80           0              27.7
#>  4 2021-04-24 04:17:00  4580   317   125           0              27.9
#>  5 2021-04-24 04:18:00  2624   255    33           0              28.0
#>  6 2021-04-24 04:19:00  3929   246   105           0              28.1
#>  7 2021-04-24 04:20:00  5812   369   171           0              28.2
#>  8 2021-04-24 04:21:00  3182   270    54           0              28.4
#>  9 2021-04-24 04:22:00  6362   373   189           0              28.6
#> 10 2021-04-24 04:23:00  2621   159    64           0              28.7
#> # … with 51,796 more rows, and 11 more variables: external_temperature <dbl>,
#> #   light <dbl>, ambient_light <dbl>, red_light <dbl>, green_light <dbl>,
#> #   blue_light <dbl>, ir_light <dbl>, uva_light <dbl>, uvb_light <dbl>,
#> #   event <dbl>, state <dbl>
```

### Period functions

-   `periodogram()`: Compute Sokolove & Bushell’s
    ![\\chi^{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cchi%5E%7B2%7D "\chi^{2}")
    periodogram.
-   `spectrogram()`: Compute a spectrogram based on Sokolove & Bushell’s
    periodogram.

Example:

``` r
per <- periodogram(data, "pim")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
spec <- spectrogram(data, "pim")
```

<img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

### Data interpolation

-   `na_approx()` `na_locf()` `na_overall_mean()` `na_overall_median()`
    `na_overall_mode()` `na_spline()` `na_weekly_mean()` `na_zero()`
    `na_plot()`: Replace `NA` by interpolation.

Example:

``` r
x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")

na_approx(x, index, fill_na_tips = TRUE)
#>  [1]  1.0  1.0  5.0 10.0  7.5  5.0 10.0  1.0  5.5 10.0  1.0  5.0  5.0  5.0
na_plot(x, index, na_approx(x, index, fill_na_tips = TRUE))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

### API clients

-   `get_from_zenodo()`: Get data from a Zenodo record.
-   `get_sun_stats()`: Get sun related statistics from different APIs.

``` r
get_sun_stats(lat = -23.5489, lon = -46.6388, tz = "America/Sao_Paulo") %>%
    dplyr::as_tibble() %>% 
    t()
#>                   [,1]               
#> date              "2022-05-27"       
#> lat               "-23.5489"         
#> lon               "-46.6388"         
#> tz                "America/Sao_Paulo"
#> sunrise_start     "06:40:20"         
#> sunrise_end       "06:42:52"         
#> golden_hour_end   "07:13:08"         
#> solar_noon        "12:05:10"         
#> golden_hour_start "16:57:11"         
#> sunset_start      "17:27:27"         
#> sunset_end        "17:29:59"         
#> dusk              "17:54:16"         
#> nautical_dusk     "18:22:01"         
#> night_start       "18:49:25"         
#> nadir             "00:05:10"         
#> night_end         "05:20:54"         
#> nautical_dawn     "05:48:18"         
#> dawn              "06:16:03"
```

### Other features

`actverse` also comes equipped with many utility functions. The package
also provides free datasets for testing and learning purposes.

All functions are well documented, showing all the guidelines behind the
computations. Click [here](http://gipso.github.io/actverse) to see a
list of them.

Example:

``` r
# Find the epochs/periodicities in a 'tsibble'
read_acttrust(file, regularize = FALSE) %>%
    find_epoch()
#> $best_match
#> [1] 60
#> 
#> $prevalence
#> # A tibble: 4 × 2
#>   epoch proportion
#>   <dbl>      <dbl>
#> 1    60  1.00     
#> 2    94  0.0000193
#> 3    86  0.0000193
#> 4   101  0.0000193
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
#>   data analysis (v. 0.0.0). https://gipso.github.io/actverse/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Unpublished{,
#>     title = {{actverse}: an R Package for actigraphy data analysis},
#>     author = {Daniel Vartanian and Vinicius Alves Matias and Cassio Almeida Mattos Serrano and Ana Amelia Benedito-Silva and Mario Pedrazzoli},
#>     year = {2022},
#>     url = {https://gipso.github.io/actverse/},
#>     note = {(v. 0.0.0). Lifecycle: experimental},
#>   }
```

## Contributing

We welcome contributions, including bug reports. Take a moment to review
our [Guidelines for
Contributing](https://gipso.github.io/actverse/CONTRIBUTING.html).

Please note that `actverse` is released with a [Contributor Code of
Conduct](https://gipso.github.io/actverse/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
