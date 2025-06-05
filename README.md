# actverse <a href = "https://danielvartan.github.io/brandr/"><img src = "man/figures/logo.svg" align="right" width="120" /></a>

<!-- quarto render -->

<!-- badges: start -->
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check.yaml](https://github.com/danielvartan/actverse/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/danielvartan/actverse/actions/workflows/check-standard.yaml)
[![Codecov test
coverage](https://codecov.io/gh/danielvartan/actverse/branch/main/graph/badge.svg)](https://app.codecov.io/gh/danielvartan/actverse?branch=main)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green)](https://choosealicense.com/licenses/mit/)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)
<!-- badges: end -->

## Overview

`actverse` is an R package that offers a comprehensive toolkit for
processing, analyzing, and visualizing
[actigraphy](https://en.wikipedia.org/wiki/Actigraphy) data. It is
designed to support sleep and chronobiology researchers by streamlining
workflows and enhancing reproducibility in actigraphy research.

The package is built on [tidyverse
principles](https://tidyverse.tidyverse.org/articles/manifesto.html) and
integrates seamlessly with the [tidyverse
ecosystem](https://www.tidyverse.org/), ensuring a consistent and
user-friendly experience for data manipulation and analysis.

> If you find this project useful, please consider giving it a star!  
> [![GitHub repo
> stars](https://img.shields.io/github/stars/danielvartan/actverse)](https://github.com/danielvartan/actverse/)

## Installation

You can install `actverse` using the
[`remotes`](https://github.com/r-lib/remotes) package:

``` r
remotes::install_github("danielvartan/actverse")
```

## Usage

The R ecosystem offers a [wide
variety](https://cran.r-project.org/view=TimeSeries) of time series
standards, and selecting the right one was an important decision in
developing `actverse`. A consistent standard for time objects is
essential, as [time can be represented in many
ways](https://youtu.be/eelVqfm8vVc) and may be based on different
numerical systems. For packages that follow [tidyverse
principles](https://tidyverse.tidyverse.org/articles/manifesto.html), we
believe the [`tsibble`](https://tsibble.tidyverts.org/index.html)
package provides the best time series standard. As its name suggests,
`tsibble` extends the [tidyverse](https://www.tidyverse.org/)
[`tibble`](https://tibble.tidyverse.org/) object for time series data.

Most `actverse` functions require your data to be in the
[`tsibble`](https://tsibble.tidyverts.org/index.html) format. Converting
your data is straightforward and can significantly improve your
experience working with time series in R. Please refer to the
[`tsibble`](https://tsibble.tidyverts.org/index.html) documentation for
guidance on adapting your data.

### Read/Write

- [`read_acttrust()`](https://danielvartan.github.io/actverse/reference/read_acttrust.html):
  Read, tidy, and validate an [ActTrust](https://condorinst.com/en/)
  file.
- [`write_acttrust()`](https://danielvartan.github.io/actverse/reference/write_acttrust.html):
  Write a `tsibble` to a readable [ActTrust](https://condorinst.com/en/)
  file.

Example:

``` r
file <- get_from_zenodo(
  doi = "10.5281/zenodo.4898822",
  dir = tempdir(),
  file = "processed.txt"
)
```

``` r
data <- file |> read_acttrust(tz = "America/Sao_Paulo")
```

``` r
library(dplyr)

data |> glimpse()
#> Rows: 51,806
#> Columns: 17
#> $ timestamp            <dttm> 2021-04-24 04:14:00, 2021-04-24 04:15:00, 202…
#> $ pim                  <dbl> 7815, 2661, 3402, 4580, 2624, 3929, 5812, 3182…
#> $ tat                  <dbl> 608, 160, 243, 317, 255, 246, 369, 270, 373, 1…
#> $ zcm                  <dbl> 228, 64, 80, 125, 33, 105, 171, 54, 189, 64, 6…
#> $ orientation          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ wrist_temperature    <dbl> 26.87, 27.18, 27.68, 27.86, 28.04, 28.13, 28.2…
#> $ external_temperature <dbl> 24.62, 25.06, 25.50, 25.75, 25.94, 26.06, 26.3…
#> $ light                <dbl> 3.58, 5.23, 3.93, 4.14, 3.16, 3.63, 11.53, 2.4…
#> $ ambient_light        <dbl> 1.45, 2.12, 1.59, 1.68, 1.28, 1.47, 4.67, 0.97…
#> $ red_light            <dbl> 0.57, 0.86, 0.64, 0.67, 0.51, 0.56, 3.22, 0.37…
#> $ green_light          <dbl> 0.66, 0.95, 0.71, 0.75, 0.57, 0.68, 3.49, 0.44…
#> $ blue_light           <dbl> 0.24, 0.36, 0.26, 0.28, 0.21, 0.30, 1.48, 0.20…
#> $ ir_light             <dbl> 0.17, 0.25, 0.20, 0.20, 0.16, 0.18, 1.00, 0.13…
#> $ uva_light            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ uvb_light            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ event                <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ state                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```

### Sleep Statistics

- [`sri()`](https://danielvartan.github.io/actverse/reference/sri.html):
  Compute Phillips et al. Sleep Regularity Index
  ([SRI](https://doi.org/10.1038/s41598-017-03171-4)).
- [`state_prop()`](https://danielvartan.github.io/actverse/reference/state_prop.html):
  Compute the proportion of time spent in a specific state (e.g.,
  Sleeping, Awake).

Example:

> Compare the results with the actogram plot shown in the following
> sections.

``` r
sri_data <- data |> sri()

sri_data
#> # A tsibble: 1,440 x 5 [1m]
#>   time   state      previous_state agreement    sri
#>   <time> <list>     <list>         <list>     <dbl>
#> 1 00'00" <fct [36]> <fct [36]>     <lgl [36]>  77.1
#> 2 01'00" <fct [36]> <fct [36]>     <lgl [36]>  77.1
#> 3 02'00" <fct [36]> <fct [36]>     <lgl [36]>  77.1
#> 4 03'00" <fct [36]> <fct [36]>     <lgl [36]>  77.1
#> 5 04'00" <fct [36]> <fct [36]>     <lgl [36]>  77.1
#> 6 05'00" <fct [36]> <fct [36]>     <lgl [36]>  77.1
#> # ℹ 1,434 more rows
```

``` r
library(rutils) # github.com/danielvartan/rutils

sri_data |>
  rutils:::stats_summary("sri", threshold = NULL) |>
  print(n = Inf)
#> # A tibble: 15 × 2
#>    name     value             
#>    <chr>    <chr>             
#>  1 n        1440              
#>  2 n_rm_na  1440              
#>  3 n_na     0                 
#>  4 mean     58.7795701029482  
#>  5 var      785.141504008496  
#>  6 sd       28.0203765857723  
#>  7 min      -25               
#>  8 q_1      37.1428571428571  
#>  9 median   63.6363636363637  
#> 10 q_3      86.6666666666667  
#> 11 max      100               
#> 12 iqr      49.5238095238095  
#> 13 range    125               
#> 14 skewness -0.401815749365758
#> 15 kurtosis 2.10073487543524
```

``` r
library(ggplot2)
library(scales)

sri_data |>
  ggplot(ggplot2::aes(x = time, y = sri)) +
  geom_smooth(color = "#FC2913") +
  labs(
    x = "Time of day (Hour)",
    y = "Sleep Regularity Index (SRI)"
  ) +
  scale_x_time(
    breaks = breaks_width("6 hours"),
    labels = label_time("%-H") # Use "%#H" for Windows
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  actverse:::get_actverse_theme()
```

![](man/figures/readme-sri-1.png)

### Period Functions

- [`periodogram()`](https://danielvartan.github.io/actverse/reference/periodogram.html):
  Compute Sokolove & Bushell’s $\chi^{2}$ periodogram.
- [`spectrogram()`](https://danielvartan.github.io/actverse/reference/spectrogram.html):
  Create a spectrogram plot based on Sokolove & Bushell’s $\chi^{2}$
  periodogram.

Example:

``` r
data |> periodogram("pim")
```

![](man/figures/readme-periodogram-1.png)

``` r
data |> spectrogram("pim")
```

![](man/figures/readme-spectrogram-1.png)

### Data Visualization

We strongly recommend using the [`ragg`](https://ragg.r-lib.org/)
package as your backend graphics device for complex data visualizations.
It is faster and produces higher quality images than R default graphics
device.

- [`actogram()`](https://danielvartan.github.io/actverse/reference/actogram.html):
  Create an actogram plot from actigraphy data.

``` r
data |>
  actogram(
    col = "pim",
    days = -1,
    latitude = -23.55065,
    longitude = -46.63338,
    double_plot = TRUE
  )
```

![](man/figures/readme-actogram-1.png)

### Data Interpolation

- [`na_approx()`](https://danielvartan.github.io/actverse/reference/na_approx.html)
  `na_locf()` `na_overall_mean()` `na_overall_median()`
  `na_overall_mode()` `na_spline()` `na_weekly_mean()` `na_zero()`
  `na_plot()`: Interpolate missing values in a numeric vector.

Example:

``` r
x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")

na_approx(x, index, fill_na_tips = TRUE)
#>  [1]  1.0  1.0  5.0 10.0  7.5  5.0 10.0  1.0  5.5 10.0  1.0  5.0  5.0  5.0
```

``` r
na_plot(x, index, na_approx(x, index, fill_na_tips = TRUE))
```

![](man/figures/readme-na-approx-1.png)

### API Clients

- [`get_from_zenodo()`](https://danielvartan.github.io/actverse/reference/get_from_zenodo.html):
  Get data from a Zenodo record.
- [`get_sun_stats()`](https://danielvartan.github.io/actverse/reference/get_sun_stats.html):
  Get sun related statistics from different APIs.

Example:

``` r
library(rutils) # github.com/danielvartan/rutils

get_sun_stats(
  latitude = -23.5489,
  longitude = -46.6388,
  tz = "America/Sao_Paulo"
) |>
  rutils:::list_as_tibble() |>
  print(n = Inf)
#> # A tibble: 18 × 2
#>    name              value            
#>    <chr>             <chr>            
#>  1 date              2025-06-05       
#>  2 latitude          -23.5489         
#>  3 longitude         -46.6388         
#>  4 tz                America/Sao_Paulo
#>  5 sunrise_start     06:44:16         
#>  6 sunrise_end       06:46:50         
#>  7 golden_hour_end   07:17:28         
#>  8 solar_noon        12:06:31         
#>  9 golden_hour_start 16:55:34         
#> 10 sunset_start      17:26:13         
#> 11 sunset_end        17:28:46         
#> 12 dusk              17:53:17         
#> 13 nautical_dusk     18:21:18         
#> 14 night_start       18:48:54         
#> 15 nadir             00:06:31         
#> 16 night_end         05:24:08         
#> 17 nautical_dawn     05:51:44         
#> 18 dawn              06:19:45
```

### Other Features

`actverse` also comes with many utility functions and provides
actigraphy datasets for testing and learning purposes.

All functions are thoroughly documented, with detailed explanations of
the underlying methodologies and computational guidelines. For a
complete list of available functions and their documentation, click
[here](https://danielvartan.github.io/actverse/).

Example:

``` r
file |>
  read_acttrust(regularize = FALSE) |>
  find_epoch()
#> $best_match
#> [1] 60
#> 
#> $prevalence
#> # A tibble: 4 × 2
#>   epoch proportion
#>   <dbl>      <dbl>
#> 1    60  1.00     
#> 2    86  0.0000193
#> 3    94  0.0000193
#> 4   101  0.0000193
```

## How to Cite

``` r
citation("actverse")
#> To cite {actverse} in publications use:
#> 
#>   Vartanian, D., Matias, V. A., Serrano, C. A. M., & Benedito-Silva,
#>   A. A. (2025). {actverse}: A Tidyverse-style toolbox for actigraphy
#>   data analysis [Computer software, R package].
#>   https://danielvartan.github.io/actverse/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Misc{,
#>     title = {{actverse}: A Tidyverse-style toolbox for actigraphy data analysis},
#>     author = {Daniel Vartanian and Vinicius Alves Matias and Cassio Almeida Mattos Serrano and Ana Amélia Benedito-Silva},
#>     year = {2025},
#>     url = {https://danielvartan.github.io/actverse/},
#>     note = {R package},
#>   }
```

## License

[![](https://img.shields.io/badge/license-MIT-green.svg)](https://choosealicense.com/licenses/mit/)

`actverse` code is released under the [MIT
license](https://opensource.org/license/mit). This means you can use,
modify, and distribute the code freely, as long as you include the
original license and copyright notice in any copies or substantial
portions of the software.

## Contributing

[![](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

Contributions are welcome, including bug reports. Take a moment to
review our [Guidelines for
Contributing](https://danielvartan.github.io/actverse/CONTRIBUTING.html).

[![](https://img.shields.io/static/v1?label=Sponsor&message=%E2%9D%A4&logo=GitHub&color=%23fe8e86)](https://github.com/sponsors/danielvartan)

You can also support the development of `actverse` by becoming a
sponsor. Click [here](https://github.com/sponsors/danielvartan) to make
a donation. Please mention `actverse` in your donation message.
