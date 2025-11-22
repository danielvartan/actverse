# Package index

## Read/Write

Functions to read and write various actigraphy data formats.

- [`read_acttrust()`](https://danielvartan.github.io/actverse/reference/read_acttrust.md)
  : Read, tidy, and validate an ActTrust file

- [`write_acttrust()`](https://danielvartan.github.io/actverse/reference/write_acttrust.md)
  :

  Write a `tsibble` to a readable ActTrust file

## Sleep analysis

Functions to compute sleep statistics.

- [`sri()`](https://danielvartan.github.io/actverse/reference/sri.md) :
  Compute Phillips et al.'s Sleep Regularity Index (SRI)
- [`state_prop()`](https://danielvartan.github.io/actverse/reference/state_prop.md)
  : Compute the proportion of time spent in a specific state

## Period analysis

Functions for period analysis.

- [`periodogram()`](https://danielvartan.github.io/actverse/reference/periodogram.md)
  : Compute Sokolove & Bushell's \\\chi^{2}\\ periodogram
- [`spectrogram()`](https://danielvartan.github.io/actverse/reference/spectrogram.md)
  : Create a spectrogram plot based on Sokolove & Bushell's periodogram

## Data visualization

Functions to visualize actigraphy data.

- [`actogram()`](https://danielvartan.github.io/actverse/reference/actogram.md)
  : Create an actogram plot from actigraphy data

## Data interpolation

Functions for data interpolation using several techniques compatible
with [`tsibble`](https://tsibble.tidyverts.org/).

- [`na_approx()`](https://danielvartan.github.io/actverse/reference/na_approx.md)
  [`na_locf()`](https://danielvartan.github.io/actverse/reference/na_approx.md)
  [`na_overall_mean()`](https://danielvartan.github.io/actverse/reference/na_approx.md)
  [`na_overall_median()`](https://danielvartan.github.io/actverse/reference/na_approx.md)
  [`na_overall_mode()`](https://danielvartan.github.io/actverse/reference/na_approx.md)
  [`na_spline()`](https://danielvartan.github.io/actverse/reference/na_approx.md)
  [`na_weekly_mean()`](https://danielvartan.github.io/actverse/reference/na_approx.md)
  [`na_zero()`](https://danielvartan.github.io/actverse/reference/na_approx.md)
  [`na_plot()`](https://danielvartan.github.io/actverse/reference/na_approx.md)
  : Interpolate missing values in a numeric vector

## API clients

Client functions for accessing external data sources via APIs.

- [`get_from_zenodo()`](https://danielvartan.github.io/actverse/reference/get_from_zenodo.md)
  : Get data from a Zenodo record
- [`get_sun_stats()`](https://danielvartan.github.io/actverse/reference/get_sun_stats.md)
  : Get sun related statistics from different APIs

## Data

Example datasets for testing and learning purposes.

- [`acttrust`](https://danielvartan.github.io/actverse/reference/acttrust.md)
  : An ActTrust 1 actigraphy record

## Utilities

Miscellaneous utility functions for actigraphy data manipulation and
analysis.

- [`aggregate_index()`](https://danielvartan.github.io/actverse/reference/aggregate_index.md)
  :

  Aggregate the index of a `tsibble`

- [`find_epoch()`](https://danielvartan.github.io/actverse/reference/find_epoch.md)
  : Find distinct epochs in a series

- [`get_raw_data()`](https://danielvartan.github.io/actverse/reference/get_raw_data.md)
  :

  Get paths to `actverse` raw datasets
