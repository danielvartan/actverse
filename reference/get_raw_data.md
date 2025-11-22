# Get paths to `actverse` raw datasets

`actverse` comes bundled with raw datasets for testing and learning
purposes. `get_raw_data()` makes it easy to access their paths.

## Usage

``` r
get_raw_data(file = NULL)
```

## Arguments

- file:

  (optional) A [`character`](https://rdrr.io/r/base/character.html)
  vector specifying the raw data file name(s) to retrieve. If `NULL`,
  returns all available raw data file names (default: `NULL`).

## Value

If `file` is `NULL`, a
[`character`](https://rdrr.io/r/base/character.html) object with all
file names available. Else, a string with the file name path.

## See also

Other utility functions:
[`aggregate_index()`](https://danielvartan.github.io/actverse/reference/aggregate_index.md),
[`find_epoch()`](https://danielvartan.github.io/actverse/reference/find_epoch.md)

## Examples

``` r
get_raw_data()
#> [1] "acttrust.txt"

get_raw_data("acttrust.txt")
#> [1] "/home/runner/work/_temp/Library/actverse/extdata/acttrust.txt"
```
