# Get data from a Zenodo record

`get_from_zenodo()` allows you to easy download a Zenodo record or a
Zenodo file.

**Note**: This function only works for Zenodo created DOIs (not when the
DOI is, for example, derived from Zookeys).

## Usage

``` r
get_from_zenodo(doi, dir = ".", file = NULL, parallel = FALSE, force = FALSE)
```

## Arguments

- doi:

  A string indicating a Zenodo DOI (Digital Object Identifier) starting
  with `"10.5281/zenodo."`. See the Examples section to learn more.

- dir:

  (optional) A string indicating a directory dir where the data must be
  downloaded (default: `"."`).

- file:

  (optional) A [`character`](https://rdrr.io/r/base/character.html)
  vector with the names of the files that must be downloaded. If `NULL`,
  the function will download the entire record (default: `NULL`).

- parallel:

  (optional) A [`logical`](https://rdrr.io/r/base/logical.html) flag
  indicating if the function must run a number of parallel processes,
  each downloading another file. This is useful when multiple large
  files are present in the Zenodo record, which otherwise would be
  downloaded sequentially. This operation is limited by bandwidth and
  traffic limitations (default: `FALSE`).

- force:

  (optional) A [`logical`](https://rdrr.io/r/base/logical.html) flag
  indicating if the function should download the files even if they
  already exist (default: `FALSE`).

## Value

A [`character`](https://rdrr.io/r/base/character.html) object with the
paths for all the files downloaded.

## Details

### Zenodo API

You can find more about the Zenodo API at:
<https://developers.zenodo.org>

### License

`get_from_zenodo()` code is based on the `download_zenodo()` function
found in the [`inborutils`](https://github.com/inbo/inborutils) package
of the [Research Institute for Nature and Forest
(INBO)](http://www.inbo.be/en). `download_zenodo()` was created by Hans
Van Calster (hans.vancalster@inbo.be) and Floris Vanderhaeghe
(floris.vanderhaeghe@inbo.be).

We give our thanks for the INBO institute and for all developers
involved in this piece of software.

Please note that this code comes with an [MIT
License](https://opensource.org/license/mit). You can read the latter
below.

    Copyright (c) 2016 Instituut voor Natuur en Bosonderzoek (INBO)

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

## See also

Other API functions:
[`get_sun_stats()`](https://danielvartan.github.io/actverse/reference/get_sun_stats.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  library(readr)

  ## Downloading a single file from a Zenodo record

  dir <- tempdir()
  file <- "sleep-diary.txt"

  get_from_zenodo(
    doi = "10.5281/zenodo.4898822",
    dir = dir,
    file = file
  )

  read_lines(file.path(dir, file))

  ## Downloading all the files from a Zenodo record

  get_from_zenodo(
    doi = "10.5281/zenodo.4898822",
    dir = tempdir(),
    file = NULL
  )
} # }
```
