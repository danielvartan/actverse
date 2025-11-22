# An ActTrust 1 actigraphy record

A small sample (1 day length) of an
[ActTrust](https://condorinst.com/en/) (model 1) actigraphy record.

ActTrust is a trademark of [Condor Instruments
Ltda](https://condorinst.com/en/).

**Note**: This dataset is based on older methodologies, some of which
have since been improved or replaced. For up-to-date recommendations on
sleep scoring, please refer to the Brazilian Consensus on Actigraphy
([Pedrazzoli & Gonçalves,
2021](https://doi.org/10.5281/zenodo.15377336)).

## Usage

``` r
acttrust
```

## Format

A [`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html) with
17 columns and 1,441 rows:

- timestamp:

  Date and time of the recorded events.  
    
  R class: [`POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html).

- pim:

  Wrist activity in Proportional Integral Mode (PIM).  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- tat:

  Wrist activity in Time Above Threshold (TAT) mode.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- zcm:

  Wrist activity in Zero Crossing Mode (ZCM).  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- orientation:

  Device orientation.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- wrist_temperature:

  Wrist temperature in degrees `Celsius`.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- external_temperature:

  External temperature in degrees `Celsius` (unreliable data).  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- light:

  Light intensity in `lux`.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- ambient_light:

  Ambient light intensity in `microwatts/square meter`.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- red_light:

  Red light intensity in `microwatts/square centimeter`.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- green_light:

  Green light intensity in `microwatts/square centimeter`.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- blue_light:

  Blue light intensity in `microwatts/square centimeter`.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- ir_light:

  Infrared light intensity in `microwatts/square centimeter`.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- uva_light:

  Ultraviolet A light intensity in `microwatts/square centimeter`.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- uvb_light:

  Ultraviolet B light intensity in `microwatts/square centimeter`.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- event:

  A binary number indicating the moments when the subject pressed the
  event button: `0` when the button is not pressed and `1` when the
  button is pressed. This is commonly used to indicate when the subject
  goes to sleep or wakes up.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

- state:

  An integer number indicating the subject state: `0` for awake, `1` for
  sleeping, `2` for resting, `4` for offwrist (device removal), `6` for
  editable state 1, `7` for editable state 2, and `8` for editable
  state 3. These states can be assigned by a specialist or by a
  validated inference algorithm.  
    
  R class: [`numeric`](https://rdrr.io/r/base/numeric.html).

## Source

Created by [Daniel Vartanian](https://linktr.ee/danielvartan) (package
author).

## Details

This dataset was created by one of the `actverse` package authors and is
free for all kinds of use. You can download the full dataset at:
<https://zenodo.org/record/4898822>

### Raw data

The raw data for this dataset can be found by running the following
code:

    [`get_raw_data("acttrust.txt")`][get_raw_data()].

### Data munging

This dataset was processed with the default settings of
[`read_acttrust()`](https://danielvartan.github.io/actverse/reference/read_acttrust.md).
Details about the scoring process can be found in the Metadata section.

### Variable naming

Variable names follow the [tidyverse style
guide](https://style.tidyverse.org/), using lowercase letters and
underscores to ensure clarity and consistency.

### Variable classes

R supports a [wide variety](https://cran.r-project.org/view=TimeSeries)
of time series data formats. We use
[`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html) as it
aligns well with [tidyverse
principles](https://tidyverse.tidyverse.org/articles/manifesto.html) and
offers a modern, consistent approach to time series data.

To facilitate interoperability with other standards, all variables are
stored as [`numeric`](https://rdrr.io/r/base/numeric.html) vectors
except for the index, which is of class
[`POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html). This design choice
ensures that data from `actverse` can be easily converted to other
formats if needed.

## Metadata

### Subject

- Age: `35`

- Gender: Male

- Height: `180` cm

- Weight: `85` kg

- BMI: `~26.235` (overweight)

### Record

- Body part: Left wrist (non-dominant wrist)

- Start: `2021-04-24 04:14:25`

- End: `2021-05-31 03:39:06`

- Days: `37`

- Epoch (sample interval): `60` seconds

- Mode: PIM, TAT, and ZCM

- TAT Threshold: `1024`

- Location: São Paulo, SP, Brazil

- UTC: `-3`

- DST: `FALSE`

- Light phase: `06:23:59` - `17:44:58`

- Dark phase: `17:44:59` - `06:23:58`

### Actigraph

- Brand: [Condor Instruments](https://condorinst.com/en/)

- Model: [ActTrust 1](https://condorinst.com/en/)

- Hardware version: `3.1`

- Firmware version: `3.7`

- Manual: `1.0.13` (PT-BR)

- Software: ActStudio Alpha 1.0.17 (Win \| Mac)

### Scoring algorithm settings

- Condor Instruments algorithm version 1.0.17.

- The sleep diary data was not used for scoring.

### Scoring review

- Offwrist periods were indicated from sudden drops in wrist
  temperature.

&nbsp;

- Micro sleep states (isolated states lasting less than `5 minutes`)
  were removed.

&nbsp;

- Minor adjustments were made on the sleep onset and sleep offset,
  taking into consideration the event button and sleep diary records.

### Sleep statistics settings

- Multiple main sleep periods: `TRUE`.

- Main sleep period time of day: `Day or Night`.

- Minimum duration for main sleep periods: `01:00:00`.

- Awake period limit: `02:00:00`.

## References

Pedrazzoli, M., Gonçalves, B. da S. B., Benedito-Silva, A. A.,
Toscanini, A. C., Souza, A. P. P. B. de, Conway, B. A., Vartanian, D.,
Santos, E. H. R., Pires, M. L. N., & Leocadio-Miguel, M. A. (2021).
*Consenso brasileiro de actigrafia*. Segmento Farma Editores.
[doi:10.5281/zenodo.15377336](https://doi.org/10.5281/zenodo.15377336)
