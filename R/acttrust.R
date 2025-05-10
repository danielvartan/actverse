#' An ActTrust 1 actigraphy record
#'
#' @description
#'
#' A small sample (1 day length) of an [ActTrust](https://condorinst.com/en/)
#' (model 1) actigraphy record.
#'
#' ActTrust is a trademark of
#' [Condor Instruments Ltda](https://condorinst.com/en/).
#'
#' **Note**: This dataset is based on older methodologies, some of which have
#' since been improved or replaced. For up-to-date recommendations on sleep
#' scoring, please refer to the Brazilian Consensus on Actigraphy
#' ([Pedrazzoli & Gonçalves, 2021](https://doi.org/10.5281/zenodo.15377336)).
#'
#' @details
#'
#' This dataset was created by one of the `actverse` package authors and is free
#' for all kinds of use. You can download the full dataset at:
#' \url{https://zenodo.org/record/4898822}
#'
#' ## Raw data
#'
#' The raw data for this dataset can be found by running the following code:
#'
#' ```r
#' [`get_raw_data("acttrust.txt")`][get_raw_data()].
#' ```
#'
#' ## Data munging
#'
#' This dataset was processed with the default settings of
#' [`read_acttrust()`][actverse::read_acttrust()]. Details about the scoring
#' process can be found in the Metadata section.
#'
#' ## Variable naming
#'
#' Variable names follow the
#' [tidyverse style guide](https://style.tidyverse.org/),
#' using lowercase letters and underscores to ensure clarity and consistency.
#'
#' ## Variable classes
#'
#' R supports a [wide variety](https://cran.r-project.org/view=TimeSeries) of
#' time series data formats. We use [`tsibble`][tsibble::tsibble()] as it aligns
#' well with [tidyverse
#' principles](https://tidyverse.tidyverse.org/articles/manifesto.html) and
#' offers a modern, consistent approach to time series data.
#'
#' To facilitate interoperability with other standards, all variables are stored
#' as [`numeric`][numeric()] vectors except for the index, which is of class
#' [`POSIXct`][as.POSIXct()]. This design choice ensures that data from
#' `actverse` can be easily converted to other formats if needed.
#'
#' @section Metadata:
#'
#' ## Subject
#'
#' - Age: `35`
#' - Gender: Male
#' - Height: `180` cm
#' - Weight: `85` kg
#' - BMI: `~26.235` (overweight)
#'
#' ## Record
#'
#' - Body part: Left wrist (non-dominant wrist)
#' - Start: `2021-04-24 04:14:25`
#' - End: `2021-05-31 03:39:06`
#' - Days: `37`
#' - Epoch (sample interval): `60` seconds
#' - Mode: PIM, TAT, and ZCM
#' - TAT Threshold: `1024`
#' - Location: São Paulo, SP, Brazil
#' - UTC: `-3`
#' - DST: `FALSE`
#' - Light phase: `06:23:59` - `17:44:58`
#' - Dark phase: `17:44:59` - `06:23:58`
#'
#' ## Actigraph
#'
#' - Brand: [Condor Instruments](https://condorinst.com/en/)
#' - Model: [ActTrust 1](https://condorinst.com/en/)
#' - Hardware version: `3.1`
#' - Firmware version: `3.7`
#' - Manual: `1.0.13` (PT-BR)
#' - Software: ActStudio Alpha 1.0.17 (Win | Mac)
#'
#' ## Scoring algorithm settings
#'
#' - Condor Instruments algorithm version 1.0.17.
#' - The sleep diary data was not used for scoring.
#'
#' ## Scoring review
#'
#' - Offwrist periods were indicated from sudden drops in wrist temperature.
#' * Micro sleep states (isolated states lasting less than `5 minutes`) were
#' removed.
#' - Minor adjustments were made on the sleep onset and sleep offset, taking
#' into consideration the event button and sleep diary records.
#'
#' ## Sleep statistics settings
#'
#' - Multiple main sleep periods: `TRUE`.
#' - Main sleep period time of day: `Day or Night`.
#' - Minimum duration for main sleep periods: `01:00:00`.
#' - Awake period limit: `02:00:00`.
#'
#' @format A [`tsibble`][tsibble::tsibble()] with 17 columns and 1,441 rows:
#'
#' \describe{
#'   \item{timestamp}{
#'   Date and time of the recorded events.
#'   \cr \cr
#'   R class: [`POSIXct`][as.POSIXct()].
#'   }
#'
#'   \item{pim}{
#'   Wrist activity in Proportional Integral Mode (PIM).
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{tat}{
#'   Wrist activity in Time Above Threshold (TAT) mode.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{zcm}{
#'   Wrist activity in Zero Crossing Mode (ZCM).
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{orientation}{
#'   Device orientation.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{wrist_temperature}{
#'   Wrist temperature in degrees `Celsius`.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{external_temperature}{
#'   External temperature in degrees `Celsius` (unreliable data).
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{light}{
#'   Light intensity in `lux`.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{ambient_light}{
#'   Ambient light intensity in `microwatts/square meter`.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{red_light}{
#'   Red light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{green_light}{
#'   Green light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{blue_light}{
#'   Blue light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{ir_light}{
#'   Infrared light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{uva_light}{
#'   Ultraviolet A light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{uvb_light}{
#'   Ultraviolet B light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{event}{
#'   A binary number indicating the moments when the subject pressed the event
#'   button: `0` when the button is not pressed and `1` when the button is
#'   pressed. This is commonly used to indicate when the subject goes to sleep
#'   or wakes up.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#'
#'   \item{state}{
#'   An integer number indicating the subject state: `0` for awake, `1` for
#'   sleeping, `2` for resting, `4` for offwrist (device removal), `6` for
#'   editable state 1, `7` for editable state 2, and `8` for editable state 3.
#'   These states can be assigned by a specialist or by a validated inference
#'   algorithm.
#'   \cr \cr
#'   R class: [`numeric`][numeric()].
#'   }
#' }
#'
#' @template references_d
#' @family datasets
#'
#' @source Created by [Daniel Vartanian](https://linktr.ee/danielvartan)
#'   (package author).
"acttrust"
