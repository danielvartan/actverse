#' An ActTrust actigraphy record
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' A small sample (1 day length) of an
#' [ActTrust](https://condorinst.com.br/acttrust-actigrafo/)
#' actigraphy record.
#'
#' __Note__: We're always looking for actigraphy data samples. If you have a
#' personal actigraphy record made with a device that is not listed in the
#' `actverse` package and is ok with sharing it, please contact the package
#' authors.
#'
#' ActTrust is a trademark of
#' [Condor Instruments Ltda](https://condorinst.com.br/).
#'
#' @details
#'
#' This dataset was created by one of the `actverse` package authors and is free
#' for all kinds of use. You can download the full dataset at
#' \url{https://zenodo.org/record/4898822}.
#'
#' ## Data wrangling
#'
#' This data was tidied and validated with the default settings of
#' [`read_acttrust()`][actverse::read_acttrust()].
#'
#' The process of _tiding_ a dataset is understood as transforming it in input
#' data, like described in Loo and Jonge (2018). It's a very similar process of
#' tiding data described in the workflow proposed by Wickham and Grolemund
#' (n.d.).
#'
#' The process of _validating_ a dataset is understood as detecting invalid
#' data, by checking whether data satisfies certain assumptions from domain
#' knowledge, to then,  removing or, if possible, fixing them. This process can
#' be considered as part of the process of transforming data, described in the
#' workflow proposed by Wickham and Grolemund (n.d.).
#'
#' To learn more about the concept of tidy data, see Wickham (2014) and Wickham
#' and Grolemund (n.d.). You can find more about data validation and error
#' location in Loo and Jonge (2018).
#'
#' ## Variable naming
#'
#' The `actverse` package stick to the [tidyverse
#' principles](https://tidyverse.tidyverse.org/articles/manifesto.html). Our
#' code and naming schemes follow the guidelines of the [tidyverse style
#' guide](https://style.tidyverse.org/).
#'
#' ## Variable classes
#'
#' R has a [vast number](https://cran.r-project.org/view=TimeSeries) of time
#' series standards. We choose to work with [`tsibble`][tsibble::tsibble()]
#' because we think is the best standard that follows the [tidyverse
#' principles](https://tidyverse.tidyverse.org/articles/manifesto.html).
#'
#' To make sure that the resulting data from `actverse` could be easily
#' transformed to other standards, we decided to maintain all variable classes
#' as [`numeric`][numeric()], except for the index (class
#' [`POSIXct`][as.POSIXct()]).
#'
#' If you're looking for a good way to easily transform your data to other time
#' series standards, check the [`tsbox`](https://www.tsbox.help/) package.
#'
#' ## Raw data
#'
#' The raw data for this dataset can be found with
#' [`raw_data("acttrust.txt")`][raw_data()].
#'
#' @section Metadata:
#'
#' ## Subject
#'
#' * Age: `35`.
#' * Gender: Male.
#' * Height: `180` cm.
#' * Weight: `85` kg.
#' * BMI: `~26.235` (overweight).
#'
#' ## Record
#'
#' * Body part: Left wrist (non-dominant wrist).
#' * Start: `2021-04-24 04:14:25`.
#' * End: `2021-05-31 03:39:06`.
#' * Days: `37`
#' * Epoch (sample interval): `60` seconds.
#' * Mode: PIM, TAT, and ZCM.
#' * TAT Threshold: `1024`.
#' * Location: Sao Paulo, SP, Brazil.
#' * UTC: `-3`.
#' * DST: `FALSE`.
#' * Light phase: `06:23:59` - `17:44:58`.
#' * Dark phase: `17:44:59` - `06:23:58`.
#'
#' ## Actigraph
#'
#' * Brand: [Condor Instruments](https://condorinst.com.br/).
#' * Model: [ActTrust 1](https://condorinst.com.br/acttrust-actigrafo/).
#' * Hardware version: `3.1`.
#' * Firmware version: `3.7`.
#' * Manual: `1.0.13` (PT-BR).
#' * Software: ActStudio Alpha 1.0.17 (Win | Mac).
#'
#' ## Scoring settings
#'
#' * Scoring algorithm: Condor Instruments.
#' * The sleep diary data was not used for scoring.
#'
#' ## Scoring review
#'
#' * Offwrist periods were indicated from sudden drops in wrist temperature.
#' * Micro sleep states (isolated states lasting less than `5 minutes`) were
#' removed.
#' * Minor adjustments were made on the sleep onset and sleep offset, taking
#' into consideration the event button and sleep diary records.
#'
#' ## Sleep statistics settings
#'
#' * Multiple main sleep periods: `TRUE`.
#' * Main sleep period time of day: `Day or Night`.
#' * Minimum duration for main sleep periods: `01:00:00`.
#' * Awake period limit: `02:00:00`.
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
#' @source Created by Daniel Vartanian (package author).
#' @family datasets
#' @template references_b
"acttrust"
