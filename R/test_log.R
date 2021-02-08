#' An actigraphy record
#'
#' @description
#'
#' An actigraphy record, free for all kinds of use, personal made, and granted
#' by one of the authors of the package.
#'
#' __Device__: [Condor Instruments](https://www.condorinst.com.br/) ActTrust 1
#'
#' @format A tibble with `r ncol(test_log)` columns and `r nrow(test_log)` rows:
#'
#' \describe{
#'   \item{timestamp}{
#'   Log timestamp.
#'   \cr \cr
#'   R class: `POSIXct`.}
#'
#'   \item{ms}{
#'   Log millisecond.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{x_axis}{
#'   X axis signal.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{y_axis}{
#'   Y axis signal.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{z_axis}{
#'   Z axis signal.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{pim}{
#'   Wrist activity in Proportional Integral Mode (PIM).
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{pim_n}{
#'   Wrist activity in normalized PIM value per second (PIM divided by epoch
#'   seconds).
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{tat}{
#'   Wrist activity in Time Above Threshold (TAT) mode.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{tat_n}{
#'   Wrist activity in normalized TAT value per second (TAT divided by epoch
#'   seconds).
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{zcm}{
#'   Wrist activity in Zero Crossing Mode (`ZCM`).
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{zcm_n}{
#'   Wrist activity in normalized ZCM value per second (ZCM divided by epoch
#'   seconds).
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{orientation}{
#'   Device orientation.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{body_temperature}{
#'   Body temperature in degrees `Celsius` (wrist skin temperature).
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{external_temperature}{
#'   External temperature in degrees `Celsius`.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{light}{
#'   Light intensity in `lux`.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{ambient_light}{
#'   Ambient light intensity in `microwatts/square meter`.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{red_light}{
#'   Red light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{green_light}{
#'   Green light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{blue_light}{
#'   Blue light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{ir_light}{
#'   Infrared light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{uva_light}{
#'   Ultraviolet A light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{uvb_light}{
#'   Ultraviolet B light intensity in `microwatts/square centimeter`.
#'   \cr \cr
#'   R class: `numeric`.}
#'
#'   \item{event}{
#'   A `logical` value indicating whether there was an event in the sampling
#'   period.
#'   \cr \cr
#'   R class: `logical`.}
#'
#'   \item{state}{
#'   A `factor` value indicating the subject status: `"Awake"`, `"Resting"`,
#'   `"Sleeping"`, `"OffWrist"` (device removed) or editable states.
#'   \cr \cr
#'   R class: `factor`.}
#' }
#'
#' @source Prepared by Daniel Vartanian (package's author) (personal log).
#' @family datasets
#' @template references_a
"test_log"
