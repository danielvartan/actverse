#' An actimetric record
#'
#' @description
#'
#' An actimetric record, free for all kinds of use, personal made, and granted
#' by one of the authors of the package.
#'
#' __Device__: [Condor Instruments](https://www.condorinst.com.br/) ActTrust 1
#'
#' @format A data frame with 10074 rows and 24 variables:
#'
#' \describe{
#'   \item{__timestamp__ (`POSIXct`)}{
#'   Log timestamp}
#'
#'   \item{__ms__ (`numeric`)}{
#'   Log millisecond}
#'
#'   \item{__x_axis__ (`numeric`)}{
#'   X axis signal}
#'
#'   \item{__y_axis__ (`numeric`)}{
#'   Y axis signal}
#'
#'   \item{__z_axis__ (`numeric`)}{
#'   Z axis signal}
#'
#'   \item{__pim__ (`numeric`)}{
#'   Wrist activity in Proportional Integral Mode (PIM)}
#'
#'   \item{__pim_n__ (`numeric`)}{
#'   Wrist activity in normalized PIM value per second (PIM divided by epoch
#'   seconds)}
#'
#'   \item{__tat__ (`numeric`)}{
#'   Wrist activity in Time Above Threshold (TAT) mode}
#'
#'   \item{__tat_n__ (`numeric`)}{
#'   Wrist activity in normalized TAT value per second (TAT divided by epoch
#'   seconds)}
#'
#'   \item{__zcm__ (`numeric`)}{
#'   Wrist activity in Zero Crossing Mode (ZCM)}
#'
#'   \item{__zcm_n__ (`numeric`)}{
#'   Wrist activity in normalized ZCM value per second (ZCM divided by epoch
#'   seconds)}
#'
#'   \item{__orientation__ (`numeric`)}{
#'   Device orientation}
#'
#'   \item{__body_temperature__ (`numeric`)}{
#'   Body temperature in degrees Celsius (wrist skin temperature)}
#'
#'   \item{__external_temperature__ (`numeric`)}{
#'   External temperature in degrees Celsius}
#'
#'   \item{__light__ (`numeric`)}{
#'   Light intensity in lux}
#'
#'   \item{__ambient_light__ (`numeric`)}
#'   {Ambient light intensity in microwatts/square meter}
#'
#'   \item{__red_light__ (`numeric`)}{
#'   Red light intensity in microwatts/square centimeter}
#'
#'   \item{__green_light__ (`numeric`)}{
#'   Green light intensity in microwatts/square centimeter}
#'
#'   \item{__blue_light__ (`numeric`)}{
#'   Blue light intensity in microwatts/square centimeter}
#'
#'   \item{__ir_light__ (`numeric`)}{
#'   Infrared light intensity in microwatts/square centimeter}
#'
#'   \item{__uva_light__ (`numeric`)}{
#'   Ultraviolet A light intensity in microwatts/square centimeter}
#'
#'   \item{__uvb_light__ (`numeric`)}{
#'   Ultraviolet B light intensity in microwatts/square centimeter}
#'
#'   \item{__event__ (`logical`)}{
#'   A logical value indicating whether there was an event in the sampling
#'   period}
#'
#'   \item{__state__ (`factor`)}{
#'   A factor value indicating the subject status: `"Awake"`, `"Resting"`,
#'   `"Sleeping"`, `"OffWrist"` (device removed) or editable states}
#' }
#'
#' @usage data(test_log)
#' @source Prepared by Daniel Vartanian (package's author) (personal log).
#'
#' @references
#'
#' __UNDER DEVELOPMENT__
"test_log"
