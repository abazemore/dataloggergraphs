#' Sample environmental data
#'
#' A parsed dataframe containing one year of anonymized TRH data and three months of light
#' data in the standard format used by functions in this package.
#'
#' @format A dataframe with 16000 rows and 9 variables:
#' \describe{
#'  \item{site}{site name, such as the building}
#'  \item{location}{location or description of datalogger, such as the store or case; may not be recoverable from the file}
#'  \item{datetime}{date and time of observation in YYYY-mm-dd HH:MM:SS format}
#'  \item{temp}{temperature as a decimal}
#'  \item{RH}{RH as a decimal}
#'  \item{lux}{lux as a decimal}
#'  \item{UV}{UV as a decimal}
#'  \item{model}{model or brand of the datalogger}
#'  \item{serial}{serial of the datalogger, may not be recoverable from the file}
#' }
"sample_data"
