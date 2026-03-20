
# Summary by location

#' Summarise environmental data with maximum, minimum, range, median, and average temperature and RH. Optional 1-99th percentile.
#'
#' @param envdata A dataframe returned from parse_datalogger
#' @param exclude_stores A string or list of stores to exclude from analysis
#' @param start_date A string to use as minimum date
#' @param end_date A string to use as maximum date
#' @param type 'annual', 'monthly', or 'daily'
#'
#' @returns site_summary A dataframe of summary statistics
#'
#' @examples dplyr::summarise_site(envdata, start_date = '2024-01-01', type = 'annual')
summarise_site <- function(envdata, exclude_stores = FALSE,
                           start_date = FALSE, end_date = FALSE, type = 'monthly', percentile = FALSE) {
  message('Summarising site')
  # Drop missing values and filter data
  subset <- subset_readings(envdata, exclude_stores = exclude_stores,
                            start_date = start_date, end_date = end_date)

  # Group by location and date to appropriate level
  if(type == 'all'| type == 'annual') {
    subset <- dplyr::group_by(subset, site, location, year = year(datetime))
  }
  if(type == 'monthly') {
    subset <- dplyr::group_by(subset, site, location,
                       year = year(datetime), month = month(datetime))
  }
  if(type == 'daily') {
    subset <- dplyr::group_by(subset, site, location,
                       year = year(datetime), month = month(datetime), day = day(datetime))
  }

  # If monitor type includes light data, include lux and UV columns
  site_summary <- subset |>
    dplyr::summarise(min_temp = min(temp, na.rm = TRUE),
              max_temp = max(temp, na.rm = TRUE),
              mean_temp = round(mean(temp, na.rm = TRUE), 1),
              p01_temp = round(quantile(temp, 0.01, na.rm = TRUE), 1),
              p99_temp = round(quantile(temp, 0.99, na.rm = TRUE), 1),
              range_temp = max_temp - min_temp,
              range_trim_temp = p99_temp - p01_temp,
              min_RH = min(RH, na.rm = TRUE),
              max_RH = max(RH, na.rm = TRUE),
              mean_RH = round(mean(RH, na.rm = TRUE), 1),
              range_RH = max_RH - min_RH,
              range_trim_RH = p99_RH - p01_RH)

  if (percentile == TRUE) {
    site_summary <- dplyr::summarise(
      p01_temp = round(quantile(temp, 0.01, na.rm = TRUE), 1),
      p99_temp = round(quantile(temp, 0.99, na.rm = TRUE), 1),
      .before = range_temp,
      range_trim_temp = p99_temp - p01_temp,
      .before = min_RH,
      p01_RH = round(quantile(RH, 0.01, na.rm = TRUE), 1),
      p99_RH = round(quantile(RH, 0.99, na.rm = TRUE), 1),
      .before = range_RH)
  }
  if(!all(is.na(subset$lux))) {
    light_data <- dplyr::summarise(subset,
                            min_lux = min(lux, na.rm = TRUE),
                            max_lux = max(lux, na.rm = TRUE),
                            mean_lux = round(mean(lux, na.rm = TRUE), 1),
                            p01_lux = round(quantile(lux, 0.01, na.rm = TRUE), 1),
                            p99_lux = round(quantile(lux, 0.99, na.rm = TRUE), 1),
                            range_trim_lux = p99_lux - p01_lux,
                            min_UV = min(UV, na.rm = TRUE),
                            max_UV = max(UV, na.rm = TRUE),
                            mean_UV = round(mean(UV, na.rm = TRUE), 1)
                            )
    if (percentile == TRUE) {
      light_data <- dplyr::summarise(
        p01_lux = round(quantile(lux, 0.01, na.rm = TRUE), 1),
        p99_lux = round(quantile(lux, 0.99, na.rm = TRUE), 1),
        range_trim_lux = p99_lux - p01_lux,
        .before = min_UV,
        p01_UV = round(quantile(UV, 0.01, na.rm = TRUE), 1),
        p99_UV = round(quantile(UV, 0.99, na.rm = TRUE), 1),
        range_trim_UV = p99_UV - p01_UV
      )
    }
    site_summary <- dplyr::bind_cols(site_summary, light_data)
  }


  return(site_summary)
}

#' Calculate light dose and compare to blue wool standards
#'
#' @param envdata A dataframe returned from parse_datalogger
#' @param start_date A string to use as a minimum date
#' @param end_date A string to use as a maximum date
#' @param obs_hour Number of observations logged per hour
#'
#' @returns dose A dataframe of the cumulative lux and UV hours with percent to just noticeable fade for BW standards
#'
#' @examples light_dose(envdata_light, obs_hour = 4)
light_dose <- function(envdata, start_date = FALSE,
                       end_date = FALSE, obs_hour = 3) {
  subset <- subset_readings(envdata,
                            start_date = start_date, end_date = end_date)
  # Summarize light exposure including just noticeable fade thresholds for blue wool standards
  dose <- subset |>
    dplyr::group_by(site, location) |>
    dplyr::summarise(start_period = min(subset$datetime, na.rm = TRUE),
              end_period = max(subset$datetime, na.rm = TRUE),
              luxhours = sum(lux, na.rm = TRUE) / obs_hour,
              UVhours = sum(UV, na.rm = TRUE) / obs_hour,
              BW1 = sum(lux, na.rm = TRUE) / obs_hour / 3000,
              BW2 = sum(lux, na.rm = TRUE) / obs_hour / 10000,
              BW3 = sum(lux, na.rm = TRUE) / obs_hour / 30000,
              BW4 = sum(lux, na.rm = TRUE) / obs_hour / 100000,
              BW5 = sum(lux, na.rm = TRUE) / obs_hour / 300000,
              BW6 = sum(lux, na.rm = TRUE) / obs_hour / 1000000,
              BW7 = sum(lux, na.rm = TRUE) / obs_hour / 3000000,
              BW8 = sum(lux, na.rm = TRUE) / obs_hour / 10000000)
  return(dose)
}

#' Set minimum and maximum temp with standard or custom values
#'
#' @param standard Standard ('BS 4971', 'PAS 198', 'Icon', or 'Bizot')
#' @param min_temp Minimum temperature, overwrites standard if provided
#' @param max_temp Maximum temperature, overwrites standard if provided
#' @param min_RH Minimum RH, overwrites standard if provided
#' @param max_RH Maximum RH, overwrites standard if provided
#'
#' @returns minmax A vector of minimum and maximum values for temp and RH
#' @examples set_minmax(standard = 'PAS 198', max_temp = 25)
set_minmax <- function(standard = 'BS 4971', min_temp = FALSE, max_temp = FALSE,
                       min_RH = FALSE, max_RH = FALSE) {
  # Set min and max if not provided
  if (!min_temp) {
    min_temp <- switch(
      standard,
      'BS 4971' = 13,
      'PAS 198' = 5,
      'Icon' = 5,
      'Bizot' = 16
    )
  }


  if (!max_temp) {
    max_temp <- switch(
      standard,
      'BS 4971' = 23,
      'PAS 198' = 25,
      'Icon' = 23,
      'Bizot' = 25
    )
  }

  if (!min_RH) {
    min_RH <- switch(
      standard,
      'BS 4971' = 35,
      'PAS 198' = 30,
      'Icon' = 35,
      'Bizot' = 40
    )
  }

  if (!max_RH) {
    max_RH <- switch(
      standard,
      'BS 4971' = 60,
      'PAS 198' = 65,
      'Icon' = 65,
      'Bizot' = 60
    )
  }

  return(c(min_temp, max_temp, min_RH, max_RH))
}

#' Calculate standard compliance for each area
#'
#' @param envdata A dataframe returned from parse_datalogger.
#' @param start_date A string to use as a minimum date.
#' @param end_date A string to use as a maximum date.
#' @param standard Standard ('BS 4971', 'PAS 198', 'Icon', or 'Bizot').
#' @param min_temp Minimum temperature, overwrites standard if provided.
#' @param max_temp Maximum temperature, overwrites standard if provided.
#' @param min_RH Minimum RH, overwrites standard if provided.
#' @param max_RH Maximum RH, overwrites standard if provided.
#'
#' @returns rated A dataframe with overall standard compliance, percentage of time above and below, and mean temp and RH.
#'
#' @examples compliance(envdata, standard = 'Icon')
compliance <- function(envdata, exclude_stores = FALSE,
                       start_date = FALSE, end_date = FALSE,
                       standard = 'BS 4971', min_temp = FALSE, max_temp = FALSE,
                       min_RH = FALSE, max_RH = FALSE) {
  message('Calculating compliance')
  subset <- subset_readings(envdata, exclude_stores = exclude_stores,
                            start_date = start_date, end_date = end_date)

  start_date <- min(subset$datetime)

  end_date <- max(subset$datetime)

  minmax <- set_minmax(standard, min_temp, max_temp, min_RH, max_RH)
  rated <- subset |>
    dplyr::group_by(site, location)  |>
    dplyr::summarise(
      start_period = min(datetime),
      end_period = max(datetime),
      standard = mean(temp >= minmax[1] &
                        temp <= minmax[2] &
                        RH >= minmax[3] &
                        RH <= minmax[4], na.rm = TRUE),
      temp_mean = mean(temp, na.rm = TRUE),
      temp_low = mean(temp < minmax[1], na.rm = TRUE),
      temp_good = mean(temp >= minmax[1] &
                         temp <= minmax[2], na.rm = TRUE),
      temp_high = mean(temp > minmax[2], na.rm = TRUE),
      RH_low = mean(RH < minmax[3], na.rm = TRUE),
      RH_good = mean(RH >= minmax[3] &
                       RH <= minmax[4], na.rm = TRUE),
      RH_high = mean(RH > minmax[4], na.rm = TRUE),

    ) |>
    dplyr::pivot_longer(
      cols = starts_with(c('temp', 'RH', 'standard')),
      names_to = 'rating',
      values_to = 'value',
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      rating = fct_relevel(
        rating,
        'RH_high',
        'RH_good',
        'RH_low',
        'temp_high',
        'temp_good',
        'temp_low',
        'temp_mean',
        'standard'
      )
    )
  return(rated)
}
