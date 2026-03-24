#' Combine data from lapply list and match separate temperature and humidity files
#'
#' @param datalist A list of parsed dataframes
#'
#' @returns envdata A dataframe containing all rows in `datalist`, with combined temperature and RH files from Trend
#'
#' @noRd
combine_data <- function(datalist, old_data = NULL) {
  message('Combining files')

  datalist <- append(list(old_data), datalist)
  datalist <- purrr::keep(datalist, is.data.frame)

  envdata <- dplyr::bind_rows(datalist) |> dplyr::distinct()

  # Join where temperature and humidity are in separate files
  if (nrow(dplyr::filter(envdata, grepl('Trend', envdata$model)))) {
    message('Merging T&RH files')
    envdata_notbms <- dplyr::filter(envdata, !grepl('Trend', envdata$model))
    envdata_bms <- dplyr::filter(envdata, grepl('Trend', envdata$model))
    with_temp <- dplyr::filter(envdata_bms, !is.na(envdata_bms$temp)) |>
      dplyr::select(-RH)
    with_RH <- dplyr::filter(envdata_bms, !is.na(envdata_bms$RH)) |>
      dplyr::select(-temp)
    envdata <- dplyr::full_join(with_temp, with_RH) |>
      dplyr::bind_rows(envdata_notbms)
  }
if ("temp" %in% names(envdata)) {
  if ("lux" %in% names(envdata)) {
    envdata <-  dplyr::mutate(
      envdata,
      site = site,
      location = location,
      datetime = datetime,
      temp = temp,
      RH = RH,
      lux = lux,
      UV = UV,
      model = model,
      serial = serial,
      .keep = 'none'
    ) |>
      dplyr::relocate(site, location, datetime, temp, RH, lux, UV, model, serial)
  } else {
    envdata <-  dplyr::mutate(
      envdata,
      site = site,
      location = location,
      datetime = datetime,
      temp = temp,
      RH = RH,
      model = model,
      serial = serial,
      .keep = 'none'
    ) |>
      dplyr::relocate(site, location, datetime, temp, RH, model, serial)
  }
} else {
  envdata <-  dplyr::mutate(
    envdata,
    site = site,
    location = location,
    datetime = datetime,
    lux = lux,
    UV = UV,
    model = model,
    serial = serial,
    .keep = 'none'
  ) |>
    dplyr::relocate(site, location, datetime, lux, UV, model, serial)
}
  # Drop missing data
  envdata <-  tidyr::drop_na(envdata, datetime)
  envdata

}


#' Remove potentially faulty readings outside range
#'
#' @param envdata A dataframe returned from parse_brand
#' @param min_temp A number for the minimum realistic temperature
#' @param max_temp A number for the maximum realistic temperature
#' @param min_RH A number for the minimum realistic RH
#' @param max_RH A number for the maximum realistic RH
#'
#' @returns subset A dataframe with rows out of range removed
#' @export
#' @examples remove_faulty(envdata, max_RH = 70)
remove_faulty <- function(envdata,
                          min_temp = -25,
                          max_temp = 35,
                          min_RH = 10,
                          max_RH = 90) {
  # Remove readings outside range but keep NA light readings
  subset <- dplyr::filter(envdata, (is.na(temp) |
                                      (between(
                                        temp, min_temp, max_temp
                                      ))
                                    &
                                      (is.na(RH) | between(RH, min_RH, max_RH))))
  message('Removed ', nrow(envdata) - nrow(subset), ' readings')
  subset
}

#' Subset readings
#'
#' @param envdata Dataframe returned from parse_brand
#' @param store Store name or pattern to match in the location
#' @param exclude_stores Store name or pattern in the location to filter out
#' @param start_date Date in "YYYY-mm-dd" format
#' @param end_date Date in "YYYY-mm-dd" format
#'
#' @returns subset, a filtered dataframe
#' @export
#'
#' @examples subset_readings(envdata, store = "C", start_date = "2024-01-01", end_date = "2024-12-31")
subset_readings <- function(envdata,
                            store = FALSE,
                            exclude_stores = FALSE,
                            start_date = FALSE,
                            end_date = FALSE) {
  subset <- dplyr::ungroup(envdata)
  if (store != FALSE) {
    subset <- dplyr::filter(subset, grepl(store, subset$location))
  }
  if (exclude_stores != FALSE) {
    subset <- dplyr::filter(subset, !grepl(exclude_stores, subset$location))

  }
  if (start_date != FALSE) {
    subset <- dplyr::filter(subset, subset$datetime >= start_date)
  }
  if (end_date != FALSE) {
    subset <- dplyr::filter(subset, subset$datetime <= end_date)
  }
  subset
}
