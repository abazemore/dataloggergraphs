# Parse files ----
#' parse_brand
#'
#' @description
#' A central function to route to the appropriate specific logger extraction function
#'
#' @param filepath Path to a directory of logfiles of the same brand
#' @param site The site name used for each row as a string for any brand other than Meaco
#' @param brand The logger brand in lowercase
#' @param model Optional, the model as a string for brands that do not include the model in the file. Not required for TinyTag
#'
#' @returns envdata, a standard format dataframe to use with other functions
#'
#' @export
#'
#' @examples parse_datalogger("/data/tinytag/2025-01-01_to_2025-02-01.csv", "Anonymous Library", "tinytag")
parse_brand <- function (filepath,
                         brand = FALSE,
                         site = "",
                         model = "",
                         location = "",
                         old_data = NULL) {
  message("Checking brand")
  if (is.character(filepath)) {
    if (stringr::str_detect(filepath, ".csv|.xls")) {
      dirlist <- list(filepath)
    }
    else {
      dirlist <- dir(filepath, full.names = TRUE)
    }
  }
  if (is.data.frame(filepath) && "datapath" %in% names(filepath)) {
    dirlist <- filepath$datapath
  }
  else {
    message("Could not read filepath")
  }

  if (brand %in% c(
    "hanwell",
    "meaco",
    "miniclima",
    "previous",
    "rotronic",
    "tandd",
    "tinytag",
    "trend"
  ))
  {
    datalist <-   switch(
      brand,
      "hanwell" = lapply(
        dirlist,
        parse_hanwell,
        site = site,
        location = location
      ),
      "meaco" = lapply(dirlist, parse_meaco),
      "miniclima" = lapply(
        dirlist,
        parse_miniclima,
        site = site,
        location = location
      ),
      "previous" = lapply(dirlist, parse_previous),
      "rotronic" = lapply(dirlist, parse_rotronic, site = site),
      "tandd" = lapply(dirlist, parse_tandd, site = site),
      "tinytag" = lapply(dirlist, parse_tinytag, site = site),
      "trend" = lapply(dirlist, parse_trend, site = site)
    )
  } else {
    stop("Brand not supported")
  }
  envdata <- combine_data(datalist, old_data = old_data)

  envdata
}

#' Parse Hanwell file
#'
#' @param filepath path to .csv logfile
#' @param site The site name used for each row
#' @param model Model of logger, not recoverable from file
#' @param location Location of logger, not recoverable from file
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#'
#' @noRd
parse_hanwell <- function(filepath,
                          site = "",
                          location = "",
                          model = "Hanwell") {
  message("Parsing as Hanwell")
  tryCatch({
    if (!stringr::str_detect(readr::read_lines(filepath, skip = 1, n_max = 1), "Database")) {
      message("Not a Hanwell file")
      return(NULL)
    }
    # Logger information not stored in file, try filename

    envdata <- readr::read_csv(
      filepath,
      col_names = c("datetime", "temp", "RH"),
      skip = 13,
      col_types = readr::cols()
    ) |>
      dplyr::mutate(
        site = as.character(site),
        location = location,
        datetime = lubridate::parse_date_time(datetime, orders = "HMS dmy"),
        temp = temp,
        RH = RH,
        model = model,
        serial = '',
        .keep = "none"
      ) |>
      dplyr::relocate(site, location, datetime, temp, RH, model, serial)

    if (all(is.na(envdata$datetime)) ||
        all(is.na(envdata[, 4])) || all(is.na(envdata[, 5]))) {
      warning("May not be Hanwell file")
    }
    envdata
  }, warning = function(w) {
    warning("Could not be read as Hanwell: ", w)
    return(NULL)
  }, error = function(e) {
    warning("Could not be read as Hanwell: ", e)
    return(NULL)
  })
}


#' Parse Meaco file
#'
#' @param filepath path to .csv logfile
#' @param site The site name used for each row
#' @param model Model of logger, not recoverable from file
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#'
#' @noRd
parse_meaco <- function(filepath,
                        site = "",
                        model = "Meaco") {
  message("Parsing as Meaco")
  tryCatch({
    # Extract first few rows containing logger information
    file_head <- readr::read_csv(
      filepath,
      col_names = FALSE,
      n_max = 1,
      col_types = readr::cols()
    )
    # Gingerbread uses different format
    if (stringr::str_detect(file_head$X1[1], " - ID: ")) {
      envdata <- readr::read_csv(filepath,
                                 skip = 1,
                                 col_types = readr::cols())
      if ("Temperature" %in% names(envdata)) {
        # Add site, location, model, and serial columns, parse as date/time
        envdata <- dplyr::mutate(
          envdata,
          site = as.character(
            stringr::str_extract(file_head$X1[1], "^[A-Za-z0-9 ]+(?= - )")
          ),
          location = as.character(
            stringr::str_extract(file_head$X1[1], "(?<= - ).*(?= - )")
          ),
          datetime = lubridate::parse_date_time(Timestamp, orders = c("dmy HM", "dmy HMS")),
          temp = as.numeric(Temperature),
          RH = as.numeric(Humidity),
          model = model,
          serial = as.character(stringr::str_extract(file_head$X1[1], "(?<=ID: ).*")),
          .keep = "none"
        ) |>
          dplyr::relocate(site, location, datetime, temp, RH, model, serial)
      } else {
        envdata <- dplyr::mutate(
          envdata,
          site = as.character(
            stringr::str_extract(file_head$X1[1], "^[A-Za-z0-9 ]+(?= - )")
          ),
          location = as.character(
            stringr::str_extract(file_head$X1[1], "(?<= - ).*(?= - )")
          ),
          datetime = lubridate::parse_date_time(Timestamp, orders = c("dmy HM", "dmy HMS")),
          lux = as.numeric(LUX),
          UV = as.numeric(UV),
          model = model,
          serial = stringr::str_extract(file_head$X1[1], "(?<=ID: ).*"),
          .keep = "none"
        ) |>
          dplyr::relocate(site, location, datetime, lux, UV, model, serial)
      }
    }
    # Pre-Gingerbread
    else if (ncol(file_head) == 5) {
      if (stringr::str_detect(file_head$X4[1], "TEMP")) {
        # Rest of file is observations
        envdata <- readr::read_csv(filepath, col_types = readr::cols()) |>

          # Add site, location, model, and serial columns, parse as date/time
          dplyr::mutate(
            site = as.character(RECEIVER),
            location = as.character(TRANSMITTER),
            datetime = as.POSIXct(DATE),
            temp = as.numeric(TEMPERATURE),
            RH = as.numeric(HUMIDITY),
            model = model,
            serial = "",
            .keep = "none"
          ) |>
          dplyr::relocate(site, location, datetime, temp, RH, model, serial)
      }
      else if (stringr::str_detect(file_head$X4[1], "LUX")) {
        envdata <- readr::read_csv(filepath, col_types = readr::cols()) |>

          # Add site, location, model, and serial columns, parse as date/time
          dplyr::mutate(
            site = as.character(RECEIVER),
            location = as.character(TRANSMITTER),
            datetime = lubridate::parse_date_time(DATE, orders = 'ymd HMS'),
            lux = as.numeric(LUX),
            UV = as.numeric(UV),
            model = model,
            serial = "Serial unknown",
            .keep = "none"
          ) |>
          dplyr::relocate(site, location, datetime, lux, UV, model, serial)
      } else {
        message("Could not be read as Meaco")
      }
    }


    if (all(is.na(envdata$datetime)) ||
        all(is.na(envdata[, 4])) || all(is.na(envdata[, 5]))) {
      warning("May not be Meaco file")
    }

    envdata
  }, warning = function(w) {
    warning("Could not be read as Meaco: ", w)
    return(NULL)
  }, error = function(e) {
    warning("Could not be read as Meaco: ", e)
    return(NULL)
  })
}

#' Parse miniClima file
#'
#' @param filepath .csv logfile.
#' @param site The site name used for each row.
#' @param model Model of logger, not recoverable from file.
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#'
#' @noRd
parse_miniclima <- function(filepath,
                            site = "",
                            model = "miniClima",
                            location = "Location unknown",
                            serial = "") {
  message("Parsing as miniClima")
  tryCatch({
    # Read observations
    envdata <- readr::read_csv2(
      filepath,
      col_names = c(
        "datetime",
        "temp",
        "RH",
        "setpoint",
        "alarm_min",
        "alarm_max",
        "timediff"
      ),
      skip = 1,
      col_types = readr::cols()
    )

    # Logger information not stored in file, try filename in pattern "Location EBC Master serial COM"
    if (location == "" && stringr::str_detect(filepath, " EBC")) {
      location <- stringr::str_extract(filepath, "[A-Za-z0-9 ]+(?= EBC)") |>
        tidyr::replace_na("Location unknown")
      serial <- stringr::str_extract(filepath, "(?<= Master ).*?(?= COM)") |>
        tidyr::replace_na("")
    }

    #Add site, location, model, and serial columns, parse as date/time
    envdata <- dplyr::mutate(
      envdata,
      site = as.character(site),
      location = location,
      datetime = lubridate::parse_date_time(stringr::str_replace(datetime, ",", " "), orders = "dmy HMS"),
      temp = temp,
      RH = RH,
      model = model,
      serial = serial,
      .keep = "none"
    ) |>
      dplyr::relocate(site, location, datetime, temp, RH, model, serial)

    if (all(is.na(envdata$datetime)) ||
        all(is.na(envdata[, 4])) || all(is.na(envdata[, 5]))) {
      warning("May not be miniClima file")
    }

    envdata
  }, warning = function(w) {
    warning("Could not be read as miniClima: ", w)
    return(NULL)
  }, error = function(e) {
    warning("Could not be read as miniClima: ", e)
    return(NULL)
  })
}


#' Parse previously processed file
#'
#' @param filepath .csv of envdata
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#'
#' @noRd
parse_previous <- function(filepath) {
  tryCatch({
    message("Parsing as previously processed")
    envdata <- readr::read_csv(filepath, col_types = readr::cols()) |>
      dplyr::mutate(datetime = as.POSIXct(datetime))


    if (all(is.na(envdata$datetime)) ||
        (all(is.na(envdata[, 4])) || all(is.na(envdata[, 5])))) {
      warning("May not be previously processed file")
    }

    envdata
  }, warning = function(w) {
    warning("Could not be read as previously processed: ", w)
    return(NULL)
  }, error = function(e) {
    warning("Could not be read as previously processed: ", e)
    return(NULL)
  })
}

#' Parse Rotronic file
#'
#' @param filepath .csv or .xls logfile.
#' @param site The site name used for each row.
#' @param model Model of logger, not recoverable from file.
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#'
#' @noRd
parse_rotronic <- function(filepath,
                           site = "",
                           model = "Rotronic") {
  message("Parsing as Rotronic")
  tryCatch({
    # Extract first few rows containing logger information
    # Check .xls or .csv
    if (stringr::str_detect(filepath, ".xls$")) {
      file_head <- readr::read_delim(
        filepath,
        delim = "\t",
        col_names = c("date"),
        n_max = 5,
        col_types = readr::cols()
      )
      file_data <- readr::read_delim(
        filepath,
        col_names = c("date", "time", "RH", "temp"),
        delim = "\t",
        skip = 23,
        col_types = readr::cols()
      )
    }

    if (stringr::str_detect(filepath, ".csv$")) {
      file_head <- readr::read_csv(
        filepath,
        col_names = c("date"),
        n_max = 5,
        col_types = readr::cols()
      )
      file_data <- readr::read_csv(
        filepath,
        col_names = c("date", "time", "RH", "temp"),
        skip = 23,
        col_types = readr::cols()
      )
    }

    # Rest of file is observations
    envdata <-  file_data |>
      # Add site, location, and serial columns extracted from first rows
      # Assuming "Device description" is the name of the logger and normally the location
      # Combine and parse datetime as POSIXct and extract numeric value of temp/RH
      # Model not recoverable
      dplyr::mutate(
        site = as.character(site),
        location = as.character(file_head$date[2]),
        datetime = lubridate::parse_date_time(paste(date, time), orders = "dmy HMS"),
        temp = as.numeric(temp),
        RH = as.numeric(RH),
        model = model,
        serial = stringr::str_remove(file_head$date[5], "Serial Number = "),
        .keep = "none"
      ) |>
      dplyr::relocate(site, location, datetime, temp, RH, model, serial)


    if (all(is.na(envdata$datetime)) ||
        all(is.na(envdata[, 4])) || all(is.na(envdata[, 5]))) {
      warning("May not Rotronic file")
    }

    envdata
  }, warning = function(w) {
    warning("Could not be read as Rotronic: ", w)
    return(NULL)
  }, error = function(e) {
    warning("Could not be read as Rotronic: ", e)
    return(NULL)
  })
}

#' Parse T&D file
#'
#' @param filepath .csv logfile.
#' @param site The site name used for each row.
#' @param model Model of logger, not recoverable from file.
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#'
#' @noRd
parse_tandd <- function(filepath,
                        site = "",
                        model = "") {
  message("Parsing as T&D", model = "T&D")
  tryCatch({
    #Extract first few rows containing logger information
    file_head <- readr::read_csv(
      filepath,
      col_names = c("datetime", "time", "lux", "UV", "temp", "RH"),
      n_max = 3,
      col_types = readr::cols()
    )
    if (stringr::str_detect(filepath, "([A-Z0-9]){8}")) {
      location <- stringr::str_extract(filepath, "(?<=/).*?(?= ([A-Z0-9]){8})")
      serial = stringr::str_extract(filepath, "([A-Z0-9]){8}")
    }
    else{
      serial <- ""
      location <- "Location unknown"
    }
    # Rest of file is observations
    envdata <- readr::read_csv(
      filepath,
      col_names = c(
        "datetime",
        "time",
        "lux",
        "UV",
        "temp",
        "RH",
        "luxhours",
        "UVhours"
      ),
      col_types = "cccccccc",
      skip = 3
    ) |>
      # Add site, location, and serial columns extracted from first rows
      # Assuming "Device description" is the name of the logger and normally the location, ours were not set up correctly
      # Combine and parse datetime as POSIXct and extract numeric value of temp/RH
      # Model not recoverable
      dplyr::mutate(
        site = as.character(site),
        location = as.character(location),
        datetime = lubridate::parse_date_time(datetime, orders = 'ymd HMS'),
        temp = as.numeric(temp),
        RH = as.numeric(RH),
        lux = as.numeric(lux),
        UV = as.numeric(UV),
        model = model,
        serial = as.character(serial),
        .keep = "none"
      )  |>
      dplyr::relocate(site, location, datetime, temp, RH, lux, UV, model, serial)


    if (all(is.na(envdata$datetime)) ||
        all(is.na(envdata[, 4])) || all(is.na(envdata[, 5]))) {
      warning("May not be T&D file")
    }

    envdata
  }, warning = function(w) {
    warning("Could not be read as T&D: ", w)
    return(NULL)
  }, error = function(e) {
    warning("Could not be read as T&D: ", e)
    return(NULL)
  })
}

#' parse_tinytag
#'
#' @param filepath .csv logfile exported from TinyTag Explorer
#' @param site The site name used for each row, string
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#'
#' @noRd
parse_tinytag <- function(filepath, site = "") {
  message("Parsing as Tinytag")
  tryCatch({
    file_head <- readr::read_csv(
      filepath,
      col_names = c("id", "datetime", "temp", "RH"),
      col_types = "cccc",
      n_max = 5
    )
    envdata <- readr::read_csv(
      filepath,
      col_names = c("id", "datetime", "temp", "RH"),
      skip = 5,
      col_types = readr::cols()
    ) |>
      # Add site, location, model, and serial columns extracted from first rows
      # Model does not work correctly for older files
      dplyr::mutate(
        site = as.character(site),
        location = as.character(file_head$temp[4]),
        datetime = lubridate::parse_date_time(datetime, orders = c("ymd HMS", "dmy HMS", "dmy HM")),
        temp = as.numeric(
          stringr::str_extract_all(temp, "[:digit:]+\\.[:digit:]+")
        ),
        RH = as.numeric(stringr::str_extract_all(RH, "[:digit:]+\\.[:digit:]+")),
        model = as.character(stringr::str_remove(file_head$temp[3], " H�C/%RH")),
        serial = as.character(file_head$temp[2]),
        .keep = "none"
      )  |>
      dplyr::relocate(site, location, datetime, temp, RH, model, serial)



    if (all(is.na(envdata$datetime)) ||
        all(is.na(envdata[, 4])) || all(is.na(envdata[, 5]))) {
      warning("May not be TinyTag file")
    }

    envdata
  }, warning = function(w) {
    warning("Could not be read as TinyTag: ", w)
    return(NULL)
  }, error = function(e) {
    warning("Could not be read as TinyTag: ", e)
    return(NULL)
  })
}


#' Parse Trend BMS file
#'
#' @param filepath .csv logfile.
#' @param site The site name used for each row.
#' @param model Model of logger, not recoverable from file.
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package. temp or RH will be NA.
#'
#' @noRd
parse_trend <- function(filepath,
                        site = "",
                        model = "Trend BMS", keep_number = TRUE) {
  message("Parsing as Trend BMS")
  tryCatch({
    #Extract first few rows containing logger information
    file_head <- readr::read_csv(
      filepath,
      col_names = c("datetime", "obs"),
      n_max = 1,
      col_types = readr::cols()
    )
    #Finds out whether the file is temperature or humidity
    is_temp <- stringr::str_detect(file_head$obs[1], "Temp")
    #Makes a variable for the column name for the observations
    temp_or_RH <- ifelse(is_temp == TRUE, "temp", "RH")
    #extract location from string, matches for example "Small Arch 7" up to " Space"
    #Formula is "(?<=(START)).*?(?=END)", where START and END are the characters before and after the location and logger number
    location_extract <- stringr::str_extract(file_head$obs[1], "(?<=\\[).*?(?= Space)")
    if (keep_number == TRUE) {
      logger_number <- stringr::str_extract(file_head$obs[1], "(?<=(Temp |Humidity )).*?(?=\\])") |>
        tidyr::replace_na("")
    }
    else {
      logger_number <- ""
    }
    #Rest of file is observations
    envdata <- readr::read_csv(
      filepath,
      col_names = c("datetime", temp_or_RH),
      skip = 1,
      col_types = readr::cols()
    ) |>
      #Add site, location, model, and serial columns, parse as date/time
      dplyr::mutate(
        site = as.character(site),
        location = paste(location_extract, logger_number),
        datetime = lubridate::parse_date_time(datetime, orders = c("dmy IM p", "dmy IMS p")),
        model = model,
        serial = "Trend BMS serial unknown"
      )  |>
      dplyr::relocate(site,
                      location,
                      datetime,
                      !!rlang::sym(temp_or_RH),
                      model,
                      serial)


    if (all(is.na(envdata$datetime))) {
      warning("May not be Trend file")
    }

    envdata
  }, warning = function(w) {
    warning("Could not be read as Trend: ", w)
    return(NULL)
  }, error = function(e) {
    warning("Could not be read as Trend: ", e)
    return(NULL)
  })
}
