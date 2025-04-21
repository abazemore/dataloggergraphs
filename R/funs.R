library(tidyverse)
library(lubridate)
library(readxl)


# Parse files ----
#' parse_datalogger
#' 
#' @description
#' A central function to use with lapply, to route to the appropriate specific logger extraction function
#'
#' @param datafile A logger file, .csv or .xls only for Rotronic.
#' @param site The site name used for each row as a string.
#' @param brand The logger brand as a string.
#' @param model The model as a string for brands that do not include the model in the file. Not required for Tinytag
#'
#' @returns envdata, a standard format dataframe to use with other functions
#'
#' @examples parse_datalogger('/data/tinytag/2025-01-01_to_2025-02-01.csv', 'Anonymous Library', 'tinytag')
parse_datalogger <- function (datafile, site = '', brand = FALSE) {
  message('Checking brand')
  if(brand == 'tinytag') { envdata <- parse_tinytag(datafile, site) }
  if(brand == 'rotronic') { envdata <- parse_rotronic(datafile, site) }
  if(str_detect(brand, 'trend')) { envdata <- parse_trendBMS(datafile, site) }
  if(brand == 'tandd') { envdata <- parse_TandD(datafile, site) }
  if(brand == 'miniclima') { envdata <- parse_miniClima(datafile, site) }
  if(brand == 'meaco') { envdata <- parse_meaco(datafile, site) }
  if(brand == 'previous') { envdata <- parse_previous(datafile, site) }
  if(!brand %in% c('tinytag', 'rotronic', 'trendbms', 'bms', 'tandd', 'meaco', 'miniprevious')) { message('Brand not identified') }
  return(envdata)
}

#' parse_tinytag
#'
#' @param datafile .csv logfile exported from TinyTag Explorer
#' @param site The site name used for each row, string
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#' 
#' @noRd
parse_tinytag <- function(datafile, site = '') {
  message('Parsing as Tinytag')
  file_head <- read_csv(datafile, 
                        col_names = c('id', 'datetime', 'temp', 'RH'),
                        col_types = 'cccc', n_max = 5)
  envdata <- read_csv(datafile, 
                      col_names = c('id', 'datetime', 'temp', 'RH'),
                      skip = 5) %>%
    # Add site, location, model, and serial columns extracted from first rows
    # Model does not work correctly for older files
    mutate(site = as.character(site),
           location = as.character(file_head$temp[4]),
           datetime = as.POSIXct(datetime, tryFormats = c('%Y-%m-%d %H:%M:%S', '%d/%m/%Y %H:%M:%S', '%d/%m/%Y %H:%M')),
           temp = as.numeric(str_extract_all(temp, '[:digit:]+\\.[:digit:]+')),
           RH = as.numeric(str_extract_all(RH, '[:digit:]+\\.[:digit:]+')),
           lux = NA,
           UV = NA,
           model = as.character(str_remove(file_head$temp[3], ' H�C/%RH')),
           serial = as.character(file_head$temp[2]),
           .keep = 'none')
  return(envdata)
}


#' Parse Rotronic file
#'
#' @param datafile .csv or .xls logfile.
#' @param site The site name used for each row.
#' @param model Model of logger, not recoverable from file.
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#' 
#' @noRd
parse_rotronic <- function(datafile, site = '', model = 'HL-1D') {
  message('Parsing as Rotronic')
  # Extract first few rows containing logger information
  # Check .xls or .csv
  if(str_detect(datafile, '.xls$')) {
  file_head <- read_delim(datafile, delim = '\t', col_names = c('date'), 5)
  file_data <- read_delim(datafile, col_names = c('date', 'time', 'RH', 'temp'), delim = '\t', skip = 23)
  }
  
  if(str_detect(datafile, '.csv$')) {
    file_head <- read_csv(datafile, col_names = c('date'), n_max = 5)
  file_data <- read_csv(datafile, col_names = c('date', 'time', 'RH', 'temp'), skip = 23)
  }
  
  # Rest of file is observations
  envdata <-  file_data %>%
    # Add site, location, and serial columns extracted from first rows
    # Assuming 'Device description' is the name of the logger and normally the location
    # Combine and parse datetime as POSIXct and extract numeric value of temp/RH
    # Model not recoverable
    mutate(site = as.character(site),
           location = as.character(file_head$date[2]),
           datetime = as.POSIXct(paste(date, time), format='%d/%m/%Y %H:%M:%S'),
           temp = as.numeric(str_extract_all(temp, '[:digit:]+\\.[:digit:]+')),
           RH = as.numeric(str_extract_all(RH, '[:digit:]+\\.[:digit:]+')),
           lux = NA,
           UV = NA,
           model = model,
           serial = str_remove(file_head$date[5],'Serial Number = '),
           .keep = 'none')
  return(envdata)
}

#' Parse Trend BMS file
#'
#' @param datafile .csv logfile.
#' @param site The site name used for each row.
#' @param model Model of logger, not recoverable from file.
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package. Temp or RH will be NA.
#'
#' @noRd
parse_trendBMS <- function(datafile, site = '', model = 'Trend BMS') {
  message('Parsing as Trend BMS')
  #Extract first few rows containing logger information
  file_head <- read_csv(datafile,
                        col_names = c('datetime', 'obs'), n_max = 1)
  #Finds out whether the file is temperature or humidity
  is_temp <- str_detect(file_head$obs[1], 'Temp')
  #Makes a variable for the column name for the observations
  temp_or_RH <- if_else(is_temp == TRUE, 'temp', 'RH')
  #extract location from string, matches for example 'Small Arch 7' up to ' Space'
  #Formula is '(?<=(START)).*?(?=END)', where START and END are the characters before and after the location and logger number
  location_extract <- str_extract(file_head$obs[1],
                                  '(?<=\\[).*?(?= Space)')
  logger_number <- str_extract(file_head$obs[1],
                               '(?<=(Temp |Humidity )).*?(?=\\])') %>%
    replace_na('')
  #Rest of file is observations
  envdata <- read_csv(datafile,
                      col_names = c('datetime', temp_or_RH),
                      skip = 1) %>%
    #Add site, location, model, and serial columns, parse as date/time
    mutate(
      site = as.character(site),
      location = paste(location_extract, logger_number),
      datetime = parse_datetime(datetime, format = '%d-%b-%y %I:%M %p%*'),
      lux = NA,
      UV = NA,
      model = model,
      serial = 'Trend BMS serial unknown')
  return(envdata)
}

#' Parse T&D file
#'
#' @param datafile .csv logfile.
#' @param site The site name used for each row.
#' @param model Model of logger, not recoverable from file.
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#' 
#' @noRd
parse_TandD <- function(datafile, site = '') {
  message('Parsing as T&D', model = 'TR-74Ui')
  #Extract first few rows containing logger information
  file_head <- read_csv(datafile, 
                        col_names = c('datetime', 'time', 'lux', 'UV',
                                      'temp', 'RH'), n_max = 3)
  if(datafile != '0.csv') {
    location <- str_extract(datafile, '([A-Za-z0-9 ])+(?= F8)')
    serial = str_extract(datafile, '(F80)[A-Z0-9]+(?= )')
  }
  else{
    if(file_head$datetime[1] != 'Date/Time') { location <- file_head$datetime[1] }
    if(file_head$time[1] != 'Date/Time') { serial <- file_head$time[1] }
    else { serial <- 'Serial unknown'
    location <- 'Location unknown' }
  }
  # Rest of file is observations
  envdata <- read_csv(datafile, 
                      col_names = c('datetime', 'time', 'lux', 'UV',
                                    'temp', 'RH', 'luxhours', 'UVhours'),
                      col_types = 'cccccccc',
                      skip = 3) %>%
    # Add site, location, and serial columns extracted from first rows
    # Assuming 'Device description' is the name of the logger and normally the location, ours were not set up correctly
    # Combine and parse datetime as POSIXct and extract numeric value of temp/RH
    # Model not recoverable
    mutate(site = as.character(site),
           location = as.character(location),
           datetime = as.POSIXct(datetime),
           temp = as.numeric(temp),
           RH = as.numeric(RH),
           lux = as.numeric(lux),
           UV = as.numeric(UV),
           model = model,
           serial = as.character(serial),
           .keep = 'none')
  return(envdata)
}

#' Parse miniClima file
#'
#' @param datafile .csv logfile.
#' @param site The site name used for each row.
#' @param model Model of logger, not recoverable from file.
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#' 
#' @noRd
parse_miniClima <- function(datafile, site = '', model = 'miniClima EBC') {
  message('Parsing as miniClima')
  # Read observations
  envdata <- read_csv2(datafile,
                       col_names = c('datetime', 'temp', 'RH',
                                     'setpoint', 'alarm_min', 'alarm_max', 'timediff'),
                       skip = 1)
  # Read first line potentially containing logger information
  first_line <- read_csv2(datafile, 
                          col_names = c('datetime', 'temp', 'RH',
                                        'setpoint', 'alarm_min', 'alarm_max', 'timediff'),
                          n_max = 1)
  # Logger information not stored in file, try filename or first line
  if(str_detect(datafile, ' EBC')) {
    info <- datafile
  } else if(str_detect(first_line$datetime[1], ' EBC')) {
    info <- first_line$envdata$datetime[1]
  } else { info <- '' }
  # Extract location and serial
  location <- str_extract(info, '[A-Za-z0-9 ]+(?= EBC)') %>%
    str_replace_na('Location unknown')
  serial <- str_extract(info, '(?<= Master ).*?(?= COM)') %>%
    str_replace_na('Serial unknown')
  
  #Add site, location, model, and serial columns, parse as date/time
  envdata <- mutate(
    site = as.character(site),
    location = location,
    datetime = parse_datetime(datetime, format = '%d.%m.%y,%H:%M:%S'),
    temp = temp,
    RH = RH,
    lux = NA,
    UV = NA,
    model = model,
    serial = serial,
    .keep = 'none')
  return(envdata)
}

#' Parse Meaco file
#'
#' @param datafile .csv logfile.
#' @param site The site name used for each row.
#' @param model Model of logger, not recoverable from file.
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#'
#' @noRd
parse_meaco <- function(datafile, site = '', model = 'Meaco') {
  message('Parsing as Meaco')
  # Extract first few rows containing logger information
  file_head <- read_csv(datafile, col_names = FALSE,
                        n_max = 1)
  # Finds out whether the file is temperature or humidity
  if(str_detect(file_head$X4[1], 'TEMP')) {
    # Rest of file is observations
    envdata <- read_csv(datafile) %>%
      
      # Add site, location, model, and serial columns, parse as date/time
      mutate(
        site = as.character(RECEIVER),
        location = as.character(TRANSMITTER),
        datetime = DATE,
        temp = as.numeric(TEMPERATURE),
        RH = as.numeric(HUMIDITY),
        lux = NA,
        UV = NA,
        model = model,
        serial = 'Serial unknown',
        .keep = 'none')
  }
  else {
    envdata <- read_csv(datafile) %>%
      
      # Add site, location, model, and serial columns, parse as date/time
      mutate(
        site = as.character(RECEIVER),
        location = as.character(TRANSMITTER),
        datetime = DATE,
        temp = NA,
        RH = NA,
        lux = as.numeric(LUX),
        UV = as.numeric(UV),
        model = model,
        serial = 'Serial unknown',
        .keep = 'none')
  }
  return(envdata)
}

#' Parse previously processed file
#'
#' @param datafile .csv of envdata
#'
#' @returns envdata, a dataframe in a standard format used by other functions in this package
#' 
#' @noRd
parse_previous <- function(datafile) {
  message('Parsing as previously processed')
  envdata <- read_csv(datafile) %>%
    mutate(datetime = as.POSIXct(datetime))
  return(envdata)
}

#' Combine data from lapply list and match separate temperature and humidity files
#'
#' @param datalist A list of parsed dataframes
#'
#' @returns envdata A dataframe containing all rows in `datalist`, with combined temperature and RH files from Trend
#' 
#' @noRd
combine_data <- function(datalist) {
  message('Combining files')
  # Combine data from each file into one list
  envdata <- bind_rows(datalist) %>% distinct()
  
  # Join where temperature and humidity are in separate files
  if(nrow(filter(envdata, grepl('Trend', model) & is.na(temp))) > 0) {
    message('Merging T&RH files')
    envdata_notbms <- filter(envdata, !grepl('Trend', model))
    envdata_bms <- filter(envdata, grepl('Trend', model))
    with_temp <- filter(envdata_bms, !is.na(envdata_bms$temp)) %>%
      select(-RH)
    with_RH <- filter(envdata_bms, !is.na(envdata_bms$RH)) %>%
      select(-temp)
    envdata <- full_join(with_temp, with_RH) %>%
      bind_rows(envdata_notbms)
  }
  # Drop missing data
  envdata <-  mutate(envdata, site = site,
                     location = location,
                     datetime = datetime,
                     temp = temp,
                     RH = RH,
                     lux = lux,
                     UV = UV,
                     model = model,
                     serial = serial,
                     .keep = 'none') %>%
    drop_na(any_of('datetime'))
  return(envdata)
}

# Subset readings ----
#' Remove potentially faulty readings outside range
#'
#' @param envdata A dataframe returned from parse_datalogger
#' @param min_temp A number for the minimum realistic temperature
#' @param max_temp A number for the maximum realistic temperature
#' @param min_RH A number for the minimum realistic RH
#' @param max_RH A number for the maximum realistic RH
#'
#' @returns subset A dataframe with rows out of range removed
#'
#' @examples remove_faulty(envdata, max_RH = 70)
remove_faulty <- function(envdata, min_temp = 5, max_temp = 35,
                          min_RH = 10, max_RH = 80) {
  # Remove readings outside range but keep NA light readings
  subset <- filter(envdata, (is.na(temp) | (between(temp, min_temp, max_temp))
                             & (is.na(RH) | between(RH, min_RH, max_RH))))
  message('Removed ', nrow(envdata) - nrow(subset), ' readings')
  return(subset)
}

subset_readings <- function(envdata, store = FALSE, exclude_stores = FALSE,
                            start_date = FALSE, end_date = FALSE) {
  subset <- envdata
  if(store != FALSE) { 
    subset <- filter(subset, grepl(store, subset$location))
  }
  if(exclude_stores != FALSE) {
    subset <- filter(subset, !grepl(exclude_stores, subset$location))
    
  }
  if(start_date != FALSE) {
    subset <- filter(subset, subset$datetime >= start_date)
  }
  if(end_date != FALSE) {
    subset <- filter(subset, subset$datetime <= end_date)
  }
  return(subset)
}

# Summarise -----
# Summary by location

#' Summarise site
#'
#' @param envdata A dataframe returned from parse_datalogger
#' @param exclude_stores A string or list of stores to exclude from analysis
#' @param start_date A string to use as minimum date
#' @param end_date A string to use as maximum date
#' @param type 'annual', 'monthly', or 'daily'
#'
#' @returns site_summary A dataframe of summary statistics
#'
#' @examples summarise_site(envdata, start_date = '2024-01-01', type = 'annual')
summarise_site <- function(envdata, exclude_stores = FALSE, 
                           start_date = FALSE, end_date = FALSE, type = 'monthly') {
  message('Summarising site')
  # Drop missing values and filter data
  subset <- subset_readings(envdata, exclude_stores = exclude_stores,
                             start_date = start_date, end_date = end_date)
  
  # Group by location and date to appropriate level
  if(type == 'all'| type == 'annual') {
    subset <- group_by(subset, site, location, year = year(datetime))
  }
  if(type == 'monthly') {
    subset <- group_by(subset, site, location, 
                        year = year(datetime), month = month(datetime)) 
  }
  if(type == 'daily') {
    subset <- group_by(subset, site, location, 
                        year = year(datetime), month = month(datetime), day = day(datetime))
  }
  
  # If monitor type includes light data, include lux and UV columns
  site_summary <- subset %>%
    summarise(min_temp = min(temp, na.rm = TRUE),
              max_temp = max(temp, na.rm = TRUE),
              mean_temp = round(mean(temp, na.rm = TRUE), 1),
              p01_temp = round(quantile(temp, 0.01, na.rm = TRUE), 1),
              p99_temp = round(quantile(temp, 0.99, na.rm = TRUE), 1),
              range_temp = max_temp - min_temp,
              range_trim_temp = p99_temp - p01_temp,
              min_RH = min(RH, na.rm = TRUE),
              max_RH = max(RH, na.rm = TRUE),
              mean_RH = round(mean(RH, na.rm = TRUE), 1),
              p01_RH = round(quantile(RH, 0.01, na.rm = TRUE), 1),
              p99_RH = round(quantile(RH, 0.99, na.rm = TRUE), 1),
              range_RH = max_RH - min_RH,
              range_trim_RH = p99_RH - p01_RH)
  
  if(!all(is.na(subset$lux))) {
    light_data <- summarise(subset,
                            min_lux = min(lux, na.rm = TRUE),
                            max_lux = max(lux, na.rm = TRUE),
                            mean_lux = round(mean(lux, na.rm = TRUE), 1),
                            p01_lux = round(quantile(lux, 0.01, na.rm = TRUE), 1),
                            p99_lux = round(quantile(lux, 0.99, na.rm = TRUE), 1),
                            range_trim_lux = p99_lux - p01_lux,
                            min_UV = min(UV, na.rm = TRUE),
                            max_UV = max(UV, na.rm = TRUE),
                            mean_UV = round(mean(UV, na.rm = TRUE), 1),
                            p01_UV = round(quantile(UV, 0.01, na.rm = TRUE), 1),
                            p99_UV = round(quantile(UV, 0.99, na.rm = TRUE), 1),
                            range_trim_UV = p99_UV - p01_UV)
    site_summary <- bind_cols(site_summary, light_data)
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
  dose <- subset %>%
    group_by(site, location) %>%
    summarise(start_period = min(subset$datetime, na.rm = TRUE),
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
      'PAS 198 25' = 30,
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
  rated <- subset %>%
    group_by(site, location)  %>%
    summarise(
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
      
    ) %>%
    pivot_longer(
      cols = starts_with(c('temp', 'RH', 'standard')),
      names_to = 'rating',
      values_to = 'value',
      values_drop_na = TRUE
    ) %>%
    mutate(
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

# Graph ----
dmy_style <- stamp('1 March 2021', orders = '%0d %B %Y')

#' Graph temperature and RH for single or multiple stores
#'
#' @param envdata A dataframe returned from parse_datalogger.
#' @param store A string matching a single store or a group of stores with a matching pattern.
#' @param exclude_stores A string or vector to exclude a group of stores.
#' @param graph_title Title of graph, else 'All stores at site' or first location if `store` specified.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param date_format Format of dates on x axis, like '%d/%m'.
#' @param breaks Breaks on x axis, like '2 weeks'.
#' @param standard Standard to use for temp/RH guidelines.
#' @param min_temp Minimum temperature guideline.
#' @param max_temp Maximum temperature guideline.
#' @param min_RH Minimum RH guideline.
#' @param max_RH Maximum RH guideline.
#' @param max_axis_temp Maximum of temperature axis.
#' @param max_axis_RH Maximum of RH axis.
#' @param col_temp Colour for temperature line, as named HTML colour or hex.
#' @param col_RH Colour for RH line, as named HTML colour or hex.
#'
#' @returns ggplot2 Object containing temperature and RH graph.
#' 
#' @examples graph_store(envdata_exhib, graph_title = 'Exhibition cases')
graph_store <- function(envdata, store = FALSE, exclude_stores = FALSE,
                        graph_title = FALSE,
                        start_date = FALSE, end_date = FALSE,
                        date_format = '%m/%Y', breaks = '2 months',
                        standard = 'BS 4971', min_temp = FALSE, max_temp = FALSE,
                        min_RH = FALSE, max_RH = FALSE, max_axis_temp = 40, max_axis_RH = 100,
                        col_temp = 'red', col_RH = 'blue') {
  message('Graphing T&RH')
  subset <- subset_readings(envdata, store = store, exclude_stores = exclude_stores,
                            start_date = start_date, end_date = end_date)
  
  start_date <- min(subset$datetime)
  end_date <- max(subset$datetime)
  minmax <- set_minmax(standard, min_temp, max_temp, min_RH, max_RH)
  
  # store is identifying part of location, does not have to match whole string
  if(store != FALSE) {
    message('Graphing single store')
    line_alpha <- 1
    if(graph_title == FALSE) {
    graph_title <- subset$location[1]
    }
  }
  
  if(store == FALSE) {
    message('Graphing all stores')
    line_alpha <- 0.5
    if(graph_title == FALSE) {
    graph_title <- paste('All stores at', subset$site[1])
    }
  }
  
  graph_subtitle <- paste(str_remove(dmy_style(min(subset$datetime)), '^0'),'to',
                          str_remove(dmy_style(max(subset$datetime)), '^0'))
  
  # Create graph
  # with time on x axis, temperature on left y axis, and RH on right y axis
  # Y scales 0-40º and 0-100% by default
  # Dotted lines indicate BS4971 storage guidelines
  return(subset %>% ggplot(mapping = aes(x = datetime, group = location)) +
           geom_hline(
             yintercept = minmax[1],
             color = col_temp,
             linetype = 'dotted',
             alpha = 0.8
           ) +
           geom_hline(
             yintercept = minmax[2],
             color = col_temp,
             linetype = 'dotted',
             alpha = 0.8
           ) +
           geom_hline(
             yintercept = minmax[3] / 2.5,
             color = col_RH,
             linetype = 'dotted',
             alpha = 0.6
           ) +
           geom_hline(
             yintercept = minmax[4] / 2.5,
             color = col_RH,
             linetype = 'dotted',
             alpha = 0.6
           ) +
           geom_line(aes(y = temp),
                     color = col_temp,
                     size = 0.25,
                     #linetype = linetype,
                     alpha = line_alpha) +
           geom_line(aes(y = RH / (max_axis_RH / max_axis_temp)),
                     color = col_RH,
                     size = 0.25,
                     #linetype = linetype,
                     alpha = line_alpha) +
           scale_x_datetime(name = 'Date',
                            date_breaks = breaks,
                            date_labels = date_format) +
           labs(title = graph_title, subtitle = graph_subtitle) +
           scale_y_continuous(
             name = 'Temperature (º C, red)',
             limits = c(0, max_axis_temp),
             sec.axis = sec_axis( ~ . * (max_axis_RH / max_axis_temp),
                                  name = 'Rel. Humidity (%, blue)',
                                  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))) +
           theme(axis.title.y.left = element_text(color = col_temp),
                 axis.title.y.right = element_text(color = col_RH))
  )
}

# Graph max/min/mean summary ----
#' Graph temperature and RH summary for single or multiple stores
#'
#' @param envdata A dataframe returned from parse_datalogger.
#' @param type 'annual', 'monthly', or 'daily'
#' @param store A string matching a single store or a group of stores with a matching pattern.
#' @param exclude_stores A string or vector to exclude a group of stores.
#' @param graph_title Title of graph, else 'All stores at site' or first location if `store` specified.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param date_format Format of dates on x axis, like '%d/%m'.
#' @param breaks Breaks on x axis, like '2 weeks'.
#' @param standard Standard to use for temp/RH guidelines.
#' @param min_temp Minimum temperature guideline.
#' @param max_temp Maximum temperature guideline.
#' @param min_RH Minimum RH guideline.
#' @param max_RH Maximum RH guideline.
#' @param max_axis_temp Maximum of temperature axis.
#' @param max_axis_RH Maximum of RH axis.
#' @param col_temp Colour for temperature line, as named HTML colour or hex.
#' @param col_RH Colour for RH line, as named HTML colour or hex.
#'
#' @returns ggplot2 Object containing temperature and RH graph with mean line and min/max ribbons
#' 
#' @examples graph_summary(envdata_exhib, graph_title = 'Exhibition cases', type = 'daily')
graph_summary <- function(envdata, title = FALSE,
                          type = 'monthly',
                          store = FALSE, exclude_stores = FALSE,
                          start_date = FALSE, end_date = FALSE,
                          date_format = '%m/%y', breaks = '2 months',
                          standard = 'BS 4971', min_temp = FALSE, max_temp = FALSE,
                          min_RH = FALSE, max_RH = FALSE,
                          max_axis_temp = 40, max_axis_RH = 100,
                          col_temp = 'red', col_RH = 'blue') {
  message('Graphing max/min/mean')
  # Set min/max by standard where not specified
  minmax <- set_minmax(standard, min_temp, max_temp, min_RH, max_RH)
  
  # Filter readings to given stores and timeframe
  subset <- subset_readings(envdata, store = store, exclude_stores = exclude_stores,
                            start_date = start_date, end_date = end_date)
  site_summary <- summarise_site(subset, type = type)
  
 
  # Add datetime column for graphing
  if (type == 'daily') {
    site_summary <-  mutate(site_summary, datetime = as.POSIXct(paste0(year, '-', month, '-', day),
                                                                format = '%Y-%m-%d')) }
  if (type == 'monthly') {
    site_summary <-  mutate(site_summary, datetime = as.POSIXct(paste0(year, '-', month,'-01'),
                                                    format = '%Y-%m-%d')) }
  if (type == 'annual') {
    site_summary <-  mutate(site_summary, datetime = as.POSIXct(paste0(year, '-01-01'),
                                                                format = '%Y-%m-%d')) }
  # Graph single store
  # store is identifying part of location, does not have to match whole string
  if(store != FALSE) {
    message('Graphing single store')
    line_alpha <- 1
    fill_alpha <- 0.2
    graph_title <- subset$location[1]
  }
  
  
  # Graph all stores
  if(store == FALSE) {
    message('Graphing all stores')
    line_alpha <- 0.7
    fill_alpha <- 0.05
    graph_title <- paste('All stores at', subset$site[1])
  }
  if(title != FALSE) { graph_title <- title }

  graph_subtitle <-  paste(str_remove(dmy_style(min(subset$datetime)), '^0'),
  'to',str_remove(dmy_style(max(subset$datetime)), '^0'))
  
  #Create graph ----
  # with time on x axis, temperature on left y axis, and RH on right y axis
  #Y scales 0-40º and 0-100%
  #Dotted lines indicate PD5454 storage guidelines
  return(site_summary %>% ggplot(mapping = aes(x = datetime, group = location)) +
           geom_hline(
             yintercept = minmax[1],
             color = col_temp,
             linetype = 'dotted',
             alpha = 0.8
           ) +
           geom_hline(
             yintercept = minmax[2],
             color = col_temp,
             linetype = 'dotted',
             alpha = 0.8
           ) +
           geom_hline(
             yintercept = minmax[3] / (max_axis_RH / max_axis_temp),
             color = col_RH,
             linetype = 'dotted',
             alpha = 0.6
           ) +
           geom_hline(
             yintercept = minmax[4] / (max_axis_RH / max_axis_temp),
             color = col_RH,
             linetype = 'dotted',
             alpha = 0.6
           ) +
           geom_line(aes(y = mean_temp),
                     color = col_temp,
                     size = 0.25,
                     alpha = line_alpha) +
           geom_ribbon(aes(ymin = min_temp, ymax = max_temp),
                       fill = col_temp,
                       size = 0.25,
                       alpha = 0.1) +
           geom_ribbon(aes(ymin = p01_RH, ymax = p99_RH),
                       fill = col_temp,
                       size = 0.25,
                       alpha = 0.1) +
           geom_line(aes(y = mean_RH / (max_axis_RH / max_axis_temp)),
                     color = col_RH,
                     size = 0.25,
                     alpha = line_alpha) +
           geom_ribbon(aes(ymin = min_RH, ymax = max_RH),
                       fill = col_RH,
                       size = 0.25,
                       alpha = 0.1) +
           geom_ribbon(aes(ymin = p01_RH, ymax = p99_RH),
                       fill = col_RH,
                       size = 0.25,
                       alpha = 0.1) +
           scale_x_datetime(name = 'Date',
                            date_breaks = breaks,
                            date_labels = date_format) +
           labs(title = graph_title, subtitle = graph_subtitle) +
           scale_y_continuous(
             name = 'Temperature (º C)',
             limits = c(0, max_axis_temp),
             sec.axis = sec_axis( ~ . * (max_axis_RH / max_axis_temp),
                                  name = 'Rel. Humidity (%)',
                                  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
           ) +
           theme(axis.title.y.left = element_text(color = col_temp),
                 axis.title.y.right = element_text(color = col_RH)))
}

# Graph light ----
#' Graph light data
#'
#' @param envdata A dataframe returned from parse_datalogger.
#' @param store A string matching a single store or a group of stores with a matching pattern.
#' @param exclude_stores A string or vector to exclude a group of stores.
#' @param graph_title Title of graph, else 'All stores at site' or first location if `store` specified.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param breaks Breaks on x axis, like '2 weeks'.
#' @param date_format Format of dates on x axis, like '%d/%m' or '12/24'.
#' @param max_lux Value of maximum lux guideline.
#' @param max_UV Value of maximum UV guideline
#' @param col_lux Colour for visible light line, as named HTML colour or hex.
#' @param col_UV Colour for UV line, as named HTML colour or hex.
#'
#' @returns ggplot2 Object containing visible light and UV graph
#' 
#' @examples graph_light(envdata_exhib, graph_title = 'Exhibition cases')
graph_light <- function(envdata, store = FALSE, exclude_stores = FALSE,
                        graph_title = FALSE,
                        start_date = FALSE, end_date = FALSE,
                        breaks = '2 months', date_format = '%m/%y',
                        max_lux = 50, max_UV = 0,
                        col_lux = 'darkgreen', col_UV = 'darkorange') {
  message('Graphing lux/UV')
  subset <- subset_readings(envdata, store = store, exclude_stores = exclude_stores,
                            start_date = start_date, end_date = end_date) %>%
    filter(!is.na(lux) & !is.na(UV))
  
  start_date <- min(subset$datetime)
  end_date <- max(subset$datetime)
  
  # Graph single store
  # store is identifying part of location, does not have to match whole string
  if(store != FALSE) {
    message('Graphing single store')
    line_alpha <- 1
    if(graph_title == FALSE) {
    graph_title <- subset$location[1]
    }
  }
  
  # Graph all stores ----
  if(store == FALSE) {
    message('Graphing all stores')
    line_alpha <- 0.5
    if(graph_title == FALSE) {
    graph_title <- paste('All stores at', subset$site[1])
    }
  }
  
  graph_subtitle <- paste(str_remove(dmy_style(subset$start_period[1]), '^0'),
                          'to',str_remove(dmy_style(subset$end_period[1]), '^0'))
  
  #Create graph ----
  # with time on x axis, light on left y axis, and UV on right y axis
  #Dotted line indicates max lux for display
  return(subset %>% ggplot(mapping = aes(x = datetime, group = location)) +
           geom_hline(
             yintercept = max_lux,
             color = col_lux,
             linetype = 'dotted',
             alpha = 0.8
           ) +
           geom_hline(
             yintercept = max_UV,
             color = col_UV,
             linetype = 'dotted',
             alpha = 0.8
           ) +
           geom_line(aes(y = lux),
                     color = col_lux,
                     size = 0.25,
                     alpha = line_alpha) +
           geom_line(aes(y = UV / 2.5),
                     color = col_UV,
                     size = 0.25,
                     alpha = line_alpha) +
           scale_x_datetime(name = 'Date',
                            date_breaks = breaks,
                            date_labels = date_format) +
           labs(title = graph_title, subtitle = graph_subtitle) +
           scale_y_continuous(
             name = 'Visible light (lux)',
             sec.axis = sec_axis( ~ . / max(subset$lux), name = 'UV (μW/lumen)')) +
           theme(axis.title.y.left = element_text(color = col_lux),
                 axis.title.y.right = element_text(color = col_UV))
  )
}

#' Graph light data summary
#'
#' @param envdata A dataframe returned from parse_datalogger.
#' @param type 'annual', 'monthly', or 'daily'
#' @param store A string matching a single store or a group of stores with a matching pattern.
#' @param exclude_stores A string or vector to exclude a group of stores.
#' @param graph_title Title of graph, else 'All stores at site' or first location if `store` specified.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param date_format Format of dates on x axis, like '%d/%m' or '12/24'.
#' @param breaks Breaks on x axis, like '2 weeks'.
#' @param max_lux Value of maximum lux guideline.
#' @param max_UV Value of maximum UV guideline
#' @param col_lux Colour for visible light line and ribbon, as named HTML colour or hex.
#' @param col_UV Colour for UV line and ribbon, as named HTML colour or hex.
#'
#' @returns ggplot2 Object containing visible light and UV graph with mean and min/max ribbons
#' 
#' @examples graph_light_summary(envdata_exhib, graph_title = 'Exhibition cases', type = 'daily')
graph_light_summary <- function(envdata, title = FALSE,
                                store = FALSE, exclude_stores = FALSE,
                                start_date = FALSE, end_date = FALSE,
                                type = 'monthly',
                                breaks = '2 months', date_format = '%m/%Y',
                                max_lux = 50, max_UV = 0,
                                col_lux = 'darkgreen', col_UV = 'darkorange') {
  message('Graphing max/min/mean')
  subset <- subset_readings(envdata, store = store, exclude_stores = exclude_stores,
                            start_date = start_date, end_date = end_date)
  if (type == 'monthly') {
    subset <-  mutate(subset, datetime = as.POSIXct(paste0(year, '-', month,'-01'),
                                                    format = '%Y-%m-%d')) }
  
  # Graph single store
  # store is identifying part of location, does not have to match whole string
  if(store != FALSE) {
    message('Graphing single store')
    line_alpha <- 1
    fill_alpha <- 0.2
    graph_title <- subset$location[1]
  }
  
  # Graph all stores
  if(store == FALSE) {
    message('Graphing all stores')
    line_alpha <- 0.7
    fill_alpha <- 0.05
    graph_title <- paste('All stores at', subset$site[1])
  }
  
  if(title != FALSE) { graph_title <- title }
  date_style <- stamp(date_format)
  graph_subtitle <- paste(dmy_style(min(subset$datetime)),'to',
                          dmy_style(max(subset$datetime)))
  
  #Create graph ----
  # with time on x axis, lux on left y axis, and UV on right y axis
  #Dotted line indicates max 50 lux for display
  return(subset %>% ggplot(mapping = aes(x = datetime, group = location)) +
           geom_hline(
             yintercept = max_lux,
             color = col_lux,
             linetype = 'dotted',
             alpha = 0.8
           ) +
           geom_line(aes(y = mean_lux),
                     color = col_lux,
                     size = 0.25,
                     alpha = line_alpha) +
           geom_ribbon(aes(ymin = min_lux, ymax = max_lux),
                       fill = col_lux,
                       size = 0.25,
                       alpha = 0.2) +
           geom_ribbon(aes(ymin = p01_lux, ymax = p99_lux),
                       fill = col_lux,
                       size = 0.25,
                       alpha = 0.1) +
           geom_line(aes(y = mean_UV / 2.5),
                     color = col_UV,
                     size = 0.25,
                     alpha = line_alpha) +
           geom_ribbon(aes(ymin = min_UV, ymax = max_UV),
                       fill = col_UV,
                       size = 0.25,
                       alpha = 0.1) +
           geom_ribbon(aes(ymin = p01_UV, ymax = p99_UV),
                       fill = col_UV,
                       size = 0.25,
                       alpha = 0.1) +
           scale_x_datetime(name = 'Date',
                            date_breaks = breaks,
                            date_labels = date_format) +
           labs(title = graph_title, subtitle = graph_subtitle) +
           scale_y_continuous(
             name = 'Visible light (lux)',
             sec.axis = sec_axis(name = 'UV (μW/lumen)')
           ) +
           theme(axis.title.y.left = element_text(color = col_lux),
                 axis.title.y.right = element_text(color = col_UV)))
}

#' Graph standard compliance
#'
#' @param envdata A dataframe returned from parse_datalogger
#' @param o_t_r 'o' for overall graph, 't' for temperature, or 'r' for RH.
#' @param standard A string for graph titles and to set minimum and maximum values if recognised.
#' @param exclude_stores A string or vector to exclude a group of stores.
#' @param graph_title Title of graph, else 'All stores at site' or first location if `store` specified.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param min_temp Minimum temperature guideline.
#' @param max_temp Maximum temperature guideline.
#' @param min_RH Minimum RH guideline.
#' @param max_RH Maximum RH guideline.
#' @param col_low Colour of low bar for temp or RH graph, as named HTML colour or hex.
#' @param col_good Colour of good bar for temp or RH graph, as named HTML colour or hex.
#' @param col_high Colour of high bar for temp or RH graph, as named HTML colour or hex.
#' @param grad_bad Colour of low end of gradient for overall graph, as named HTML colour or hex.
#' @param grad_mid Colour of midpoint of gradient for overall graph, as named HTML colour or hex.
#' @param grad_good Colour of high end of gradient for overall graph, as named HTML colour or hex.
#'
#' @returns ggplot2 Object containing bar graph of overall or temp/RH rating for each space in dataframe
#' 
#' @examples graph_compliance(envdata, 't', standard = 'Icon')
graph_compliance <- function(envdata, o_t_r = 'o', standard = 'BS 4971',
                             exclude_stores = FALSE, graph_title = FALSE,
                             start_date = FALSE, end_date = FALSE,
                             min_temp = FALSE, max_temp = FALSE, min_RH = FALSE, max_RH = FALSE,
                             col_low = '#336699', col_good = '#669933', col_high = '#993322',
                             grad_bad = '#990000', grad_mid = '#CC6600', grad_good = '#006600') {
  message('Graphing standard compliance')
  
  # Get rating data
  rated <- compliance(envdata, exclude_stores = exclude_stores, 
                      start_date = start_date, end_date = end_date,
                      standard = standard, min_temp = min_temp, max_temp = max_temp,
                      min_RH = min_RH, max_RH = max_RH)
  # Check what kind of graph it should be and set the search term
  grep_str <- switch(o_t_r,
                     'o' = 'standard',
                     't' = 'temp',
                     'r' = 'RH')
  # Filter the rating data for the relevant rows
  rated <-  filter(rated, grepl(grep_str, rated$rating))
  if (o_t_r == 't' || o_t_r == 'r') {
    rated <- filter(rated, !grepl('mean', rated$rating))
  }
  # Assign the minimum and maximum by standard or manually 
  # (supports BS 4971, PAS 198, Icon, and Bizot)
  minmax <- set_minmax(standard, min_temp, max_temp, min_RH, max_RH)
  # Check for title
  if (title == FALSE) {
    graph_title <- paste(subset$site[1], standard, 'compliance')
  }
  
  # Assign values for the subtitle and legend, and check 
  if (grepl('t', o_t_r, ignore.case = T)) {
    message('Graphing temperature compliance')
    graph_subtitle <- paste('Temperature rating',str_remove(dmy_style(min(subset$datetime)), '^0'),
                  'to',str_remove(dmy_style(max(subset$datetime)), '^0'))
    high <- paste0('Above ', minmax[2],'C')
    low <- paste0('Below ', minmax[1],'C')
  }
  if (grepl('r', o_t_r, ignore.case = T)) {
    message('Graphing RH compliance')
    graph_subtitle <- paste('RH rating',str_remove(dmy_style(min(subset$datetime)), '^0'),
                            'to',str_remove(dmy_style(max(subset$datetime)), '^0'))
    high <- paste0('Above ', minmax[4],'%')
    low <- paste0('Below ', minmax[3],'%')
  }
  if (grepl('o', o_t_r, ignore.case = T)) {
    message('Graphing overall compliance')
    graph_subtitle <- paste(standard, 'compliance', 
                            str_remove(dmy_style(min(subset$datetime)), '^0'),
                            'to',str_remove(dmy_style(max(subset$datetime)), '^0'))
  }
  
  if(!grepl('o', o_t_r)) {
    message('Creating graph')
    return(ggplot(rated, aes(
      x = factor(location,
                 levels = rev(levels(factor(location)))),
      y = value, fill = rating)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(title = graph_title, subtitle = graph_subtitle,
             x = 'Store', y = paste('Time within', standard, 'range')) +
        scale_fill_manual(name = 'Rating',
                          labels = c(high, 'Good', low),
                          values = c(col_high, col_good, col_low)) +
        coord_flip() )
  }
  
  if(grepl('o', o_t_r)) {
    message('Creating graph')
    # % in BS4971,
    # for descending x = reorder(location,value)
    # for alphabetical x = factor(location,
    # levels = rev(levels(factor(location))))
    return(ggplot(rated, aes(
      #for alphabetical
      x = factor(location, levels = rev(levels(factor(location)))),
      #for descending
      #x = reorder(location,value),
      y = value)) +
        geom_col(aes(fill = value)) +
        scale_fill_gradient2(
          low = grad_bad,
          mid = grad_mid,
          high = grad_good,
          midpoint = .5,
          limits = c(0, 1),
          labels = scales::label_percent()) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(
          title = graph_title,
          subtitle = graph_subtitle,
          fill = 'Percent',
          x = 'Store',
          y = paste0('Time within ', standard, ' range')
        ) +
        coord_flip() )
    
  }
}

#' Splice temp and RH graph on specified date
#'
#' @param envdata1 Dataframe returned from parse_datalogger for left of graph
#' @param envdata2 Dataframe returned from parse_datalogger for right of graph
#' @param store1 String to search location in envdata1
#' @param store2 String to search location in envdata2
#' @param move_date Date to splice data
#' @param graph_title Title of graph, otherwise 'Store 1 to Store 2'.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param date_format Format of dates on x axis, like '%d/%m'.
#' @param breaks Breaks on x axis, like '2 weeks'.
#' @param standard Standard to use for temp/RH guidelines.
#' @param min_temp Minimum temperature guideline.
#' @param max_temp Maximum temperature guideline.
#' @param min_RH Minimum RH guideline.
#' @param max_RH Maximum RH guideline.
#' @param max_axis_temp Maximum of temperature axis.
#' @param max_axis_RH Maximum of RH axis.
#' @param col_temp Colour for temperature line, as named HTML colour or hex.
#' @param col_RH Colour for RH line, as named HTML colour or hex.
#' @param col_line Colour for vertical line at splice date
#'
#' @returns ggplot2 Object containing temp and RH graph of two sets of data spliced on `move_date`.
#' 
#' @examples graph_move(envdata_old, envdata_new, store1 = 'Archive 1', store2 = 'Archive A', move_date = '2025-01-01')
graph_move <- function(envdata1, envdata2, store1, store2, move_date,
                       graph_title = FALSE,
                       start_date = FALSE, end_date = FALSE, 
                       date_format = '%m/%Y', breaks = '2 months',
                       standard = 'BS 4971', min_temp = FALSE, max_temp = FALSE,
                       min_RH = FALSE, max_RH = FALSE, max_axis_temp = 40, max_axis_RH = 100,
                       col_temp = 'red', col_RH = 'blue', col_line = 'darkgrey') {

  premove <- subset_readings(envdata1, start_date = start_date, 
                             end_date = move_date, store = store1)
  postmove <- subset_readings(envdata2, start_date = move_date, 
                              end_date = end_date, store = store2)
  move <- bind_rows(premove, postmove)
  end_date <- max(move$datetime)
  if(graph_title == FALSE) {   graph_title <- paste(premove$location[1],'to',postmove$location[1])
  }
   graph_subtitle <- paste0(str_remove(dmy_style(subset$start_period[1]), '$0'),
                           'to',str_remove(dmy_style(subset$end_period[1]), '$0'),
                           ', moved ',
                           str_remove(dmy_style(move_date), '^0'))
  
  #Create graph with time on x axis, temperature on left y axis, and RH on right y axis
  #Y scales 0-40º and 0-100%
  return(move %>% ggplot(mapping = aes(x = datetime)) + 
           geom_hline(yintercept = min_temp, color = col_temp, linetype = 'dotted', alpha = 0.8) +
           geom_hline(yintercept = max_temp, color = col_temp, linetype = 'dotted', alpha = 0.8) +
           geom_hline(yintercept = min_RH / (max_axis_RH / max_axis_temp), color = col_RH, linetype = 'dotted', alpha = 0.6) +
           geom_hline(yintercept = max_RH / (max_axis_RH / max_axis_temp), color = col_RH, alpha = 0.6) +
           geom_vline(xintercept = as.POSIXct(move_date), color = col_line) +
           geom_line(aes(y = temp), color = col_temp, size = 0.25) +
           geom_line(aes(y = RH/ (max_axis_RH / max_axis_temp)), color = col_RH, size = 0.25) +
           scale_x_datetime(name = 'Date') + 
           labs(title = graph_title, subtitle = graph_subtitle) +
           scale_y_continuous(
             name = 'Temperature (º C)',
             limits = c(0,max_axis_temp),
             sec.axis = sec_axis(~ .*(max_axis_RH / max_axis_temp), 
                                 name = 'Rel. Humidity (%)')) +
           theme(axis.title.y.left = element_text(color = col_temp),
                 axis.title.y.right = element_text(color = col_RH)))
}
