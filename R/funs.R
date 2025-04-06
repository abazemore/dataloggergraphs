library(tidyverse)
library(lubridate)
library(readxl)


# Parse files ----
 parse_datalogger <- function (datafile, site = "", brand = FALSE) {
   message("Checking brand")
   if(brand == "tinytag") { envdata <- parse_tinytag(datafile, site) }
   if(brand == "rotronic") { envdata <- parse_rotronic(datafile, site) }
   if(brand == "trendbms") { envdata <- parse_trendBMS(datafile, site) }
   if(brand == "tandd") { envdata <- parse_TandD(datafile, site) }
   if(brand == "previous") { envdata <- parse_ahgm(datafile, site) }
   if(brand == FALSE) { message("Brand not identified") }
   return(envdata)
 }

parse_tinytag <- function(datafile, site = "") {
  message("Parsing as Tinytag")
    file_head <- read_csv(datafile, 
                          col_names = c("id", "datetime", "temp", "RH")) %>%
      head(5)
    envdata <- read_csv(datafile, 
                        col_names = c("id", "datetime", "temp", "RH"),
                        skip = 5) %>%
      #Add venue, location, model, and serial columns extracted from first rows
      #Model does not work correctly for older files
      transmute(venue = as.character(site),
                location = as.character(file_head$temp[4]),
                datetime = datetime,
                temp = temp,
                RH = RH,
                model = as.character(file_head$temp[3]),
                serial = as.character(file_head$temp[2])) %>%
      #Extract numeric value from temp and RH columns
      mutate(temp = as.numeric(str_extract_all(temp, "[:digit:]+\\.[:digit:]+")),
             RH = as.numeric(str_extract_all(RH, "[:digit:]+\\.[:digit:]+")),
             datetime = as.POSIXct(datetime))
    return(envdata)
  }


parse_rotronic <- function(datafile, site = "") {
  message("Parsing as Rotronic")
  #Extract first few rows containing logger information
  if(str_detect(datafile, ".csv")) {
  file_head <- read_csv(datafile, 
                        col_names = c("date", "time", "RH", "temp")) # %>%
  head(23)
  file_data <- read_csv(datafile, 
                       col_names = c("date", "time", "RH", "temp"),
                       skip = 23)
  }
  # The xls export is actually a tsv, but this section does not work yet.
  if(str_detect(datafile, ".xls")) {
    file_head <- head(read_delim(datafile, delim = "\t", col_names = c("date")), 4)
    file_data <- read_delim(datafile, delim = "\t", skip = 22)
  }
  # Rest of file is observations
  envdata <-  file_data %>%
    # Add venue, location, and serial columns extracted from first rows
    # Assuming "Device description" is the name of the logger and normally the location, ours were not set up correctly
    # Combine and parse datetime as POSIXct and extract numeric value of temp/RH
    # Model not recoverable
    transmute(venue = as.character(site),
              location = as.character(file_head$date[2]),
              datetime = as.POSIXct(parse_datetime(str_c(date, " ", time), format="%d/%m/%Y %H:%M:%S")),
              temp = as.numeric(str_extract_all(temp, "[:digit:]+\\.[:digit:]+")),
              RH = as.numeric(str_extract_all(RH, "[:digit:]+\\.[:digit:]+")),
              model = str_c("Rotronic ",str_remove(file_head$date[5],"Version = ")," model unknown"),
              serial = str_remove(file_head$date[6],"Serial Number = "))
  return(envdata)
}

parse_trendBMS <- function(datafile, site = "") {
  message("Parsing as Trend BMS")
  #Extract first few rows containing logger information
  file_head <- read_csv(datafile,
                        col_names = c("datetime", "obs")) # %>%
  head(1)
  #Finds out whether the file is temperature or humidity
  is_temp <- str_detect(file_head$obs[1], "Temp")
  #Makes a variable for the column name for the observations
  temp_or_RH <- if_else(is_temp == TRUE, "temp", "RH")
  #extract location from string, matches for example "Small Arch 7" up to " Space"
  #Formula is "(?<=(START)).*?(?=END)", where START and END are the characters before and after the location and logger number
  location_extract <- str_extract(file_head$obs[1],
                                  "(?<=\\[).*?(?= Space)")
  logger_number <- str_c(" ",
                         str_extract(file_head$obs[1],
                                     "(?<=(Temp |Humidity )).*?(?=\\])")) %>%
    replace_na("")
  #Rest of file is observations
  envdata <- read_csv(datafile,
                      col_names = c("datetime", temp_or_RH),
                      skip = 1) %>%
    #Add venue, location, model, and serial columns, parse as date/time
    mutate(
      venue = as.character(site),
      location = str_c(location_extract, logger_number),
      datetime = parse_datetime(datetime, format = "%d-%b-%y %I:%M %p BST"),
      model = "Trend BMS model unknown",
      serial = "Trend BMS serial unknown")
  return(envdata)
}

parse_TandD <- function(datafile, site = "") {
  message("Parsing as T&D")
  #Extract first few rows containing logger information
  file_head <- read_csv(datafile, 
                        col_names = c("datetime", "time", "lux", "UV",
                                      "temp", "RH","luxhours","UVhours")) # %>%
  head(3)
  if(datafile != "0.csv") {
    location <- str_extract(datafile, "([A-Za-z0-9 ])+(?= F8)")
    serial = str_extract(datafile, "(F80)[A-Z0-9]+(?= )")
  }
  else{
    if(file_head$datetime[1] != "Date/Time") { location <- file_head$datetime[1] }
        if(file_head$time[1] != "Date/Time") { serial <- file_head$time[1] }
    else { serial <- "Serial unknown"
    location <- "Location unknown" }
  }
  # Rest of file is observations
  envdata <- read_csv(datafile, 
                      col_names = c("datetime", "time", "lux", "UV",
                                    "temp", "RH", "luxhours", "UVhours"),
                      col_types = "cccccccc",
                      skip = 3) %>%
    # Add venue, location, and serial columns extracted from first rows
    # Assuming "Device description" is the name of the logger and normally the location, ours were not set up correctly
    # Combine and parse datetime as POSIXct and extract numeric value of temp/RH
    # Model not recoverable
    transmute(venue = as.character(site),
              location = as.character(location),
              datetime = as.POSIXct(datetime),
              lux = as.numeric(lux),
              UV = as.numeric(UV),
              temp = as.numeric(temp),
              RH = as.numeric(RH),
              luxhours = as.numeric(luxhours),
              UVhours = as.numeric(UVhours),
              model = str_c("TR-74Ui"),
              serial = as.character(serial))
  return(envdata)
}

parse_previous <- function(datafile, site = "") {
  message("Parsing as previously processed")
  envdata <- read_csv(datafile, col_types = "Tnncicc")
  return(envdata)
}

# Combine data from lapply list, match separate temperature and humidity files ----
combine_data <- function(datalist, brand = "") {
  message("Combining files")
  #Combine data from each file into one list
  envdata <- bind_rows(datalist) %>% distinct()
  #Join where temperature and humidity are in separate files
  if(brand == "trendbms") {
    message("Merging T&RH files")
  with_temp <- filter(envdata, is.na(envdata$RH)) %>%
    select(-RH)
  with_RH <- filter(envdata, is.na(envdata$temp)) %>%
    select(-temp)
  envdata <- full_join(with_temp, with_RH)
  }
  if(brand != "tandd") {
  envdata <-  transmute(envdata, 
      venue = venue,
      location = location,
      datetime = datetime,
      temp = temp,
      RH = RH,
      model = model,
      serial = serial
    )
  }
  if(brand == "tandd") {
  envdata <-  transmute(envdata, 
      venue = venue,
      location = location,
      datetime = datetime,
      lux = lux,
      UV = UV,
      temp = temp,
      RH = RH,
      model = model,
      serial = serial
    )
  }
  return(envdata)
}

# Subset readings ----
subset_readings <- function(envdata, store = FALSE, exclude_stores = FALSE,
                            start_date = FALSE, end_date = FALSE) {
  subset <- envdata
  if(store != FALSE) { 
    subset <- filter(subset, grepl(store, location))
  }
  if(exclude_stores != FALSE) {
    subset <- filter(subset, !grepl(exclude_stores, location))
    
  }
  if(start_date != FALSE) {
    subset <- filter(subset, datetime >= start_date)
  }
  if(end_date != FALSE) {
    subset <- filter(subset, datetime <= end_date)
  }
  return(subset)
}

# Summarise -----
# Summary by location

summarise_site <- function(envdata, exclude_stores = FALSE,
                           type = "tRH") {
  message("Summarising site")
  envdata <- drop_na(subset_readings(envdata, exclude_stores))
  # If monitor type includes light data, include lux and UV columns
  if(type == "light") {
    site_summary <- envdata %>%
      group_by(venue, location, year = year(datetime), month = month(datetime)) %>%
      summarise(min_temp = min(temp),
                max_temp = max(temp),
                mean_temp = mean(temp),
                min_RH = min(RH),
                max_RH = max(RH),
                mean_RH = mean(RH),
                min_lux = min(lux),
                max_lux = max(lux),
                mean_lux = mean(lux),
                min_UV = min(UV),
                max_UV = max(UV),
                mean_UV = mean(UV))
  }
  else {
  site_summary <- envdata %>%
    mutate(location = str_remove(location, " [1-3]$")) %>%
    group_by(venue, location, year = year(datetime), month = month(datetime)) %>%
    summarise(min_temp = min(temp),
              max_temp = max(temp),
              mean_temp = mean(temp),
              min_RH = min(RH),
              max_RH = max(RH),
              mean_RH = mean(RH))
  }
  return(site_summary)
}

light_dose <- function(envdata, start_date = FALSE, 
           end_date = FALSE, obs_hour = 4) {
  subset <- subset_readings(envdata, 
                            start_date = start_date, end_date = end_date)
 # Summarize light exposure including just noticeable fade thresholds for blue wool standards
  dose <- subset %>%
    group_by(venue, location) %>%
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


# BS4971 compliance
bs4971 <- function(envdata, exclude_stores = FALSE, 
                   start_date = FALSE, end_date = FALSE) {
  message("Calculating BS4971 compliance")
  subset <- subset_readings(envdata, exclude_stores = exclude_stores,
                            start_date = start_date, end_date = end_date)
 
    start_date <- min(subset$datetime)
 
    end_date <- max(subset$datetime)
    
  rated <- subset %>%
    group_by(venue, location)  %>%
    mutate(location = str_remove(location, " [1-3]$")) %>%
    summarise(
      start_period = min(datetime),
      end_period = max(datetime),
      BS4971 = mean(temp >= 13 &
                      temp <= 23 &
                      RH >= 35 &
                      RH <= 60, na.rm = TRUE),
      temp_low = mean(temp < 13, na.rm = TRUE),
      temp_good = mean(temp >= 13 &
                         temp <= 23, na.rm = TRUE),
      temp_high = mean(temp > 23, na.rm = TRUE),
      RH_low = mean(RH < 35),
      RH_good = mean(RH >= 35 &
                       RH <= 60, na.rm = TRUE),
      RH_high = mean(RH > 60, na.rm = TRUE),
      
    ) %>%
    pivot_longer(
      cols = starts_with(c("temp", "RH", "BS")),
      names_to = "rating",
      values_to = "value",
      values_drop_na = TRUE
    ) %>%
    mutate(
      rating = fct_relevel(
        rating,
        "RH_high",
        "RH_good",
        "RH_low",
        "temp_high",
        "temp_good",
        "temp_low",
        "BS4971"
      )
    )
  return(rated)
}

# Graph ----
dmy_style <- stamp("31 March 2021", orders = "dBY")
graph_store <- function(envdata, title = FALSE,
                       store = FALSE, exclude_stores = FALSE, graph_title = FALSE,
                       start_date = FALSE, end_date = FALSE, 
                       date_format = '%m/%Y', breaks = '2 months') {
  message("Graphing T&RH")
subset <- subset_readings(envdata, store = store, exclude_stores = exclude_stores,
                          start_date = start_date, end_date = end_date)

  start_date <- min(subset$datetime)
  end_date <- max(subset$datetime)

# Graph single store ----
# store is identifying part of location, does not have to match whole string
if(store != FALSE) {
  message("Graphing single store")
line_alpha <- 1
graph_title <- subset$location[1]
}

# Graph all stores ----
if(store == FALSE) {
  message("Graphing all stores")
  line_alpha <- 0.5
  graph_title <- str_c("All stores at ", subset$venue[1])
}
  
if(title != FALSE) { graph_title <- title }
graph_subtitle <- str_c(dmy_style(min(subset$datetime))," to ",
                        dmy_style(max(subset$datetime)))

# Create graph ----
# with time on x axis, temperature on left y axis, and RH on right y axis
# Y scales 0-40º and 0-100%
# Dotted lines indicate BS4971 storage guidelines
return(subset %>% ggplot(mapping = aes(x = datetime, group = location)) +
  geom_hline(
    yintercept = 23,
    color = "red",
    linetype = "dotted",
    alpha = 0.8
  ) +
  geom_hline(
    yintercept = 13,
    color = "red",
    linetype = "dotted",
    alpha = 0.8
  ) +
  geom_hline(
    yintercept = 60 / 2.5,
    color = "blue",
    linetype = "dotted",
    alpha = 0.6
  ) +
  geom_hline(
    yintercept = 35 / 2.5,
    color = "blue",
    linetype = "dotted",
    alpha = 0.6
  ) +
  geom_line(aes(y = temp),
            color = "red",
            size = 0.25,
            alpha = line_alpha) +
  geom_line(aes(y = RH / 2.5),
            color = "blue",
            size = 0.25,
            alpha = line_alpha) +
  scale_x_datetime(name = "Date",
                   date_breaks = breaks,
                   date_labels = date_format) +
  labs(title = graph_title, subtitle = graph_subtitle) +
  scale_y_continuous(
    name = "Temperature (º C, red)",
    limits = c(0, 40),
    sec.axis = sec_axis( ~ . * 2.5,
                         name = "Rel. Humidity (%, blue)")) +
      theme(axis.title.y.left = element_text(color = "red"),
            axis.title.y.right = element_text(color = "blue"))
  )
}

# Graph max/min/mean summary ----
graph_summary <- function(site_summary, title = FALSE,
                          store = FALSE, exclude_stores = FALSE,
                          start_date = FALSE, end_date = FALSE,
                          date_format = '%m/%Y', breaks = '2 months') {
  message("Graphing max/min/mean")
  subset <- subset(site_summary, store = store, exclude_stores = exclude_stores,
                   start_date = start_date, end_date = end_date)
  
  subset <-  mutate(subset, datetime = as.POSIXct(str_c(year, "-", month,"-01"),
                                                  format = "%Y-%m-%d")) 

  date_style <- stamp("January 2021", orders = "BY")
  # Graph single store ----
  # store is identifying part of location, does not have to match whole string
  if(store != FALSE) {
    message("Graphing single store")
    subset <- filter(subset, grepl(store, location))
    line_alpha <- 1
    fill_alpha <- 0.2
    graph_title <- subset$location[1]
    }
  
  #Override title and subtitle if necessary
  #graph_title <- "" 
  #graph_subtitle <- ""
  
  # Graph all stores ----
  if(store == FALSE) {
    message("Graphing all stores")
    line_alpha <- 0.7
    fill_alpha <- 0.05
    graph_title <- str_c("All stores at ", subset$venue[1])
  }
  if(title != FALSE) { graph_title <- title }
  dmy_style <- stamp("31 December 2020", orders = "dmy")
  graph_subtitle <- str_c(my_style(min(subset$datetime))," to ",
                          my_style(max(subset$datetime)))
  
  #Create graph ----
  # with time on x axis, temperature on left y axis, and RH on right y axis
  #Y scales 0-40º and 0-100%
  #Dotted lines indicate PD5454 storage guidelines
  return(subset %>% ggplot(mapping = aes(x = datetime, group = location)) +
    geom_hline(
      yintercept = 23,
      color = "red",
      linetype = "dotted",
      alpha = 0.8
    ) +
    geom_hline(
      yintercept = 13,
      color = "red",
      linetype = "dotted",
      alpha = 0.8
    ) +
    geom_hline(
      yintercept = 60 / 2.5,
      color = "blue",
      linetype = "dotted",
      alpha = 0.6
    ) +
    geom_hline(
      yintercept = 35 / 2.5,
      color = "blue",
      linetype = "dotted",
      alpha = 0.6
    ) +
    geom_line(aes(y = mean_temp),
              color = "red",
              size = 0.25,
              alpha = line_alpha) +
    geom_ribbon(aes(ymin = min_temp, ymax = max_temp),
                fill = "red",
                size = 0.25,
                alpha = 0.2) +
    geom_line(aes(y = mean_RH / 2.5),
              color = "blue",
              size = 0.25,
              alpha = line_alpha) +
    geom_ribbon(aes(ymin = min_RH, ymax = max_RH),
                fill = "blue",
                size = 0.25,
                alpha = 0.2) +
    scale_x_datetime(name = "Date",
                     date_breaks = breaks,
                     date_labels = date_format) +
    labs(title = graph_title, subtitle = graph_subtitle) +
    scale_y_continuous(
      name = "Temperature (º C)",
      limits = c(0, 40),
      sec.axis = sec_axis( ~ . * 2.5,
                           name = "Rel. Humidity (%)")
    ) +
    theme(axis.title.y.left = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue")))
}

# Graph light ----
graph_light <- function(envdata, title = FALSE,
                        store = FALSE, exclude_stores = FALSE,
                        start_date = FALSE, end_date = FALSE,
                        breaks = '2 months', date_format = '%m/%Y') {
  message("Graphing lux/UV")
  subset <- subset_readings(envdata, store = store, exclude_stores = exclude_stores,
                            start_date = start_date, end_date = end_date)
  
  start_date <- min(subset$datetime)

  end_date <- max(subset$datetime)
  # Graph single store ----
  # store is identifying part of location, does not have to match whole string
  if(store != FALSE) {
    message("Graphing single store")
    line_alpha <- 1
    graph_title <- subset$location[1]
    

  }
  #Override title and subtitle if necessary
  #graph_title <- "" 
  #graph_subtitle <- ""
  
  # Graph all stores ----
  if(store == FALSE) {
    message("Graphing all stores")
    line_alpha <- 0.5
    graph_title <- str_c("All stores at ", subset$venue[1])
  }
  
  if(title != FALSE) { graph_title <- title }
  
  graph_subtitle <- str_c(dmy_style(min(subset$datetime))," to ",
                          dmy_style(max(subset$datetime)))

  #Create graph ----
  # with time on x axis, light on left y axis, and UV on right y axis
  #Dotted line indicates max 50 lux for display
  return(subset %>% ggplot(mapping = aes(x = datetime, group = location)) +
           geom_hline(
             yintercept = 50,
             color = "darkgreen",
             linetype = "dotted",
             alpha = 0.8
           ) +
           geom_line(aes(y = lux),
                     color = "darkgreen",
                     size = 0.25,
                     alpha = line_alpha) +
           geom_line(aes(y = UV / 2.5),
                     color = "darkorange",
                     size = 0.25,
                     alpha = line_alpha) +
           scale_x_datetime(name = "Date",
                            date_breaks = breaks,
                            date_labels = date_format) +
           labs(title = graph_title, subtitle = graph_subtitle) +
           scale_y_continuous(
             name = "Visible light (lux, green)",
             sec.axis = sec_axis( ~ . / max(subset$lux), name = "UV (μW/lumen, orange)")) +
           theme(axis.title.y.left = element_text(color = "darkgreen"),
                 axis.title.y.right = element_text(color = "darkorange"))
  )
}

# Graph max/min/mean summary ----
graph_light_summary <- function(light_summary, title = FALSE,
                          store = FALSE, exclude_stores = FALSE,
                          start_date = FALSE, end_date = FALSE,
                          breaks = '2 months', date_format = '%m/%Y') {
  message("Graphing max/min/mean")
  subset <- subset_readings(light_summary, store = store, exclude_stores = exclude_stores,
                            start_date = start_date, end_date = end_date)
  
  subset <-  mutate(subset, datetime = as.POSIXct(str_c(year, "-", month,"-01"),
                                                  format = "%Y-%m-%d")) 
  
  date_style <- stamp("January 2021", orders = "BY")
  # Graph single store ----
  # store is identifying part of location, does not have to match whole string
  if(store != FALSE) {
    message("Graphing single store")
    line_alpha <- 1
    fill_alpha <- 0.2
    graph_title <- subset$location[1]
    }
  
  # Graph all stores ----
  if(store == FALSE) {
    message("Graphing all stores")
    line_alpha <- 0.7
    fill_alpha <- 0.05
    graph_title <- str_c("All stores at ", subset$venue[1])
  }
  
  if(title != FALSE) { graph_title <- title }
  dmy_style <- stamp("31 December 2020", orders = "dmy")
  graph_subtitle <- str_c(my_style(min(subset$datetime))," to ",
                          my_style(max(subset$datetime)))
  
  #Create graph ----
  # with time on x axis, lux on left y axis, and UV on right y axis
  #Dotted line indicates max 50 lux for display
  return(subset %>% ggplot(mapping = aes(x = datetime, group = location)) +
           geom_hline(
             yintercept = 50,
             color = "darkgreen",
             linetype = "dotted",
             alpha = 0.8
           ) +
           geom_line(aes(y = mean_lux),
                     color = "darkgreen",
                     size = 0.25,
                     alpha = line_alpha) +
           geom_ribbon(aes(ymin = min_lux, ymax = max_lux),
                       fill = "darkgreen",
                       size = 0.25,
                       alpha = 0.2) +
           geom_line(aes(y = mean_UV / 2.5),
                     color = "darkorange",
                     size = 0.25,
                     alpha = line_alpha) +
           geom_ribbon(aes(ymin = min_UV, ymax = max_UV),
                       fill = "darkorange",
                       size = 0.25,
                       alpha = 0.2) +
           scale_x_datetime(name = "Date",
                            date_breaks = breaks,
                            date_labels = date_format) +
           labs(title = graph_title, subtitle = graph_subtitle) +
           scale_y_continuous(
             name = "Visible light (lux, green)",
             sec.axis = sec_axis(name = "UV (μW/lumen, orange)")
           ) +
           theme(axis.title.y.left = element_text(color = "darkgreen"),
                 axis.title.y.right = element_text(color = "darkorange")))
}
# Graph BS4971 compliance ----
graph_bs4971 <- function(rated, t_RH_BS = "B", exclude_stores = FALSE, descending = FALSE) {
  message("Graphing BS4971 compliance")
  date_style <- stamp("March 2020", orders = "BY")
  subset <-  filter(rated, grepl(t_RH_BS, rated$rating))
  
  #stacked temp or RH rating
  if (grepl("t", t_RH_BS, ignore.case = T)) {
    message("Graphing temperature compliance")
    subt <- str_c("Temperature rating ",date_style(subset$start_period[1]),
                  " to ",date_style(subset$end_period[1]))
    high <- "Above 23C"
    low <- "Below 13C"
    high_x <- "temp_high"
    good_x <- "temp_good"
    low_x <- "temp_low"
  }
  if (grepl("r", t_RH_BS, ignore.case = T)) {
    message("Graphing RH compliance")
    subt <- str_c("RH rating ",date_style(subset$start_period[1]),
                  " to ",date_style(subset$end_period[1]))
    high <- "Above 60%"
    low <- "Below 35%"
    high_x <- "RH_high"
    good_x <- "RH_good"
    low_x <- "RH_low"
  }
  if (grepl("B", t_RH_BS, ignore.case = T)) {
    message("Graphing overall compliance")
    subt <- str_c("BS4971 compliance ", 
                  date_style(min(subset$start_period)),
                  " to ", date_style(max(subset$end_period)))
  }
  
  if(!grepl("B", t_RH_BS)) {
    message("Creating graph")
    return(ggplot(subset, aes(
      x = factor(location,
                 levels = rev(levels(factor(location)))),
      y = value, fill = rating)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(title = subset$venue, subtitle = subt,
           x = "Store", y = "Time within range") +
      scale_fill_manual(name = "Rating",
                        labels = c(high, "Good", low),
                        values = c("#993322","#669933","#336699")) +
      coord_flip() )
  } 
  
  if(grepl("B", t_RH_BS)) {
    message("Creating graph")
    # % in BS4971,
    # for descending x = reorder(location,value)
    # for alphabetical x = factor(location,
    # levels = rev(levels(factor(location))))
    
    return(ggplot(subset, aes(
      #for alphabetical
      x = factor(location, levels = rev(levels(factor(location)))),
      #for descending
      #x = reorder(location,value),
      y = value)) +
      geom_col(aes(fill = value), show.legend = FALSE) +
      scale_fill_gradient2(
        low = "#990000",
        mid = "#CC6600",
        high = "#006600",
        #low = "#CC0000",
        #mid = "#FF9900",
        #high = "#33CC00",
        midpoint = .5,
        limits = c(0, 1)) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(
        title = subset$venue,
        subtitle = subt,
        x = "Store",
        y = "Time spent in BS4971 range"
      ) +
      coord_flip() )

  }
}

graph_move <- function(envdata1, envdata2, store1, store2, move_date, start_date, end_date) {
  
  premove <- filter(envdata1, grepl(store1, location)) %>%
    filter(datetime > start_date & datetime < move_date)
  postmove <- filter(envdata2, grepl(store2, location)) %>%
    filter(datetime > move_date & datetime < end_date)
  move <- bind_rows(premove, postmove)
  end_date <- max(move$datetime)
  breaks <- "2 months"
  min_breaks <- "1 month"
  date_style <- stamp("2 January 2020", orders = "dmy")
  graph_title <- str_c(premove$location[1]," to ",postmove$location[1])
  graph_subtitle <- str_c(date_style(start_date)," to ",
                          date_style(end_date),
                          ", moved ",
                          date_style(move_date))
  # Override if necessary, comment out if not
  #graph_title <- ""
  #graph_subtitle <- ""
  
  #Create graph with time on x axis, temperature on left y axis, and RH on right y axis
  #Y scales 0-40º and 0-100%
  return(move %>% ggplot(mapping = aes(x = datetime)) + 
    geom_hline(yintercept = 23, color = "red", linetype = "dotted", alpha = 0.8) +
    geom_hline(yintercept = 13, color = "red", linetype = "dotted", alpha = 0.8) +
    geom_hline(yintercept = 60/2.5, color = "blue", linetype = "dotted", alpha = 0.6) +
    geom_hline(yintercept = 35/2.5, color = "blue", alpha = 0.6) +
    geom_vline(xintercept = as.POSIXct(move_date), color = "darkgrey") +
    geom_line(aes(y = temp), color = "red", size = 0.25) +
    geom_line(aes(y = RH/2.5), color = "blue", size = 0.25) +
    scale_x_datetime(name = "Date") + 
                     # date_breaks = breaks, 
                     # minor_breaks = min_breaks, 
                     #date_labels = "%m/%Y") +
    labs(title = graph_title, subtitle = graph_subtitle) +
    scale_y_continuous(
      name = "Temperature (º C)",
      limits = c(0,40),
      sec.axis = sec_axis(~ .*2.5, 
                          name = "Rel. Humidity (%)")) +
    theme(axis.title.y.left = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue")))
}