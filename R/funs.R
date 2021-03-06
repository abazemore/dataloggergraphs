library(tidyverse)
library(lubridate)
library(readxl)


# Parse files ----
 parse_datalogger <- function (datafile, site = "", brand = FALSE) {
   message("Checking brand")
   if(brand == "tinytag") { envdata <- parse_tinytag(datafile, site) }
   if(brand == "rotronic") { envdata <- parse_rotronic(datafile, site) }
   if(brand == "trendbms") { envdata <- parse_trendBMS(datafile, site) }
   if(brand == FALSE) { message("Brand not identified") }
   return(envdata)
 }

parse_tinytag <- function(datafile, site = "") {
  message("Parsing as Tinytag")
    file_head <- read_csv(datafile, 
                          col_names = c("id", "datetime", "temp", "RH", "dew_point")) %>%
      head(5)
    envdata <- read_csv(datafile, 
                        col_names = c("id", "datetime", "temp", "RH", "dew_point"),
                        skip = 5) %>%
      select(-c("id","dew_point")) %>%
      #Add venue, location, model, and serial columns extracted from first rows
      #Model does not work correctly for older files
      transmute(venue = site,
                location = file_head$temp[4],
                datetime = datetime,
                temp = temp,
                RH = RH,
                model = file_head$temp[3],
                serial = file_head$temp[2]) %>%
      #Extract numeric value from temp and RH columns
      mutate(temp = as.numeric(str_extract_all(temp, "[:digit:]+\\.[:digit:]+")),
             RH = as.numeric(str_extract_all(RH, "[:digit:]+\\.[:digit:]+")),
             datetime = as.POSIXct(datetime))
    return(envdata)
  }


parse_rotronic <- function(datafile, site = "") {
  message("Parsing as Rotronic")
  #Extract first few rows containing logger information
  file_head <- read_csv(datafile, 
                        col_names = c("date", "time", "RH", "temp")) # %>%
  head(23)
  # Rest of file is observations
  envdata <- read_csv(datafile, 
                      col_names = c("date", "time", "RH", "temp"),
                      skip = 23) %>%
    # Add venue, location, and serial columns extracted from first rows
    # Assuming "Device description" is the name of the logger and normally the location, ours were not set up correctly
    # Combine and parse datetime as POSIXct and extract numeric value of temp/RH
    # Model not recoverable
    transmute(venue = site,
              location = file_head$date[2],
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
      venue = site,
      location = str_c(location_extract, logger_number),
      datetime = parse_datetime(datetime, format = "%d-%b-%y %I:%M:%S %p"),
      model = "Trend BMS model unknown",
      serial = "Trend BMS serial unknown")
  return(envdata)
}

# Combine data from lapply list, match separate temperature and humidity files ----
combine_data <- function(datalist, brand = "") {
  message("Combining files")
  #Combine data from each file into one list
  envdata <- bind_rows(datalist)
  #Join where temperature and humidity are in separate files
  if(brand == "trendbms") {
    message("Merging T&RH files")
  with_temp <- filter(envdata, is.na(envdata$RH)) %>%
    select(-RH)
  with_RH <- filter(envdata, is.na(envdata$temp)) %>%
    select(-temp)
  envdata <- full_join(with_temp, with_RH)
  }
  envdata <-  transmute(envdata, 
      venue = venue,
      location = location,
      datetime = datetime,
      temp = temp,
      RH = RH,
      model = model,
      serial = serial
    )

  return(envdata)
}

# Summarise -----
# Summary by location

summarise_site <- function(envdata, exclude_stores = FALSE) {
  message("Summarising site")
  # if(exclude_stores != FALSE) {
  #   excluded_stores <- str_split(exclude_stores, ", ")
  #   envdata <- filter(envdata, grepl(excluded_stores, location))
  # }
  
  site_summary <- envdata %>%
    mutate(location = str_remove(location, " [1-3]$")) %>%
    group_by(venue, location, year = year(datetime), month = month(datetime)) %>%
    summarise(min_temp = min(temp),
              max_temp = max(temp),
              mean_temp = mean(temp),
              min_RH = min(RH),
              max_RH = max(RH),
              mean_RH = mean(RH))
  return(site_summary)
}

# BS4971 compliance
bs4971 <- function(envdata, exclude_stores = FALSE, 
                   start_date = FALSE, end_date = FALSE) {
  message("Calculating BS4971 compliance")
  subset <- envdata
  if(start_date != FALSE) {
    subset <- filter(subset, datetime >= start_date)
  }
  if(end_date != FALSE) {
    subset <- filter(subset, datetime <= end_date)
  }
  
  if(start_date == FALSE) {
    start_date <- min(subset$datetime)
  }
  if(end_date == FALSE) {
    end_date <- max(subset$datetime)
  }
  # if(exclude_stores != FALSE) {
  #   excluded_stores <- str_split(exclude_stores, ", ")
  #   subset <- filter(subset, !grepl(excluded_stores, location))
  # }
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
my_style <- stamp("March 2021", orders = "BY")
dmy_style <- stamp("31 March 2021", orders = "dBY")
graph_store <- function(envdata, 
                       store = FALSE, excluded_stores = FALSE,
                       start_date = FALSE, end_date = FALSE) {
  message("Graphing T&RH")
breaks <- "2 months"
min_breaks <- "1 month"
subset <- envdata

if(start_date != FALSE) {
  subset <- filter(subset, datetime >= start_date)
}
if(end_date != FALSE) {
  subset <- filter(subset, datetime <= end_date)
}

if(start_date == FALSE) {
  start_date <- min(subset$datetime)
}
if(end_date == FALSE) {
  end_date <- max(subset$datetime)
}
# Graph single store ----
# store is identifying part of location, does not have to match whole string
if(store != FALSE) {
  message("Graphing single store")
subset <- filter(subset, grepl(store, location))
line_alpha <- 1
graph_title <- store

graph_subtitle <- str_c(dmy_style(min(subset$datetime))," to ",
                        dmy_style(max(subset$datetime)))
}
#Override title and subtitle if necessary
#graph_title <- "" 
#graph_subtitle <- ""

# Graph all stores ----
if(store == FALSE) {
  message("Graphing all stores")
   if(excluded_stores != FALSE) {
     subset <- filter(subset, !grepl(excluded_stores, location))
   }
breaks <- "2 months"
min_breaks = "1 month"
line_alpha <- 0.5
graph_title <- str_c("All stores at ", subset$venue[1])
graph_subtitle <- str_c(dmy_style(min(subset$datetime))," to ",
                        dmy_style(max(subset$datetime)))
 }

# dmy_style <- stamp("31 December 2020", orders = "dmy")
graph_subtitle <- str_c(dmy_style(min(subset$datetime))," to ",
                        dmy_style(max(subset$datetime)))

#Create graph ----
# with time on x axis, temperature on left y axis, and RH on right y axis
#Y scales 0-40º and 0-100%
#Dotted lines indicate BS4971 storage guidelines
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
                   minor_breaks = min_breaks,
                   date_labels = "%m/%Y") +
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
graph_summary <- function(site_summary, 
                          store = FALSE, excluded_stores = FALSE,
                          start_date = FALSE, end_date = FALSE) {
  message("Graphing max/min/mean")
  breaks <- "6 months"
  subset <- site_summary
  
  subset <-  mutate(subset, datetime = as.POSIXct(str_c(year, "-", month,"-01"),
                                                  format = "%Y-%m-%d")) 
  if(start_date != FALSE) {
    subset <- filter(subset, datetime >= start_date)
  }
  if(end_date != FALSE) {
    subset <- filter(subset, datetime <= end_date)
  }
  
  if(start_date == FALSE) {
    start_date <- min(subset$datetime)
  }
  if(end_date == FALSE) {
    end_date <- max(subset$datetime)
  }
  date_style <- stamp("January 2021", orders = "BY")
  # Graph single store ----
  # store is identifying part of location, does not have to match whole string
  if(store != FALSE) {
    message("Graphing single store")
    subset <- filter(subset, grepl(store, location))
    line_alpha <- 1
    fill_alpha <- 0.2
    graph_title <- subset$location[1]
    
    graph_subtitle <- str_c(date_style(min(subset$datetime))," to ",date_style(max(subset$datetime)))
  }
  
  #Override title and subtitle if necessary
  #graph_title <- "" 
  #graph_subtitle <- ""
  
  # Graph all stores ----
  if(store == FALSE) {
    message("Graphing all stores")
    if(excluded_stores != FALSE) {
      subset <- filter(subset, !grepl(excluded_stores, location))
    }
    breaks <- "3 months"
    line_alpha <- 0.7
    fill_alpha <- 0.05
    graph_title <- str_c("All stores at ", subset$venue[1])
  }

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
                     date_labels = "%m/%Y") +
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
# Graph BS4971 compliance ----
graph_bs4971 <- function(rated, t_RH_BS = "B", exclude_stores = FALSE, descending = FALSE) {
  message("Graphing BS4971 compliance")
  date_style <- stamp("March 2020", orders = "BY")
  subset <-  filter(rated, grepl(t_RH_BS, rated$rating)) # %>%
  #   filter(grepl(site, venue)) # %>%
    # if(exclude_stores != FALSE) {
    # filter(!grepl(exclude_stores,location))
    # }
  #stacked temp or RH rating
  if (grepl("t", t_RH_BS, ignore.case = T)) {
    message("Graphing temperature compliance")
    subt <- str_c("Temperature rating ",date_style(subset$start_date[1]),
                  " to ",date_style(subset$end_date[1]))
    high <- "Above 23C"
    low <- "Below 13C"
    high_x <- "temp_high"
    good_x <- "temp_good"
    low_x <- "temp_low"
  }
  if (grepl("r", t_RH_BS, ignore.case = T)) {
    message("Graphing RH compliance")
    subt <- str_c("RH rating ",date_style(subset$start_date[1]),
                  " to ",date_style(subset$end_date[1]))
    high <- "Above 60%"
    low <- "Below 35%"
    high_x <- "RH_high"
    good_x <- "RH_good"
    low_x <- "RH_low"
  }
  if (grepl("B", t_RH_BS, ignore.case = T)) {
    message("Graphing overall compliance")
    subt <- str_c("BS4971 compliance ", 
                  date_style(min(subset$start_date)),
                  " to ", date_style(max(subset$end_date)))
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
      scale_fill_gradient(
        low = "#993322",
        high = "#669933",
        limits = c(0, 1)) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(
        title = subset$venue,
        subtitle = graph_subtitle,
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
    scale_x_datetime(name = "Date", date_breaks = breaks, 
                     minor_breaks = min_breaks, date_labels = "%m/%Y") +
    labs(title = graph_title, subtitle = graph_subtitle) +
    scale_y_continuous(
      name = "Temperature (º C)",
      limits = c(0,40),
      sec.axis = sec_axis(~ .*2.5, 
                          name = "Rel. Humidity (%)")) +
    theme(axis.title.y.left = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue")))
}