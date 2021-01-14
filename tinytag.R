library(tidyverse)
library(lubridate)

#Data should have the columns:
#   time      (POSIXct)
#   temp      (dbl)
#   RH        (dbl)
#   model     (str)
#   serial    (str)
#   venue     (str)
#   location  (str)

site <- "Lambeth Palace Library"

#Place logger files exported as .csv into folder "tinytag"

#Make list of filenames
filenames <- dir("tinytag", full.names=TRUE)
#Get data from files. This runs as a loop for each file.
datalist <- lapply(filenames, FUN = function(datafile) {
  #Extract first few rows containing logger information
  fileHead <- read_csv(datafile, 
                       col_names = c("id", "datetime", "temp", "RH", "dew_point")) # %>%
    head(22)
  #Rest of file is observations
  envdata <- read_csv(datafile, 
                      col_names = c("id", "datetime", "temp", "RH", "dew_point"),
                      skip = 5) %>%
    select(-c("id","dew_point")) %>%
    #Add model, serial, and location columns extracted from first rows
    mutate(model = fileHead$temp[3],
           serial = fileHead$temp[2], 
           venue = site,
           location = fileHead$temp[4]) %>%
    #Extract numeric value from temp, RH, and dew point columns
    mutate(temp = str_extract_all(temp, "[:digit:]+\\.[:digit:]+"),
           RH = str_extract_all(RH, "[:digit:]+\\.[:digit:]+")) %>%
    #Read as numeric value instead of string
    mutate(temp = as.numeric(temp), 
           RH = as.numeric(RH),
           datetime = as.POSIXct(datetime)) %>% #Parse as date/time
    drop_na() #Drop rows with no reading
})
annual <- bind_rows(datalist) #Combine data from each file into one list
write_csv(annual, "annual.csv")

summary <- annual %>%
 #mutate(month = month(datetime)) %>%
  group_by(month(datetime), year(datetime)) %>%
  summarise(max_temp = max(temp),
            min_temp = min(temp),
            mean_temp = mean(temp),
            max_RH = max(RH),
            min_RH = min(RH),
            mean_RH = mean(RH))
write_csv(summary, "summary.csv")

#Create graph with time on x axis, temperature on left y axis, and RH on right y axis
#Y scales 0-40º and 0-100%
annual %>% ggplot(mapping = aes(x = datetime)) + 
  geom_line(aes(y = temp), color = "red", size = 0.25) +
  geom_hline(yintercept = 20, color = "red", linetype = "dotted") +
  geom_hline(yintercept = 60/2.5, color = "blue", linetype = "dotted") +
  geom_text(0,20, label = "20 ºC", color = "red") +
  geom_text(0,60/2.5, label = "60% RH") +
  geom_line(aes(y = RH/2.5), color = "blue", size = 0.25) +
  scale_x_datetime(name = "Date", date_breaks = "3 months", date_labels = "%m/%y") +
  labs(title = "Audience Chamber", subtitle = "September 2019 to December 2020") +
  scale_y_continuous(
    name = "Temperature (º C, red)",
    limits = c(0,40),
    sec.axis = sec_axis(~ .*2.5, name = "Rel. Humidity (%, blue)")) 

ggplot(NULL, mapping = aes(x = datetime)) + 
  geom_line(annual, aes(x = datetime, y = temp), color = "red", size = 0.25) +
  geom_line(annual, aes(x = datetime, y = RH/2.5), color = "blue", size = 0.25) +
  geom_ribbon(summary,)
  scale_x_datetime(name = "Date", date_breaks = "3 months", date_labels = "%m/%y") +
  labs(title = "Audience Chamber", subtitle = "September 2019 to December 2020") +
  scale_y_continuous(
    name = "Temperature (º C, red)",
    limits = c(0,40),
    sec.axis = sec_axis(~ .*2.5, name = "Rel. Humidity (%, blue)")) 
