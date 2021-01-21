library(tidyverse)
library(lubridate)
library(scales)

#Data should have the columns:
#   time      (POSIXct)
#   temp      (dbl)
#   RH        (dbl)
#   model     (str)
#   serial    (str)
#   venue     (str)
#   location  (str)

site <- "Lambeth Palace Library"
#Place logger files exported as .csv into folder "bms"

#Combine files====
#Make list of filenames
filenames <- dir("bms", full.names=TRUE)
#Get data from files. This runs as a loop for each file.
datalist <- lapply(filenames, FUN = function(datafile) {
  #Extract first few rows containing logger information
  fileHead <- read_csv(datafile, 
                       col_names = c("datetime", "obs")) # %>%
  head(1)
  #Finds out whether the file is temperature or humidity
  is_temp <- str_detect(fileHead$obs[1],"Temp")
  #Makes a variable for the column name for the observations
  temp_or_RH <- if_else(is_temp == TRUE, "temp", "RH")
  #extract location from string, matches for example "[Small Arch 7" up to " Space"
  location_extract <- str_extract(fileHead$obs[1], "\\[.+?(?=Space)") %>%
  #strip leading bracket off
                      str_remove("[\\[]")
  #Rest of file is observations
  envdata <- read_csv(datafile, 
                      col_names = c("datetime", temp_or_RH),
                      skip = 1) %>%
    #Change am/pm to AM/PM
    mutate_at("datetime", str_replace, "am$", "AM") %>%
    mutate_at("datetime", str_replace, "pm$", "PM") %>%
    #Add venue and location columns, parse as date/time
    mutate(venue = site,
           location = location_extract, 
           datetime = parse_datetime(datetime, format = "%d-%b-%y %I:%M:%S %p")) %>% #Parse as date/time
    drop_na() #Drop rows with no reading
  
})

#Bind rows and write csv=====
#Combine data from each file into one list
annual <- bind_rows(datalist)
with_temp <- filter(annual, is.na(RH)) %>%
          select(-RH) %>%
  drop_na()
with_RH <- filter(annual, is.na(temp)) %>%
  select(-temp) %>%
  drop_na()
annual <- full_join(with_temp, with_RH)
write_csv(annual, str_c(year(min(annual$datetime)),"_annual.csv"))

#Summarize=====
summary <- annual %>%
  #mutate(month = month(datetime)) %>%
  group_by(location, year(datetime), month(datetime)) %>%
  summarise(min_temp = min(temp),
            max_temp = max(temp),
            mean_temp = mean(temp),
            min_RH = min(RH),
            max_RH = max(RH),
            mean_RH = mean(RH)) %>%
  mutate(venue = annual$venue[1])
write_csv(summary, str_c(year(min(annual$datetime)),"_summary.csv"))

#Graph=====

store <- ""
subset <- filter(annual, grepl(store, location))
graph_title <- subset$location[1]
#Override title if necessary
#graph_title <- ""

dmy_style <- stamp("20 January 2020", orders = "dmy")
date_range <- str_c(dmy_style(min(subset$datetime))," to ",dmy_style(max(subset$datetime)))

#Create graph with time on x axis, temperature on left y axis, and RH on right y axis
#Y scales 0-40º and 0-100%

subset %>% ggplot(mapping = aes(x = datetime)) + 
  geom_hline(yintercept = 20, color = "red", linetype = "dotted", alpha = 0.8) +
  geom_hline(yintercept = 13, color = "red", linetype = "dotted", alpha = 0.8) +
  geom_hline(yintercept = 60/2.5, color = "blue", linetype = "dotted", alpha = 0.6) +
  geom_hline(yintercept = 35/2.5, color = "blue", linetype = "dotted", alpha = 0.6) +
  geom_line(aes(y = temp), color = "red", size = 0.25) +
  geom_line(aes(y = RH/2.5), color = "blue", size = 0.25) +
  scale_x_datetime(name = "Date", date_breaks = "2 months", date_labels = "%m/%Y") +
  labs(title = graph_title, subtitle = date_range) +
  scale_y_continuous(
    name = "Temperature (º C, red)",
    limits = c(0,40),
    sec.axis = sec_axis(~ .*2.5, 
                        name = "Rel. Humidity (%, blue)"))
