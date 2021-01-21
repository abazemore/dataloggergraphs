library(tidyverse)
library(lubridate)
library(scales)

#Data should have the columns:
#   venue     (str)
#   location  (str)
#   datetime  (POSIXct)
#   temp      (dbl)
#   RH        (dbl)
#   model     (str)
#   serial    (str)



site <- ""
#Abbreviation for filenames, use underscores rather than spaces
site_short <- ""

#Place logger files exported as .csv into folder "tinytag"

#Combine files====
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
    #Add venue, location, model, and serial columns extracted from first rows
    #Model does not work correctly for older files
    transmute(venue = site,
              location = fileHead$temp[4],
              datetime = datetime,
              temp = temp,
              RH = RH,
              model = fileHead$temp[3],
              serial = fileHead$temp[2]) %>%
    #Extract numeric value from temp and RH columns
    mutate(temp = as.numeric(str_extract_all(temp, "[:digit:]+\\.[:digit:]+")),
           RH = as.numeric(str_extract_all(RH, "[:digit:]+\\.[:digit:]+")),
           datetime = as.POSIXct(datetime))
})

annual <- bind_rows(datalist) #Combine data from each file into one list
write_csv(annual, str_c(min(annual$datetime),"_annual_",site_short,".csv"))

#Summarize====
summary <- annual %>%
  #mutate(month = month(datetime)) %>%
  group_by(venue, location, year(datetime), month(datetime)) %>%
  summarise(min_temp = min(temp),
            max_temp = max(temp),
            mean_temp = mean(temp),
            min_RH = min(RH),
            max_RH = max(RH),
            mean_RH = mean(RH))
write_csv(summary, str_c(year(min(annual$datetime)),"_summary_",site_short,".csv"))

#Graph=====

#Graph single store
#Identifying part of location, does not have to match whole string
store <- ""
breaks <- "6 months"
subset <- filter(annual, grepl(store, location))
line_alpha <- 1
graph_title <- subset$location[1]
dmy_style <- stamp("20 January 2020", orders = "dmy")
graph_subtitle <- str_c(dmy_style(min(subset$datetime))," to ",dmy_style(max(subset$datetime)))
#Override title and subtitle if necessary
#graph_title <- "" 
#graph_subtitle <- ""

#Or graph all stores
subset <- annual
breaks <- "6 months"
line_alpha <- 0.5
graph_title <- str_c("All stores at ", site)
graph_subtitle <- str_c(dmy_style(min(subset$datetime))," to ",
                        dmy_style(max(subset$datetime)))
#Override title and subtitle if necessary
#graph_title <- ""
#graph_subtitle <- ""

#Create graph with time on x axis, temperature on left y axis, and RH on right y axis
#Y scales 0-40º and 0-100%
subset %>% ggplot(mapping = aes(x = datetime)) + 
  geom_hline(yintercept = 20, color = "red", linetype = "dotted", alpha = 0.8) +
  geom_hline(yintercept = 13, color = "red", linetype = "dotted", alpha = 0.8) +
  geom_hline(yintercept = 60/2.5, color = "blue", linetype = "dotted", alpha = 0.6) +
  geom_hline(yintercept = 35/2.5, color = "blue", linetype = "dotted", alpha = 0.6) +
  geom_line(aes(y = temp), color = "red", size = 0.25, alpha = line_alpha) +
  geom_line(aes(y = RH/2.5), color = "blue", size = 0.25, alpha = line_alpha) +
  scale_x_datetime(name = "Date", date_breaks = breaks, date_labels = "%m/%Y") +
  labs(title = graph_title, subtitle = graph_subtitle) +
  scale_y_continuous(
    name = "Temperature (º C, red)",
    limits = c(0,40),
    sec.axis = sec_axis(~ .*2.5, 
                        name = "Rel. Humidity (%, blue)"))
