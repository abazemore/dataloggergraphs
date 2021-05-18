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
site_short <- ""


#Combine files====
#Make list of filenames
filenames <- dir("allsites", full.names=TRUE)
#Get data from files. This runs as a loop for each file.
datalist <- lapply(filenames, read_csv)
#Combine data from each file into one list and write csv
annual <- bind_rows(datalist)
write_csv(annual, str_c("2020_annual_",site_short,".csv"))

#Summarize=====
summary <- annual %>%
  #mutate(month = month(datetime)) %>%
  group_by(venue, location, year(datetime), month(datetime)) %>%
  summarise(min_temp = min(temp),
            max_temp = max(temp),
            mean_temp = mean(temp),
            min_RH = min(RH),
            max_RH = max(RH),
            mean_RH = mean(RH))
write_csv(summary, str_c("2020_summary_",site_short,".csv"))

#Graph====
#Or from file
#annual <- read_csv("2020_annual_all_sites.csv")
store1 <- ""
store2 <- ""

move_date <- as.Date("2020-11-04")
start_date <- as.Date("2020-03-01")

premove <- filter(annual, grepl(store1, location)) %>%
  filter(datetime > start_date & datetime < move_date)
postmove <- filter(annual, grepl(store2, location)) %>%
  filter(datetime > move_date)
move <- bind_rows(premove, postmove)
end_date <- max(move$datetime)
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
move %>% ggplot(mapping = aes(x = datetime)) + 
  geom_hline(yintercept = 20, color = "red", linetype = "dotted", alpha = 0.8) +
  geom_hline(yintercept = 13, color = "red", linetype = "dotted", alpha = 0.8) +
  geom_hline(yintercept = 60/2.5, color = "blue", linetype = "dotted", alpha = 0.6) +
  geom_hline(yintercept = 35/2.5, color = "blue", alpha = 0.6) +
  geom_vline(xintercept = as.POSIXct(move_date), color = "darkgrey") +
   geom_line(aes(y = temp), color = "red", size = 0.25) +
  geom_line(aes(y = RH/2.5), color = "blue", size = 0.25) +
  scale_x_datetime(name = "Date", date_breaks = "2 months", date_labels = "%m/%Y") +
  labs(title = graph_title, subtitle = daterange) +
  scale_y_continuous(
    name = "Temperature (º C, red)",
    limits = c(0,40),
    sec.axis = sec_axis(~ .*2.5, 
                        name = "Rel. Humidity (%, blue)"))
