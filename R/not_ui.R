library(tidyverse)
source("R/funs.R")

# Site information ----
# Site name to use in tables and graph titles
site <- "Lambeth Palace Library"
# Abbreviation of site for filenames, use underscores rather than spaces
site_short <- "LPL"
# Currently supports "tinytag", "rotronic", and "trendbms"
# Rotronic files must be converted to .csv because the .xls files are not readable
brand <- "trendbms"

# Process the data and create summaries ----
# Make list of filenames
filenames <- dir("../sampledata/csv2021/tandd", full.names=TRUE)
#Get data from files. This runs as a loop for each file.
datalist <- lapply(filenames, parse_datalogger, site = site, brand = brand)
envdata2 <- combine_data(datalist, brand)
envdata <- full_join(envdata, envdata1)

envdata <- filter(envdata2, temp < 25)
# movetest <- graph_move(envdata1,envdata2,store1,store2,move_date,start_date,end_date)
# movetest
# Create tables with store max/min/mean and BS4971 compliance
rotronic_data <- filter(envdata, grepl("Rotronic", model))
tinytag_data <- bind_rows(filter(envdata, grepl("TGU", model)),
                          filter(envdata, grepl("Tinytag", model)))
tandd_data <- filter(envdata, grepl("T&D", model))
trendbms_data <- filter(envdata, grepl("Trend", model))
combined_envdata <- bind_rows(rotronic_data, tinytag_data, trendbms_data)
site_summary <- summarise_site(envdata, start_date = "2020-09-01", end_date = "2021-09-01")
rating_bs <- bs4971(envdata, start_date = "2020-09-01", end_date = "2021-09-01")
comparison <- bind_rows(rotronic_summary, tinytag_summary, bms_summary)
compare_subset <- filter(comparison, grepl("3A", location))
dose <- light_dose(envdata1)
# Write .csv files of all data and tables
write_csv(envdata, str_c(date(min(envdata$datetime)),"_to_",
                         date(max(envdata$datetime)), "_data_", 
                         site_short, ".csv"))
write_csv(site_summary, str_c(date(min(envdata$datetime)),"_to_",
                         date(max(envdata$datetime)), "_summary_", 
                         site_short, ".csv"))
write_csv(rating_bs, str_c(date(min(envdata$datetime)),"_to_",
                         date(max(envdata$datetime)), "_BS4971_", 
                         site_short, ".csv"))
graph_store(tandd_data, title = "Mezzanine display cases")

# Graph stores ----
# Temperature and humidity graphs
graph_store(envdata, start_date = "2020-09-01")
# Change the value of "store" to graph a single store or group of stores sharing a pattern
# For example "7" graphs "Archive 7", but "Arch" graphs all archive spaces
graph_store(envdata, store = "3A", start_date = "2020-09-01")
graph_summary(site_summary, start_date = "2020-09-01")
# BS4971 compliance graphs, set of three for low/good/high temp and RH and overall
bs_t <- graph_bs4971(rating_bs, "t")
bs_r <- graph_bs4971(rating_bs, "R")
bs_B <- graph_bs4971(rating_bs, "B")
all_stores_graph
store_graph
bs_t
bs_r
bs_B

light_dose <-   envdata2 %>%
  mutate(location = str_remove(location, " [1-3]$")) %>%
  group_by(location, year = year(datetime), month = month(datetime)) %>%
  summarise(max_lux = max(lux),
            mean_lux = mean(lux))
subset <- rotronic_data
start_date <- "2021-05-01"
end_date <- "2021-09-01"
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
store <- "3A"
if(store != FALSE) {
  message("Graphing single store")
  subset_store <- filter(subset, grepl(store, location))
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
    subset <- filter(subset_store, !grepl(excluded_stores, location))
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
ggplot(subset_store, mapping = aes(x = datetime, group = location)) +
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
                   # date_breaks = breaks,
                   # minor_breaks = min_breaks,
                   date_labels = "%m/%Y") +
  labs(title = graph_title, subtitle = graph_subtitle) +
  scale_y_continuous(
    name = "Temperature (º C, red)",
    limits = c(0, 40),
    sec.axis = sec_axis( ~ . * 2.5,
                         name = "Rel. Humidity (%, blue)")) +
  theme(axis.title.y.left = element_text(color = "red"),
        axis.title.y.right = element_text(color = "blue"))

cumulative <- group_by(envdata2, location, datetime) %>% 
  summarise(location = location,
            day = day()) %>%
  mutate(cumlux = cumsum(lux))

ggplot(envdata2, mapping = aes(x = datetime, group = location, color = location)) +
  ylab("Cumulative lux hours") +
  xlab("Date") +
  geom_line(aes(y = cumsum(lux)))

