library(tidyverse)
source("R/funs.R")

# Site information ----
# Site name to use in tables and graph titles
site <- "Lambeth Palace Library"
# Abbreviation of site for filenames, use underscores rather than spaces
site_short <- "LPL"
# Currently supports "tinytag", "rotronic", "trendbms", "tandd", and "previous" for csvs generated here.
# Rotronic files must be converted to .csv because the .xls files are not readable
brand <- "tandd"

# Process the data and create summaries ----
# Make list of filenames
filenames <- dir("data/tandd", full.names=TRUE)
#Get data from files. This runs as a loop for each file.
datalist <- lapply(filenames, parse_datalogger, site = site, brand = brand)
envdata <- combine_data(datalist, brand)
#envdata <- filter(envdata, datetime >= '2022-08-08')

# Create tables with store max/min/mean and BS4971 compliance ----
site_summary <- summarise_site(envdata, type = 'light')
dose <- light_dose(envdata)
rating_bs <- bs4971(envdata)

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
write_csv(dose, str_c(date(min(dose$start_period)),"_to_",
                         date(max(dose$end_period)), "_light_", 
                         site_short, ".csv"))

# Graph stores ----
# Temperature and humidity graphs
# If no title specified, "All stores at [venue]", or if store specified, the first location in the list
graph_store(envdata, title = 'Coronation exhibition: All cases', breaks = '2 weeks', date_format = '%d/%m')
# Change the value of "store" to graph a single store or group of stores sharing a pattern
# For example "7" graphs "Archive 7", but "Arch" graphs all archive spaces
graph_store(envdata, store = 'SW', title = 'Coronation exhibition: SW case', breaks = '2 weeks', date_format = '%d/%m')
graph_light(envdata, store = 'SW', title = 'Coronation exhibition: SW case', breaks = '2 weeks', date_format = '%d/%m')
# BS4971 compliance graphs, set of three for low/good/high temp and RH and overall
graph_bs4971(rating_bs, "t")
graph_bs4971(rating_bs, "R")
graph_bs4971(rating_bs, "B")
            