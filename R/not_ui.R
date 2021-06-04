library(tidyverse)
source("R/funs.R")

# Site information ----
# Site name to use in tables and graph titles
site <- ""
# Abbreviation of site for filenames, use underscores rather than spaces
site_short <- ""
# Currently supports "tinytag", "rotronic", and "trendbms"
# Rotronic files must be converted to .csv because the .xls files are not readable
brand <- ""
graph_summary(site_summary)

# Process the data and create summaries ----
# Make list of filenames
filenames <- dir("data/bms", full.names=TRUE)
#Get data from files. This runs as a loop for each file.
datalist <- lapply(filenames, parse_datalogger, site = site, brand = brand)
envdata <- combine_data(datalist, brand)
movetest <- graph_move(envdata1,envdata2,store1,store2,move_date,start_date,end_date)
movetest
# Create tables with store max/min/mean and BS4971 compliance
site_summary <- summarise_site(envdata)
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

# Graph stores ----
# Temperature and humidity graphs
all_stores_graph <- graph_store(envdata)
# Change the value of "store" to graph a single store or group of stores sharing a pattern
# For example "7" graphs "Archive 7", but "Arch" graphs all archive spaces
store_graph <- graph_store(envdata, store = "7")
# BS4971 compliance graphs, set of three for low/good/high temp and RH and overall
bs_t <- graph_bs4971(rating_bs, "t")
bs_r <- graph_bs4971(rating_bs, "R")
bs_B <- graph_bs4971(rating_bs, "B")
all_stores_graph
store_graph
bs_t
bs_r
bs_B
