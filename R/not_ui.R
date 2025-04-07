# Load the packages and function list used in this script
library(tidyverse)
source('R/funs.R')

# Site information ----
# Site name to use in tables and graph titles
site <- 'Anonymous Library'

# Abbreviation of site for filenames, using underscores rather than spaces
site_short <- 'AL'

# Currently supports 'tinytag', 'rotronic', 'trendbms', 
# 'tandd', 'miniclima', 'meaco', and 'previous' for csvs generated here.
# Tinytag and T&D files must be exported to csv before use, but Rotronic xls files can be used directly.
brand <- 'tinytag'

# Process data ----
# Copy the logfiles into dataloggergraphs-main/data, with folders for each brand.
# Make list of filenames
filenames <- dir('data/tinytag', full.names=TRUE)

# Get data from files. This runs as a loop for each file.
datalist <- lapply(filenames, parse_datalogger, site = site, brand = brand)

# Combine the data from the list of files into one dataframe
# If you have more than one brand of datalogger, use envdata_brand then combine in the next step.
envdata_raw <- combine_data(datalist)

# Combine the envdata_brand files
envdata <- bind_rows(envdata_tinytag, envdata_rotronic) %>%
  distinct()

# Clean data ----
# Remove potentially faulty readings. The defaults are 5-35C and 10-80%.
envdata_clean <- envdata_raw
envdata_clean <- remove_faulty(envdata_clean, max_temp = 30, min_temp = 8)

# Subset the readings if you are compiling data for a specific period
envdata_clean <- subset_readings(envdata_clean, start_date = '2024-01-01 00:00:00', end_date = '2024-12-31 23:59:59')

# Some Tinytags store the model as 'Tinytag Ultra H�C/%RH', which can be trimmed
envdata_clean$model <- str_remove(envdata_clean$model, ' H�C/%RH')

# Correct locations if the brand has restrictions on names, like character limits or not allowing spaces
envdata_clean$location <- str_replace_all(envdata_clean$location, "caseA", "case A")

# If you would like to extract a specific area or set of areas, create a new dataframe
envdata_exhib <- subset_readings(envdata_clean, store = 'Exhib')
# Or exclude an area
envdata_store <- subset_readings(envdata_clean, exclude_stores = 'Exhib')

# If would like the summary to use different names, create a new dataframe
envdata_store_rename <- envdata_store
envdata_store_rename$location <- str_replace_all(envdata_store_rename$location, 'Arch ', 'Archive ')

# If you have more than one logger in the space, numbered 'Archive 2B 1' and 'Archive 2B 2', remove the final number
# ^ represents the start of the name and $ represents the end, so ' 1$' selects 'Archive 2A 1' but not 'Archive 1A'
envdata_store_rename$location <- str_replace_all(envdata_store_rename$location, ' [1-3]$', '')

# Summarise ----
# Create tables with max/min/mean, mean and range, standard compliance, or light dose

# Default 'monthly', accepts 'annual' and 'daily'
# Includes the 1st and 99th percentile to trim unusual spikes
site_summary <- summarise_site(envdata_clean, type = 'daily')
# Default BS 4971, also accepts 'Icon', 'PAS 198', and 'Bizot'
# Or set own parameters
comp <- compliance(envdata_clean, standard = 'Icon' #,
                   #min_temp = 16, max_temp = 23, min_RH = 40, max_RH = 60
                   )
# Summary of light data with percentage of JNF for BW standards
# Can be limited to exhibition period with start_date and end_date
light <- light_dose(envdata_clean)

# Write .csv files of all data and tables ----
# Set standard and site and save
envdata <- envdata_clean
standard <- 'Icon'
site_short <- 'AL'
write_csv(envdata, paste0(date(min(envdata$datetime)),'_to_',
                          date(max(envdata$datetime)), '_data_', 
                          site_short, '.csv'))
write_csv(monthly_summary, paste0(date(min(envdata$datetime)),'_to_',
                                  date(max(envdata$datetime)), '_monthly_summary_', 
                                  site_short, '.csv'))
write_csv(annual_summary, paste0(date(min(envdata$datetime)),'_to_',
                                 date(max(envdata$datetime)), '_annual_summary_', 
                                 site_short, '.csv'))
write_csv(comp, paste0(date(min(envdata$datetime)),'_to_',
                       date(max(envdata$datetime)), '_', standard, '_compliance_', 
                       site_short, '.csv'))
write_csv(dose, paste0(date(min(dose$start_period)),'_to_',
                       date(max(dose$end_period)), '_light_', 
                       site_short, '.csv'))

# Graph ----
# Temperature and humidity graphs
# Graph all stores
# If no title specified, 'All stores at [site]'
# Accepts max_axis_temp and max_axis_RH, default 40 and 100
graph_store(envdata_clean, min_temp = 16, max_temp = 20, min_RH = 40, max_RH = 60,
            max_axis_RH = 80,
            exclude_stores = 'D',
            title = 'Anonymous Library main building')

# Graph single store
# Change the value of 'store' to graph a single store or group of stores sharing a pattern
# For example '7' graphs 'Archive 7', but 'Arch' graphs all spaces named 'Archive'
# Include a ^ at the beginning or $ at the end to match a certain part
# 'E$' will match 'Exhibition Case E' but not 'Exhibition Case B'
# The default title is the first location in the subset of the data, but can be changed
graph_store(envdata_clean, min_temp = 16, max_temp = 20, min_RH = 40, max_RH = 60,
            store = 'D',
            title = 'Anonymous Library extension')

# Graph summary
graph_summary(envdata_clean)

# Save the last generated plot
# Rerun this line with a different filename after each generated plot
ggsave('AA all stores 2025.png', scale = 1.5, width = 150, height = 100, units = 'mm')

# Lux and UV graphs, with customizable max_lux and max_UV
graph_light(envdata_exhib, store = 'Case E', title = 'Exhibition case E')

# Standard compliance graphs, set of three for low/good/high temp and RH and overall
# Set o_t_r to o for overall, t for temperature, or r for RH
# Include standard name or specify range
# Set custom max/min or standards 'BS 4971', 'PAS 198 25' or 'PAS 198 30' for max 25 or 30C,
#   'Icon' [2023 environmental guidance note], and 'Bizot'
graph_compliance(envdata_clean, o_t_r = 'o',
                 standard = 'Icon')
graph_compliance(envdata_clean, o_t_r = 't',
                 standard = 'Icon')
graph_compliance(envdata_clean, o_t_r = 'r',
                 standard = 'Icon')
