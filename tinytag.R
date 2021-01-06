library(tidyverse)

#Data should have the columns:
#   id (int, optional)
#   time (POSIXct)
#   temp (dbl)
#   RH (dbl)
#   dew_point (dbl, optional)
#   model (str)
#   serial_number (str)
#   location (str)

#Place logger files exported as .csv into folder "tinytag"

#Make list of filenames
filenames <- dir("tinytag", full.names=TRUE)
#Get data from files. This runs as a loop for each file.
datalist <- lapply(filenames, FUN = function(datafile) {
  #Extract first few rows containing logger information
  fileHead <- read_csv(filenames, 
                       col_names = c("id", "time", "temp", "RH", "dew_point")) # %>%
    head(22)
  #Rest of file is observations
  envdata <- read_csv(filenames, 
                      col_names = c("id", "time", "temp", "RH", "dew_point"),
                      skip = 5) %>%
    #Add model, serial, and location columns extracted from first rows
    mutate(model = fileHead$temp[3],
           serial_no = fileHead$temp[2], location = fileHead$temp[4]) %>%
    #Extract numeric value from temp, RH, and dew point columns
    mutate(temp = str_extract_all(temp, "[:digit:]+\\.[:digit:]+"),
           RH = str_extract_all(RH, "[:digit:]+\\.[:digit:]+"),
           dew_point = str_extract_all(dew_point, "[:digit:]+\\.[:digit:]+"),
    ) %>%
    #Read as numeric value instead of string
    mutate(temp = as.numeric(temp), 
           RH = as.numeric(RH),
           dew_point = as.numeric(dew_point),
           time = as.POSIXct(time)) %>% #Parse as date/time
    drop_na() #Drop rows with no reading
})
envdata <- bind_rows(datalist) #Combine data from each file into one list
write_csv(envdata, "envdata.csv")

#Create graph with time on x axis, temperature on left y axis, and RH on right y axis
#Y scales 0-40º and 0-100%
envdata %>% ggplot(mapping = aes(x = time)) + 
  geom_path(aes(y = temp), color = "red") +
  geom_path(aes(y = RH/2.5), color = "blue") +
  scale_x_datetime(name = "Date", date_breaks = "3 months", date_labels = "%m/%Y") +
  scale_y_continuous(
    name = "Temperature (º C)",
    limits = c(0,40),
    sec.axis = sec_axis(~ .*2.5, name = "Rel. Humidity (%)"))
