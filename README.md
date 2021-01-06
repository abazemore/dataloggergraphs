# dataloggergraphs
Adapts .csv files exported from different brands of datalogger to a common format, and combines them into one .csv file and graph.
This was originally intended to create annual graphs for one monitor at a time, so the code combines and graphs a folder of .csv files without distinguishing the serial number or location.

Currently supports Tinytag, with Rotronic in progress, and will then work on separate BMS temperature and RH sensors.
If you have another brand and a sample file, please feel free to adapt this and send a pull request.

Columns and types are:
id        (int, optional)
time      (POSIXct)
temp      (dbl)
RH        (dbl)
dew_point (dbl, optional)
model     (str)
serial    (str)
location  (str)

