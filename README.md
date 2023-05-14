# dataloggergraphs
Adapts .csv files exported from different brands of datalogger to a common format, and combines them into a .csv file and graph.

Currently supports Tinytag and Rotronic temperature and humidity loggers, Trend BMS with a site-specific naming convention, and T&D light monitors.
If you have another brand and a sample file, please feel free to open an issue or send a pull request.

Columns and types are:

venue     (char)

location  (char)

datetime  (date, POSIXct)

temp      (dbl)

RH        (dbl)

lux      (dbl) for light logger brands only

UV        (dbl) for light logger brands only

model     (str)

serial    (str)

There is also a move script, which compiles annual files from different sites and makes a graph splicing two stores.
