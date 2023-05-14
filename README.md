# dataloggergraphs
Adapts .csv files exported from different brands of datalogger to a common format, and combines them into a .csv file and graph.

Currently supports Tinytag, with Rotronic in progress. Also supports Trend BMS temperature and RH sensors, though very specific to site naming convention.
If you have another brand and a sample file, please feel free to adapt this and send a pull request.

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
