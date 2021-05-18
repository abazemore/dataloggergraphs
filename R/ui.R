# Load R packages
library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  "Datalogger summary and graphs",
                  tabPanel("Load data",
                           sidebarPanel(
                             h4("Site information"),
                             p(" If you have more than one site, please load them separately."),
                             textInput("site", "Site name:", ""),
                             textInput("site_short", "Abbreviation for filenames:", ""),
                              selectInput("brand", "Datalogger brand:", 
                                         c("Rotronic" = "rotronic",
                                           "Tinytag" = "tinytag",
                                           "Trend BMS" = "trendbms")),
                             fileInput("files", "Upload files (.csv or .xlsx)",
                                       multiple = TRUE),
                             actionButton("submit", "Submit")
                             
                           ), # sidebarPanel
                           mainPanel(
                             h3("Uploaded data"),
                             dataTableOutput("alldata"),
                             #downloadButton("download_all", "Download .csv"),
                             tableOutput("summary")
                             # plotOutput("allsites"),
                             # dataTableOutput("envdata"),
                             # downloadButton("envdata_csv", "Download .csv"),
                             # 
                             # h4("Site summary by month"),
                             # dataTableOutput("site_summary"),
                             # downloadButton("site_summary_csv", "Download .csv"),
                             # tableOutput("site_summary")
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Summary by month", 
                           mainPanel(

                             
                           )),
                  tabPanel("Environmental graphs", "This panel is intentionally left blank"),
                  tabPanel("BS4971 compliance", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage

