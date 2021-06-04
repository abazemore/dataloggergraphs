# Load R packages
library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  "Datalogger summary and graphs",
                  tabPanel("Data summary",
                           sidebarPanel(
                             h4("Site information"),
                             p("If you have more than one site, please load them separately."),
                             textInput("site", "Site name:", ""),
                             textInput("site_short", "Abbreviation for filenames:", ""),
                              selectInput("brand", "Datalogger brand:", 
                                         c("Rotronic" = "rotronic",
                                           "Tinytag" = "tinytag",
                                           "Trend BMS" = "trendbms")),
                             uiOutput("ifrotronic"),
                             fileInput("files", "Upload files (.csv)",
                                       multiple = TRUE),
                             p("If you are uploading Rotronic files, 
                                   the .xls format cannot be read. Please open them in Excel 
                                   and save them as CSV UTF-8."),
                             actionButton("submit", "Submit")
                             
                           ), # sidebarPanel
                           mainPanel(
                             h3("Data summary"),
                             #dataTableOutput("alldata"),
                             splitLayout(uiOutput("download_data"),
                             uiOutput("download_summary")),
                             br(),
                             dataTableOutput("summary")
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Environmental graphs",
                           sidebarPanel(
                             uiOutput("unique_stores"),
                             uiOutput("graph_type"),
                             uiOutput("input_daterange")
                           ),
                           mainPanel(
                             plotOutput("allstores_graph"),
                             plotOutput("singlestore_graph")
                           )),
                  tabPanel("BS4971 compliance",
                           mainPanel(
                             uiOutput("download_bssum"),
                             plotOutput("bs4971_graph"),
                             plotOutput("bs4971_temp"),
                             plotOutput("bs4971_RH"),
                             dataTableOutput("bs4971_table")
                           ))
                  
                ) # navbarPage
) # fluidPage

