# Load R packages
library(shiny)


# Define UI
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
                navbarPage(
                  "Datalogger summary and graphs",
                  tabPanel("Data summary",
                           sidebarPanel(
                             h4("Site information"),
                             p("If you have more than one site, please load them separately."),
                             textInput("site", "Site name:", ""),
                             textInput("site_short", "Abbreviation for filenames:", ""),
                             selectInput("brand", "Datalogger brand:",
                                         c("Meaco" = "meaco",
                                           "miniClima" = "miniclima",
                                           "Rotronic" = "rotronic",
                                           "T&D" = "tandd",
                                           "Tinytag" = "tinytag",
                                           "Trend BMS" = "trendbms",
                                           "Previously processed" = "previous")),
                             fileInput("files", "Upload files (.csv, .xls, or .zip)",
                                       multiple = TRUE, accept = c("csv", "xls", "zip")),
                              checkboxInput("sample", "Use sample data"),
                             # radioButtons("append",
                             #              "",
                             #              choices = c("Append data" = "append",
                             #                          "Replace data" = "replace",
                             #                          "Use sample data" = "sample")),
                             checkboxInput("percentile", "Include 1st and 99th percentile in summary"),
                             actionButton("submit", "Submit")
                           ), # sidebarPanel
                           mainPanel(
                             h3("Data summary"),
                             splitLayout(uiOutput("download_data"),
                                         uiOutput("download_summary")),
                             br(),
                             DT::DTOutput("summary")
                           ) # mainPanel

                  ), # Data summary, tabPanel
                  tabPanel("Environmental graphs",
                           sidebarPanel(
                             uiOutput("unique_stores"),
                             selectInput("graph_type", "Graph type: ",
                                         c("All data" = "all",
                                           "Summary" = "summary")),
                             conditionalPanel(condition = "input.graph_type == 'summary'",
                                              selectInput("summary_type", "Summary type: ",
                                                          c("Annual" = "annual",
                                                            "Monthly" = "monthly",
                                                            "Weekly" = "weekly",
                                                            "Daily" = "daily"))),
                             uiOutput("input_daterange")
                           ),
                           mainPanel(
                             plotOutput("allstores_graph"),
                             plotOutput("singlestore_graph")
                           )), # Environmental graphs, tabPanel
                  tabPanel("Standard compliance",
                           sidebarPanel(
                             selectInput("standard",
                                         "Standard: ",
                                         c("BS 4971",
                                         "PAS 198",
                                         "Icon",
                                         "Bizot",
                                         "Custom")),
                             conditionalPanel(condition = "input.standard == 'Custom'",
                             splitLayout(
                               numericInput("min_temp", "Min temp:", 5),
                              numericInput("max_temp", "Max temp:", 23)
                              ),
                             splitLayout(
                               numericInput("min_temp", "Min RH:", 35),
                              numericInput("max_temp", "Max RH:", 65)
                              )
                             ),
                             uiOutput("input_daterange_comp")
                           ),
                           mainPanel(
                             uiOutput("download_compsum"),
                             plotOutput("comp_graph"),
                             plotOutput("comp_temp"),
                             plotOutput("comp_RH"),
                             DT::DTOutput("comp_table")
                           )) # Compliance, tabPanel
                  # Light data tab on hold until I can use the original filename to detect the location.
                  # tabPanel("Light data",
                  #          sidebarPanel(
                  #            uiOutput("input_daterange_light")
                  #          ),
                  #          mainPanel(
                  #            plotOutput("allstores_light"),
                  #            DT::DTOutput("lightdose")
                  #          )) # Light data, tabPanel
                ) # navbarPage
) # fluidPage

