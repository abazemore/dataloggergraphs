library(shiny)
library(shinythemes)
library(readxl)
source("funs.R")

# Define server function  
server <- function(input, output) {
  # Load files and parse


  getdata <- eventReactive(input$submit, {
    message("Processing files")
    req(input$files)
    num_files <- nrow(input$files)
    for(i in 1:num_files) {
      parse_datalogger(input$files$datapath, input$site, input$brand) %>%
      combine_data(input$brand)
    }
    # lapply(input$files$datapath, parse_datalogger(site = input$site, brand = input$brand))
  })

  
  output$alldata <- renderDataTable( {req(envdata())
    envdata()} )
  output$summary <- renderTable ( {
    req(envdata())
        summarise_site(envdata())
    } )
  # output$download_all <- downloadHandler(
  #   filename = str_c(date(min(envdata$datetime),"_to_",
  #                                            date(max(envdata$datetime), "_summary_", 
  #                                            input$site_short, ".csv"),
  #   # filename = function() {
    #   str_c(get_daterange(envdata(), input$site_short), "_data.csv")
    # },
  #   content = function(file) {
  #     write.csv(data(), file)
  #   }
  # )
  site_summary <- eventReactive(input$files, 
                                summarise_site(envdata()))

  output$envdata <- renderDataTable({ head(envdata()) },
                                    options = list(
                                      pageLength = 10
                                    ))
  output$site_summary <- renderDataTable({ summarise_site(data) })
  output$allsites <- renderPlot({ plot_store(envdata(), all_stores = TRUE)})


    # 
    #     
    #     # Downloadable csv of selected dataset ----
    #     output$envdata_csv <- downloadHandler(
    #       filename = str_c(date(min(envdata$datetime)),"_to_",
    #                        date(max(envdata$datetime)), "_data_", 
    #                        input$site_short, ".csv"),
    #       content = write.csv(envdata),
    #       contentType = "text/csv"
    #     )
    #     output$site_summary_csv <- downloadHandler(
    #       filename = str_c(date(min(envdata$datetime)),"_to_",
    #                        date(max(envdata$datetime)), "_summary_", 
    #                        input$site_short, ".csv"),
    #       content = write.csv(site_summary),
    #       contentType = "text/csv"
    #     )

 
}