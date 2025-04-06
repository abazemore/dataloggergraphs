library(shiny)
library(shinythemes)
source("funs.R")

# Define server function  
server <- function(input, output) {
  
  # Tab 1: Load files and parse ----
  # output$contents <- renderTable({
  #   file <- input$file1
  #   ext <- tools::file_ext(file$datapath)
  #   
  #   req(file)
  #   validate(need(ext == "csv", "Please upload a csv file"))
  #   
  #   read.csv(file$datapath, header = input$header)
  # })
datalist <- eventReactive(input$submit, {
  message("Creating list of files")
  req(input$files)
  input$files$datapath
})
  envdata <- eventReactive(input$submit, {
    message("Processing files")
    req(datalist())
# Take file list and parse each file then combine
    lapply(datalist(), parse_datalogger,
           site = input$site, brand = input$brand) %>%
  combine_data(input$brand)
  })
  
  # Prepare download button
  output$prep_download_data <- downloadHandler(
    filename = function() {str_c(date(min(envdata()$datetime)),"_to_",
                                 date(max(envdata()$datetime)), "_data_",
                                 input$site_short, ".csv")},
    content = function(file) {
      write.csv(envdata(), file)
    }
  )
  # Only show download button when data is available
  output$download_data <- renderUI({
    req(envdata())
    downloadButton("prep_download_data", "Download all data")
  })
  
  # Summary
  site_summary <- eventReactive(input$submit, {
    message("Preparing summary")
    req(envdata())
    summarise_site(envdata())
  })

  output$summary <- renderDataTable ( {
        site_summary()
    } )

# Prepare download button
  output$prep_download_summary <- downloadHandler(
    filename = function() {str_c(date(min(envdata()$datetime)),"_to_",
                                 date(max(envdata()$datetime)), "_summary_",
                                 input$site_short, ".csv")},
    content = function(file) {
      write.csv(site_summary(), file)
    }
  )
  # Only show download button when data is available
  output$download_summary <- renderUI({
    req(site_summary())
    downloadButton("prep_download_summary", "Download summary")
  })
  
  
  # Environmental graphs tab ----

  # Sidebar dropdown unique values in summary for different stores
  output$unique_stores <- renderUI({
    req(site_summary())
    radioButtons("store", "Select store to graph:",
                 choices = unique(site_summary()$location))
  })
  output$graph_type <- renderUI({
    req(site_summary())
    radioButtons("graph_type", "Graph type:",
                 choices = c("All data" = "all", 
                             "Monthly summary" = "summary"))
  })
  # Sidebar date range
  output$input_daterange <- renderUI({
    req(envdata())
    dateRangeInput("daterange", "Date range:",
                   start = min(envdata()$datetime),
                   end = max(envdata()$datetime),
                   min = min(envdata()$datetime),
                   max = max(envdata()$datetime),)
    
  })
  output$allstores_graph <- renderPlot({ 
    req(envdata(), input$daterange)
    if(input$graph_type == "summary") {
        graph_summary(site_summary(),
                      start_date = input$daterange[1],
                      end_date = input$daterange[2])
      }
    else {
      graph_store(envdata(),
                  start_date = input$daterange[1],
                  end_date = input$daterange[2]
                  )
    }
        })
  output$singlestore_graph <- renderPlot({
    req(envdata(), input$store)
     if(input$graph_type == "summary") {
       graph_summary(site_summary(), store = input$store,
                     start_date = input$daterange[1],
                     end_date = input$daterange[2])
       
     }
    else {
      graph_store(envdata(), store = input$store,
                  start_date = input$daterange[1],
                  end_date = input$daterange[2])
    }
  })
  

  # BS4971 compliance tab ----
  
  # Sidebar date range
  output$input_daterange_bs <- renderUI({
    req(envdata())
    dateRangeInput("daterange_bs", "Date range:",
                   start = min(envdata()$datetime),
                   end = max(envdata()$datetime),
                   min = min(envdata()$datetime),
                   max = max(envdata()$datetime),)
    
  })
  bs <- reactive({
    message("Preparing BS4971 summary")
    req(envdata())
    bs4971(envdata(), 
           start_date = input$daterange_bs[1],
           end_date = input$daterange_bs[2])
  })
  # Download BS4971 summary button
  # Prepare download button
  output$prep_bssum <- downloadHandler(
    filename = function() {str_c(date(min(envdata()$datetime)),"_to_",
                                 date(max(envdata()$datetime)), "_BS4971_summary_",
                                 input$site_short, ".csv")},
    content = function(file) {
      write.csv(bs(), file)
    }
  )
  # Only show download button when data is available
  output$download_bssum <- renderUI({
    req(bs())
    downloadButton("prep_bssum", "Download summary (.csv)")
  })

  # Prepare download button
  output$prep_bsgraphs <- downloadHandler(
    filename = function() {str_c(date(min(envdata()$datetime)),"_to_",
                                 date(max(envdata()$datetime)), "_BS4971_graphs_",
                                 input$site_short, ".csv")},
    content = function(file) {
      write.csv(bs(), file)
    }
  )
  # Only show download button when data is available
  output$download_bsgraphs <- renderUI({
    req(bs())
    downloadButton("prep_download_bsgraph", "Download graphs (.zip)")
  })
  output$bs4971_table <- renderDataTable( bs() )
  output$bs4971_graph <- renderPlot( graph_bs4971(bs(), "B"))
  output$bs4971_temp <- renderPlot( graph_bs4971(bs(), "t"))
  output$bs4971_RH <- renderPlot( graph_bs4971(bs(), "R"))
  
  
  # Light data tab ----
  output$input_daterange_light <- renderUI({
    req(envdata())
    dateRangeInput("daterange", "Date range:",
                   start = min(envdata()$datetime),
                   end = max(envdata()$datetime),
                   min = min(envdata()$datetime),
                   max = max(envdata()$datetime),)
    
  })
  output$allstores_light <- renderPlot({
    req(envdata(), input$daterange)
      graph_light(envdata(),
                    start_date = input$daterange[1],
                    end_date = input$daterange[2])
  })
  output$singlestore_light <- renderPlot({
    req(envdata(), input$daterange)
    graph_light(envdata(), store = input$store,
                start_date = input$daterange[1],
                end_date = input$daterange[2])
  })

  output$lightdose <- renderDataTable ( {
    light_dose(envdata(),
               start_date = input$daterange[1],
               end_date = input$daterange[2])
  } )
  
}
