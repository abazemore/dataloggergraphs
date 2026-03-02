library(shiny)
library(shinythemes)
library(DT)
source('funs.R')
envdata <- tibble(venue = character(),
                  location = character(),
                  datetime = POSIXct(),
                  temp = double(),
                  RH = double(),
                  model = character(),
                  serial = character())
# Define server function
server <- function(input, output) {

  # Tab 1: Load files and parse ----

# envdata <- eventReactive(input$submit, {
#   req(input$append == 'sample')
#   message('Using sample data')
#   read_csv('../data/sample_data.csv')
# })

observeEvent(input$submit, {
  message('Creating list of files')
  if(input$append == 'sample') { datalist <- c('../data/previous/sample_data.csv') }
  else if(input$files) {
  datalist <- input$files$datapath
  }
    else { message('Loading failed') }
  }
})

envdata <- eventReactive(input$submit, {
    message('Processing files')
    req(datalist())
# Take file list and parse each file then combine
    lapply(datalist(), parse_datalogger,
           site = input$site, brand = input$brand) %>%
  combine_data()
  })

  # Prepare download button
  output$prep_download_data <- downloadHandler(
    filename = function() {str_c(date(min(envdata()$datetime)),'_to_',
                                 date(max(envdata()$datetime)), '_data_',
                                 input$site_short, '.csv')},
    content = function(file) {
      write.csv(envdata(), file)
    }
  )
  # Only show download button when data is available
  output$download_data <- renderUI({
    req(envdata())
    downloadButton('prep_download_data', 'Download all data')
  })

  # Summary
  site_summary <- eventReactive(input$submit, {
    message('Preparing summary')
    req(envdata())
    summarise_site(envdata())
  })

  output$summary <- DT::renderDT ( {
        site_summary()
    } )

# Prepare download button
  output$prep_download_summary <- downloadHandler(
    filename = function() {str_c(date(min(envdata()$datetime)),'_to_',
                                 date(max(envdata()$datetime)), '_summary_',
                                 input$site_short, '.csv')},
    content = function(file) {
      write.csv(site_summary(), file)
    }
  )
  # Only show download button when data is available
  output$download_summary <- renderUI({
    req(site_summary())
    downloadButton('prep_download_summary', 'Download summary')
  })


  # Environmental graphs tab ----

  # Sidebar dropdown unique values in summary for different stores
  output$unique_stores <- renderUI({
    req(site_summary())
    radioButtons('store', 'Select store to graph:',
                 choices = unique(site_summary()$location))
  })
  output$graph_type <- renderUI({
    req(site_summary())
    radioButtons('graph_type', 'Graph type:',
                 choices = c('All data' = 'all',
                             'Monthly summary' = 'summary'))
  })
  # Sidebar date range
  output$input_daterange <- renderUI({
    req(envdata())
    dateRangeInput('daterange', 'Date range:',
                   start = min(envdata()$datetime),
                   end = max(envdata()$datetime),
                   min = min(envdata()$datetime),
                   max = max(envdata()$datetime),)

  })
  output$allstores_graph <- renderPlot({
    req(envdata(), input$daterange)
    if(input$graph_type == 'summary') {
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
     if(input$graph_type == 'summary') {
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


  # Standard compliance tab ----

  # Sidebar date range
  output$input_daterange_comp <- renderUI({
    req(envdata())
    dateRangeInput('daterange_comp', 'Date range:',
                   start = min(envdata()$datetime),
                   end = max(envdata()$datetime),
                   min = min(envdata()$datetime),
                   max = max(envdata()$datetime),)

  })
  comp <- reactive({
    message('Preparing compliance summary')
    req(envdata())
    compliance(envdata(),
           start_date = input$daterange_comp[1],
           end_date = input$daterange_comp[2])
  })
  # Download compliance summary button
  # Prepare download button
  output$prep_compsum <- downloadHandler(
    filename = function() {str_c(date(min(envdata()$datetime)),'_to_',
                                 date(max(envdata()$datetime)), '_compliance_summary_',
                                 input$site_short, '.csv')},
    content = function(file) {
      write.csv(comp(), file)
    }
  )
  # Only show download button when data is available
  output$download_compsum <- renderUI({
    req(comp())
    downloadButton('prep_compsum', 'Download summary (.csv)')
  })

  # Prepare download button
  output$prep_compgraphs <- downloadHandler(
    filename = function() {str_c(date(min(envdata()$datetime)),'_to_',
                                 date(max(envdata()$datetime)), '_compliance_graphs_',
                                 input$site_short, '.csv')},
    content = function(file) {
      write.csv(comp(), file)
    }
  )
  # Only show download button when data is available
  output$download_compgraphs <- renderUI({
    req(comp())
    downloadButton('prep_download_compgraph', 'Download graphs (.zip)')
  })
  output$comp_table <- DT::renderDT( comp() )
  output$comp_graph <- renderPlot( graph_comp(comp(), 'B'))
  output$comp_temp <- renderPlot( graph_comp(comp(), 't'))
  output$comp_RH <- renderPlot( graph_comp(comp(), 'R'))


  # Light data tab ----
  # On hold until webapp can use original filenames to extract location and serial.
  output$input_daterange_light <- renderUI({
    req(envdata())
    dateRangeInput('daterange', 'Date range:',
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

  output$lightdose <- DT::renderDT ( {
    light_dose(envdata(),
               start_date = input$daterange[1],
               end_date = input$daterange[2])
  } )

}
