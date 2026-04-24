library(shiny)

# Define server function
server <- function(input, output) {
  source("parse.R")
  source("tidy.R")
  source("summarize.R")
  source("graph.R")
  
  # Tab 1: Load files and parse ----
  envdata <- reactive({
    if (input$sample == TRUE) {
      if("dataloggergraphs" %in% installed.packages()[, "Package"]) {
        dataloggergraphs::sample_data
      } else {
        parse_previous("https://averybazemore.com/stuff/sample_data.csv")
      }
    } else if (length(input$files) > 0) {
      parse_brand(input$files,
                  brand = input$brand,
                  site = input$site)
    }


  }) |>
    bindEvent(input$submit)


  # Prepare download button
  output$prep_download_data <- downloadHandler(
    filename = function() {
      stringr::str_c(date(min(envdata()$datetime)),
                     "_to_",
                     date(max(envdata()$datetime)),
                     "_data_",
                     input$site_short,
                     ".csv")
    },
    content = function(file) {
      utils::write.csv(envdata(), file)
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
    req(envdata)
    summarize_site(envdata(), percentile = input$percentile)
  })

  output$summary <- DT::renderDT ({
    site_summary()
  })

  # Prepare download button
  output$prep_download_summary <- downloadHandler(
    filename = function() {
      stringr::str_c(date(min(envdata()$datetime)),
                     "_to_",
                     date(max(envdata()$datetime)),
                     "_summary_",
                     input$site_short,
                     ".csv")
    },
    content = function(file) {
      utils::write.csv(site_summary(), file)
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
    radioButtons("store",
                 "Select store to graph:",
                 choices = unique(site_summary()$location))
  })
  output$graph_type <- renderUI({
    req(site_summary)
    radioButtons("graph_type",
                 "Graph type:",
                 choices = c("All data" = "all", "Summary" = "summary"))
  })
  # Sidebar date range
  output$input_daterange <- renderUI({
    req(envdata)
    dateRangeInput(
      "daterange",
      "Date range:",
      start = min(envdata()$datetime),
      end = max(envdata()$datetime),
      min = min(envdata()$datetime),
      max = max(envdata()$datetime),
    )

  })
  output$allstores_graph <- renderPlot({
    req(envdata, input$daterange)
    if (input$graph_type == "summary") {
      if ("min_temp" %in% names(site_summary())) {
        message("trh")
        graph_summary(
          envdata(),
          type = input$summary_type,
          percentile = input$percentile,
          start_date = input$daterange[1],
          end_date = input$daterange[2]
        )
      }
      else if ("min_lux" %in% names(site_summary())) {
        message("lux")
        graph_light_summary(
          envdata(),
          type = input$summary_type,
          percentile = input$percentile
          # start_date = input$daterange[1],
          # end_date = input$daterange[2]
        )
      }
    }
    else {
      if ("min_temp" %in% names(site_summary())) {
        graph_store(
          envdata(),
          start_date = input$daterange[1],
          end_date = input$daterange[2]
        )
      }
      else if ("min_lux" %in% names(site_summary())) {
        graph_light(
          envdata(),
          start_date = input$daterange[1],
          end_date = input$daterange[2]
        )
      }
    }
  })
  output$singlestore_graph <- renderPlot({
    req(envdata, input$store)
    if (input$graph_type == "summary") {
        graph_summary(
          envdata(),
          store = input$store,
          start_date = input$daterange[1],
          end_date = input$daterange[2],
          type = input$summary_type,
          percentile = input$percentile
        )
    } else {
      graph_store(
        envdata(),
        store = input$store,
        start_date = input$daterange[1],
        end_date = input$daterange[2]
      )
    }

  })


  # Standard compliance tab ----

  # Sidebar date range
  output$input_daterange_comp <- renderUI({
    req(envdata)
    dateRangeInput(
      "daterange_comp",
      "Date range:",
      start = min(envdata()$datetime),
      end = max(envdata()$datetime),
      min = min(envdata()$datetime),
      max = max(envdata()$datetime),
    )

  })
  comp <- reactive({
    message("Preparing compliance summary")
    req(envdata)
    compliance(
      envdata(),
      start_date = input$daterange_comp[1],
      end_date = input$daterange_comp[2]
    )
  })
  # Download compliance summary button
  # Prepare download button
  output$prep_compsum <- downloadHandler(
    filename = function() {
      stringr::str_c(
        date(min(envdata()$datetime)),
        "_to_",
        date(max(envdata()$datetime)),
        "_compliance_summary_",
        input$site_short,
        ".csv"
      )
    },
    content = function(file) {
      utils::write.csv(comp(), file)
    }
  )
  # Only show download button when data is available
  output$download_compsum <- renderUI({
    req(comp)
    downloadButton("prep_compsum", "Download summary (.csv)")
  })

  # Prepare download button
  output$prep_compgraphs <- downloadHandler(
    filename = function() {
      stringr::str_c(date(min(envdata()$datetime)),
                     "_to_",
                     date(max(envdata()$datetime)),
                     "_compliance_graphs_",
                     input$site_short,
                     ".csv")
    },
    content = function(file) {
      utils::write.csv(comp(), file)
    }
  )
  # Only show download button when data is available
  output$download_compgraphs <- renderUI({
    req(comp)
    downloadButton("prep_download_compgraph", "Download graphs (.zip)")
  })
  output$comp_table <- DT::renderDT(comp())
  output$comp_graph <- renderPlot(graph_compliance(envdata(), "o", standard = input$standard,
                                                   start_date = input$daterange_comp[1],
                                                   end_date = input$daterange_comp[2]))
  output$comp_temp <- renderPlot(graph_compliance(envdata(), "t", standard = input$standard,
                                 start_date = input$daterange_comp[1],
                                 end_date = input$daterange_comp[2]))
  output$comp_RH <- renderPlot(graph_compliance(envdata(), "r", standard = input$standard,
                               start_date = input$daterange_comp[1],
                               end_date = input$daterange_comp[2]))


  # Light data tab ----
  # On hold until webapp can use original filenames to extract location and serial.
  output$input_daterange_light <- renderUI({
    req(envdata)
    dateRangeInput(
      "daterange",
      "Date range:",
      start = min(envdata()$datetime),
      end = max(envdata()$datetime),
      min = min(envdata()$datetime),
      max = max(envdata()$datetime),
    )

  })
  output$allstores_light <- renderPlot({
    req(envdata, input$daterange)
    graph_light(envdata(),
                start_date = input$daterange[1],
                end_date = input$daterange[2])
  })
  output$singlestore_light <- renderPlot({
    req(envdata, input$daterange)
    graph_light(
      envdata(),
      store = input$store,
      start_date = input$daterange[1],
      end_date = input$daterange[2]
    )
  })

  output$lightdose <- DT::renderDT ({
    light_dose(envdata,
               start_date = input$daterange[1],
               end_date = input$daterange[2])
  })

}
