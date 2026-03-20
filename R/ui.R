# Load R packages
library(shiny)


# Define UI
ui <- fluidPage(theme = shinythemes::shinytheme('flatly'),
                navbarPage(
                  'Datalogger summary and graphs',
                  tabPanel('Data summary',
                           sidebarPanel(
                             h4('Site information'),
                             p('If you have more than one site, please load them separately.'),
                             textInput('site', 'Site name:', ''),
                             textInput('site_short', 'Abbreviation for filenames:', ''),
                             selectInput('brand', 'Datalogger brand:',
                                         c('Meaco' = 'meaco',
                                           'miniClima' = 'miniclima',
                                           'Rotronic' = 'rotronic',
                                           'T&D' = 'tandd',
                                           'Tinytag' = 'tinytag',
                                           'Trend BMS' = 'trendbms',
                                           'Previously processed' = 'previous')),
                             fileInput('files', 'Upload files (.csv)',
                                       multiple = TRUE),
                             checkboxInput('sample', 'Use sample data'),
                             # radioButtons('append',
                             #              '',
                             #              choices = c('Append data' = 'append',
                             #                          'Replace data' = 'replace',
                             #                          'Use sample data' = 'sample')),
                             actionButton('submit', 'Submit')

                           ), # sidebarPanel
                           mainPanel(
                             h3('Data summary'),
                             splitLayout(uiOutput('download_data'),
                                         uiOutput('download_summary')),
                             br(),
                             DT::DTOutput('summary')
                           ) # mainPanel

                  ), # Data summary, tabPanel
                  tabPanel('Environmental graphs',
                           sidebarPanel(
                             uiOutput('unique_stores'),
                             uiOutput('graph_type'),
                             uiOutput('input_daterange')
                           ),
                           mainPanel(
                             plotOutput('allstores_graph'),
                             plotOutput('singlestore_graph')
                           )), # Environmental graphs, tabPanel
                  tabPanel('Standard compliance',
                           sidebarPanel(
                             uiOutput('input_daterange_comp')
                           ),
                           mainPanel(
                             uiOutput('download_compsum'),
                             plotOutput('comp_graph'),
                             plotOutput('comp_temp'),
                             plotOutput('comp_RH'),
                             DT::DTOutput('comp_table')
                           )) # Compliance, tabPanel
                  # Light data tab on hold until I can use the original filename to detect the location.
                  # tabPanel('Light data',
                  #          sidebarPanel(
                  #            uiOutput('input_daterange_light')
                  #          ),
                  #          mainPanel(
                  #            plotOutput('allstores_light'),
                  #            DT::DTOutput('lightdose')
                  #          )) # Light data, tabPanel
                ) # navbarPage
) # fluidPage

