library(leaflet)

ui <- navbarPage("Data Visualizations",
                 tabPanel("Tables",
                          titlePanel("Global Partnership and Paris Declaration Data"),                        
                          tabsetPanel(              
                            tabPanel(title = "Recipient-Donor Data",
                                     downloadButton('downloadData1', 'Download'),
                                     # Create a new Row in the UI for selectInputs
                                     fluidRow(
                                       column(4,
                                              selectInput("Donors1",
                                                          "Donor:",
                                                          c("All",
                                                            sort(unique(as.character(Data$Donor)))))
                                       ),
                                       column(4,
                                              selectInput("Recipients1",
                                                          "Recipient:",
                                                          c("All",
                                                            sort(unique(as.character(Data$Recipient)))))
                                       ),
                                       column(4,
                                              selectInput("Year1",
                                                          "Year:",
                                                          c("All",
                                                            sort(unique(Data$Year))))
                                       )
                                     ),
                                     
                                     # Create a new row for the table.
                                     fluidRow(
                                       dataTableOutput("table1")
                                     )
                            ),
                            
                            tabPanel(title = "Recipients Data",
                                     downloadButton('downloadData2', 'Download'),
                                     # Create a new Row in the UI for selectInputs
                                     fluidRow(
                                       column(4,
                                              selectInput("Recipients2",
                                                          "Recipient:",
                                                          c("All",
                                                            sort(unique(as.character(Recipients$Recipient)))))
                                       ),
                                       column(4,
                                              selectInput("Year2",
                                                          "Year:",
                                                          c("All",
                                                            sort(unique(Recipients$Year))))
                                       )
                                     ),
                                     # Create a new row for the table.
                                     fluidRow(
                                       
                                       dataTableOutput("table2")
                                       
                                     )
                            ),
                            
                            tabPanel(title = "Donors Data",
                                     downloadButton('downloadData3', 'Download'),
                                     # Create a new Row in the UI for selectInputs
                                     fluidRow(
                                       column(4,
                                              selectInput("Donors2",
                                                          "Donors:",
                                                          c("All",
                                                            sort(unique(as.character(Donors$Donor)))))
                                              
                                       ),
                                       column(4,
                                              selectInput("Year3",
                                                          "Year:",
                                                          c("All",
                                                            sort(unique(Donors$Year))))
                                       )
                                     ),
                                     # Create a new row for the table.
                                     fluidRow(
                                       dataTableOutput("table3")
                                     )
                            )
                          )
                 ),
                 tabPanel("Maps",
                          tags$style(type = "text/css", "html, body {width:100%;height:625"),
                          leafletOutput("map", width = "100%", height = 625),
                          absolutePanel(top = 75, right = 25,
                                        selectInput("input_map_indicator", "Indicator:", 
                                                    choices = c('Indicator 5a, As Scheduled'= 1,
                                                                'Indicator 5a,  Beyond Scheduled'= 2,
                                                                'Indicator 6, Of Sheduled'= 3,
                                                                'Indicator 6, Beyond Scheduled'= 4,
                                                                'Indicator 9b' = 5)),
                                        selectInput("input_map_year",
                                                    "Year:",
                                                    c(2005,2007,2010,2013))
                          )
                 ),
                 tabPanel("Word Cloud",                    
                          titlePanel("Word Cloud"),  
                          sidebarPanel(
                            textInput("word",
                                      "Type a word:",
                                      value="Aid"),
                            actionButton("update", "change")),
                          mainPanel(plotOutput("plot"))
                          ),
                 tabPanel("Graphs",
                          absolutePanel(top = 75, right = 25,
                                        selectInput("recipients_donors_graph",
                                                    "Recipients/Donors:",
                                                    c("All" = 0,
                                                      'Recipients' = 1,
                                                      'Donors' = 2)),
                                        selectInput("input_graph_year",
                                                    "Year:",
                                                    c("All",c(2005,2007,2010,2013))),
                                        selectInput("input_graph_indicator", "Indicator:", 
                                                    choices = c('Indicator 5a, As Scheduled'= 1,
                                                                'Indicator 5a,  Beyond Scheduled'= 2,
                                                                'Indicator 6, Of Sheduled'= 3,
                                                                'Indicator 6, Beyond Scheduled'= 4,
                                                                'Indicator 9b' = 5))
                          ),
                          mainPanel(plotOutput("graph"))
                 )
)