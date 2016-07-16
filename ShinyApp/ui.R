library(shiny)
# Define UI for miles per gallon application
shinyUI(fluidPage( #pageWithSidebar
  
  # Application title
  #headerPanel("US Census 2000/2010"),
  
  tags$head(
    tags$style("#map3{height:95vh !important;}"),
    #gets rid of weird small shift when you click go and the scrollbar disappears
    tags$style(type="text/css", "body { overflow-y: scroll; }")
  ),
  
  actionButton("showpanel", "Show/hide sidebar"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  
  sidebarLayout(
    # each time show/hide is clicked, counter increments by 1
    conditionalPanel("input.showpanel % 2 == 0",
      sidebarPanel(width = 12,
        # for the nice gray background
        fluidRow(
          column(width = 3,
            fluidRow(column(width = 12, selectInput("whichMapData", "Select dataset:", 
                        list("None" = "None",
                             "Plot by census data" = "Plot by census data",
                             "Plot by user data" = "Plot by user data")))),
            fluidRow(column(width = 12, fileInput('file1', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')))),
            fluidRow(
              column(width = 12,
                column(width = 6, style='padding:0px;',
                       radioButtons('sep', 'Separator',
                                    c(Comma=',',
                                      Semicolon=';',
                                      Tab='\t'), 
                                    ',')
                ),
                column(width = 6, style='padding:0px;',
                       radioButtons('quote', 'Quote',
                                    c(None='',
                                      'Double Quote'='"',
                                      'Single Quote'="'"),
                                    '"')
                ),
                checkboxInput('header', 'Include Header', TRUE)
              )
            )
          ),
        
          column(width = 5,
            column(width = 6,
              uiOutput("map1Selection1"),
              uiOutput("map2Selection1"),
              uiOutput("map3Selection1"),
              uiOutput("map4Selection1"),
              uiOutput("map5Selection1"),
              uiOutput("map6Selection1"),
              uiOutput("map7Selection1"),
              uiOutput("map8Selection1"),
              uiOutput("map9Selection1"),
              checkboxInput('difference', 'Plot by Difference', FALSE)
            ),
            column(width = 6,
              conditionalPanel("input.difference", 
                uiOutput("map1Selection2"),
                uiOutput("map2Selection2"),
                uiOutput("map3Selection2"),
                uiOutput("map4Selection2"),
                uiOutput("map5Selection2"),
                uiOutput("map6Selection2"),
                uiOutput("map7Selection2"),
                uiOutput("map8Selection2"),
                uiOutput("map9Selection2")
              )
            )
          ),
          
          column(width = 4,
                 column(width = 12,
                   fluidRow(selectInput("total", "Total Plots", list("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9"), width = "100%")),
                   fluidRow(selectInput("cols", "Columns", list("1" = "1", "2" = "2", "3" = "3"), width = "100%")),
                   fluidRow(actionButton("action", "Go!", width = "100%", icon("paper-plane"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                 )
          )
        )
      )
    ),
    mainPanel(width = 12,
              tabsetPanel(
                tabPanel("Map(s)", uiOutput("maps")
                ),
                tabPanel("Table",
                         #radioButtons('useData', NULL, choices = c('Sort table by census data', 'Sort table by user data'), selected = NULL),
                         br(),
                         column(3, selectInput("useData", "Select dataset:", 
                                               list("None" = "None",
                                                    "Sort table by census data" = "Sort table by census data",
                                                    "Sort table by user data" = "Sort table by user data"))),
                         column(3, uiOutput("tableList")),
                         column(3, selectInput("stat", "Order by statistic:",
                                               list("None" = "None",
                                                    "Binomial Exact" = "Binomial Exact", 
                                                    "Chi-squared" = "Chi-squared",
                                                    "Two Proportions" = "Two Proportions",
                                                    "Percent Difference" = "Percent Difference"))),
                  fluidRow(
                     br(),
                     column(12, align = "center",
                         dataTableOutput("table")
                     )
                  )
                ),
                tabPanel("ICD9s", 
                         radioButtons('icd9', NULL, choices = c('None selected', 'See ICD9 rates for user table')),
                         uiOutput("icd9List"),
                         actionButton("displayTable", "Go!"),
                  fluidRow(
                    br(),
                    column(12, align = "center",
                          dataTableOutput('icd9table')
                    )
                  )
                )
            )
    )
  )
))

