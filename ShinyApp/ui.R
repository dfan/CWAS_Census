library(shiny)
# Define UI for miles per gallon application
shinyUI(fluidPage( #pageWithSidebar
  
  # Application title
  #headerPanel("US Census 2000/2010"),
  
  tags$head(
    tags$style("#map3{height:95vh !important;}"),
    tags$style(HTML("
                    div.container {
        width: 100%;
                    }
                    
                    ")),
    tags$script(
      HTML("
        $(document).ready(function(){
          // Mark columns we want to toggle
          $('body').find('div [class=col-sm-3]').addClass('sidebarPanel');
          $('body').find('div [class=col-sm-6]').addClass('sidebarPanel');
          // fluidRow belongs to the well class
          $('body').find('div [class=well]').addClass('sidebarPanel');
          $('body').find('div [class=col-sm-12]').addClass('mainPanel');
        })
       
        Shiny.addCustomMessageHandler ('resize',function (message) {
        $('.sidebarPanel').toggle();
        // $('.mainPanel').toggleClass('col-sm-12');
        $(window).trigger('resize')
        });
     ")
    )
  ),
  
  actionButton("showpanel", "Show/hide sidebar"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  
  sidebarLayout(
    sidebarPanel(width = 12,
      fluidRow(
        column(width = 3,
          selectInput("variable1", "Select Year(s):",
                      list("None" = "None",
                           "2000" = "2000", 
                           "2010" = "2010",
                           "Both 2000 and 2010" = "Both 2000 and 2010",
                           "Difference Between" = "Difference Between")), 
          actionButton("action", "Go!")
        ),
        
        column(width = 3,
          selectInput("variable2", "Color Map By:",
                     list("None" = "None",
                          "Median Income" = "Median Income", 
                          "Black Population %" = "Black Population %",
                          "White Population %" = "White Population %",
                          "County Population" = "County Population",
                          "% Below Poverty" = "% Below Poverty",
                          "Blue Vote %" = "Blue Vote %",
                          "Red Vote %" = "Red Vote %"))
        ),
      
        column(width = 6,
            column(width = 5,
              fileInput('file1', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')), 
              fileInput('file2', NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
            ),
            column(width = 7,
            fluidRow(
              column(width = 12, offset = 1, checkboxInput('header', 'Include Header', TRUE))
            ),
            column(width = 5,
            radioButtons('sep', 'Separator',
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'), 
                         ',')
            ),
            column(width = 7,
                   radioButtons('quote', 'Quote',
                                c(None='',
                                  'Double Quote'='"',
                                  'Single Quote'="'"),
                                '"')
            ))
          )
      )),
    mainPanel(width = 12,
              tabsetPanel(
                tabPanel("Map(s)", uiOutput("maps", width = "100%")
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
                         radioButtons('icd9', NULL, choices = c('None selected', 'See ICD9 rates for table 1', 'See ICD9 rates for table 2')),
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

