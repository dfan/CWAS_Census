library(shiny)
library(shinythemes)
library(shinysky)
# Define UI for miles per gallon application
shinyUI(fluidPage(theme = shinytheme("united"), #theme = "yeti.css", #pageWithSidebar
  
  # Application title
  
  #'shiny-plot-output shiny-bound-output'
  #'# document.getElementById('allmaps').style.height = '100vh';
  #'
  #$(document).ready(function(){
    #$('body').find('shiny-plot-output shiny-bound-output');
   # $('body').find('div [class = col-sm-12]').addClass('allmaps'); 
  #})
  #includeHTML("script.js"),
  #console.log(message);
  #$('#allmaps').css('height', message);
  #$('#maps').css('height', message);
  tags$head(
    tags$script(
      HTML("
        $('#col-sm-12').removeClass('#maps');
        $('#maps').removeClass('#allmaps');
        Shiny.addCustomMessageHandler('resize', function(message) {
          $('#maps').height(message);
        });
      ")
    ),
    tags$style("#allmaps{height: 100vh !important;}"),
    #tags$style("$maps{height:calc(100 + vh) !important;}"),
    #tags$style("$legend{height:100vh !important;}"),
    #gets rid of weird small shift when you click go and the scrollbar disappears
    tags$style(type="text/css", "body { overflow-y: scroll; }")
  ),
  
  actionButton("showpanel", "Show/hide top panel"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarLayout(
    # each time show/hide is clicked, counter increments by 1
    conditionalPanel("input.showpanel % 2 == 0",
      column(width = 12, headerPanel("CWAS Data Visualization App")),
      sidebarPanel(width = 12,
        # for the nice gray background
        fluidRow(
          column(width = 3,
            fluidRow(column(width = 12, selectInput("whichMapData", "Select dataset:", 
                        list("None" = "None",
                             "Plot by census data" = "Plot by census data",
                             "Plot by user data" = "Plot by user data")))),
            fluidRow(column(width = 12, fileInput('file1', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')))),
            conditionalPanel("output.isUploaded",  
              column(width = 6, style='padding:0px;',
                       radioButtons('sep', 'Separator',
                                    c(Comma=',',
                                      Semicolon=';',
                                      Tab='\t'), 
                                    ','), checkboxInput('header', 'Include Header', TRUE)
                ),
                column(width = 6, style='padding:0px;',
                       radioButtons('quote', 'Quote',
                                    c(None='',
                                      'Double Quote'='"',
                                      'Single Quote'="'"),
                                    '"')
                )
            )
          ),
        
          column(width = 9,
             column(width = 3,
                    column(width = 12,
                           fluidRow(selectInput("total", "Total Plots", list("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9"), width = "100%")),
                           fluidRow(selectInput("cols", "Columns", list("1" = "1", "2" = "2", "3" = "3"), width = "100%")),
                           fluidRow(selectInput("detailLevel", "Level of Detail:", list("None" = "None", "Zip Code" = "Zip Code", "County" = "County", "State" = "State"))),
                           fluidRow(actionButton("action", "Go!", width = "100%", style = "primary"))
                    )
             ),
            column(width = 3,
              uiOutput("mapselection1"),
              checkboxInput('difference', 'Plot by Difference', FALSE)
            ),
            column(width = 3,
              conditionalPanel("input.difference", 
                uiOutput("mapselection2")
              )
            ),
            column(width = 3,
              uiOutput("colorselection")
            )
          )
        )
      )
    ),
    mainPanel(width = 12,
              tabsetPanel(
                tabPanel("Map(s)", 
                        uiOutput("maps")
                ),
                tabPanel("Table",
                         #radioButtons('useData', NULL, choices = c('Sort table by census data', 'Sort table by user data'), selected = NULL),
                         br(),
                         column(3, selectInput("useData", "Select dataset:", 
                                               list("None" = "None",
                                                    "Sort table by census data" = "Sort table by census data",
                                                    "Sort table by user data" = "Sort table by user data")), 
                                              actionButton("displayTable", "Go!", width = "100%", style = "primary")),
                         column(3, uiOutput("table1List")),
                         column(3, uiOutput("table2List")),
                         column(3, selectInput("stat", "Order by statistic:",
                                               list("None" = "None",
                                                    "Binomial Exact" = "Binomial Exact", 
                                                    "Chi-squared" = "Chi-squared",
                                                    "Two Proportions" = "Two Proportions",
                                                    "Percent Difference" = "Percent Difference"))),
                  fluidRow(
                     br(),
                     column(12, align = "center",
                         shinyalert("incompatible", click.hide = FALSE, auto.close.after = 5),
                         shinyalert("noselection", click.hide = FALSE, auto.close.after = 5),
                         br(),
                         dataTableOutput("table"),
                         br()
                     )
                  )
                ),
                tabPanel("ICD9s", 
                         radioButtons('icd9', NULL, choices = c('None selected', 'See ICD9 rates for user table')),
                         uiOutput("icd9List"),
                         actionButton("displayICD9", "Go!", style = "primary"),
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

