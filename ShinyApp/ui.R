library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinysky)
# Define UI for miles per gallon application
shinyUI(navbarPage("CWAS", id = "Page", theme = shinytheme("united"),
  
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
  
  # header option so that no ghost tab. Not necessary if not navbarPage layout
  header = tags$head(
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
    tags$style(".navbar {margin-right: 0;}
                .navbar .navbar-nav {float: right;}"),
    #            .navbar-wrapper, .navbar, .navbar-inner, .container {margin-left: 0 !important; margin-right: 0 !important;}"),
    tags$style("#thumbnail button
               {
                  width: 800px;
                  height: 500px;
               }"),
    
    #tags$style("$maps{height:calc(100 + vh) !important;}"),
    #tags$style("$legend{height:100vh !important;}"),
    #gets rid of weird small shift when you click go and the scrollbar disappears
    tags$style(type="text/css", "body { overflow-y: scroll; }")
  ),

    tabPanel(tagList(shiny::icon("home"), "Home"),
      div(class = "home",
         includeHTML("html/Template/home.html")
      )
    ),
    tabPanel("Maps", icon = shiny::icon("picture-o"),
      column(width = 2, align = 'left', style='padding:0px;', actionButton("showpanel1", "Show/hide top panel", style = "success")),
      column(width = 2, align = 'left', style='padding:0px;', downloadButton('png', 'Download as png')),
      column(width = 2, align = 'left', style='padding:0px;', downloadButton('pdf', 'Download as pdf')),
      sidebarLayout(
        conditionalPanel("input.showpanel1 % 2 == 0",
          sidebarPanel(width = 12,
            fluidRow(
              column(width = 3, style='padding:0px;',
                column(width = 12, selectInput("whichMapData", "Select dataset:", 
                                   list("None" = "None", "Plot by census data" = "Plot by census data", "Plot by user data" = "Plot by user data"))
                )
              ),
              column(width = 9, style='padding:0px;',
                column(width = 3,
                  column(width = 12,
                    fluidRow(selectInput("total", "Total Plots", list("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9"), width = "100%")),
                    fluidRow(selectInput("cols", "Columns", list("1" = "1", "2" = "2", "3" = "3"), width = "100%")),
                    fluidRow(selectInput("detailLevel", "Level of Detail:", list("None" = "None", "Zip Code" = "Zip Code", "County" = "County", "State" = "State", "Region" = "Region"))),
                    fluidRow(actionButton("action", "Go!", width = "100%", style = "primary"))
                  )
                ),
                column(width = 3,
                  uiOutput("mapselection1"),
                  checkboxInput('difference', 'Plot by Difference', FALSE),
                  checkboxInput('percentdifference', 'Plot by % Difference', FALSE),
                  checkboxInput('percent', 'Plot data as percent', FALSE)
                ),
                column(width = 3,
                  conditionalPanel("input.difference || input.percentdifference", 
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
          uiOutput("maps")
        )
      )
    ),
    tabPanel("Table", icon = shiny::icon("database"),
      actionButton("showpanel2", "Show/hide top panel", style = "success"),
      sidebarLayout(
        conditionalPanel("input.showpanel2 % 2 == 0",
          sidebarPanel(width = 12,
            fluidRow(
              br(),
              column(3, selectInput("useData", "Select dataset:", 
                                   list("None" = "None",
                                        "Sort table by census data" = "Sort table by census data",
                                        "Sort table by user data" = "Sort table by user data")), 
                                   actionButton("displayTable", "Go!", width = "100%", style = "primary")
              ),
              column(3, uiOutput("table1List"), shinyalert("incompatible", click.hide = FALSE, auto.close.after = 5)),
              column(3, uiOutput("table2List"), shinyalert("noselection", click.hide = FALSE, auto.close.after = 5)),
              column(3, selectInput("stat", "Order by statistic:",
                                   list("None" = "None",
                                        "Binomial Exact" = "Binomial Exact", 
                                        "Chi-squared" = "Chi-squared",
                                        "Two Proportions" = "Two Proportions",
                                        "Percent Difference" = "Percent Difference")),
                    shinyalert("delimitter", click.hide = FALSE, auto.close.after = 7)
              )
            )
          )
        ),
        mainPanel(width = 12,
          fluidRow(
            br(),
            column(12, align = "center",
                   br(),
                   dataTableOutput("table"),
                   br()
            )
          )
        )
      )
    ),
    tabPanel("ICD9s", icon = shiny::icon("database"),
      actionButton("showpanel3", "Show/hide top panel", style = "success"),
      sidebarLayout(
        conditionalPanel("input.showpanel3 % 2 == 0",
          sidebarPanel(width = 12,
            fluidRow(
              column(width = 4,    
                radioButtons('icd9', NULL, choices = c('None selected', 'See ICD9 rates for user table')),
                uiOutput("icd9List"),
                actionButton("displayICD9", "Go!", style = "primary")
              )
            )
          )
        ),
        mainPanel(width = 12,
          fluidRow(
            br(),
            column(12, align = "center",
                   dataTableOutput('icd9table')
            )
          )
        )
      )
    ),
    tabPanel("Upload File", icon = shiny::icon("upload"),
      sidebarLayout(
        sidebarPanel(width = 12,
          fluidRow(column(width = 4, fileInput('file1', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
            column(width = 8, 
              conditionalPanel("output.isUploaded",  
                column(width = 12, selectInput('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),','), 
                       checkboxInput('header', 'Include Header', TRUE)
                )
              )
            )
          )
        ),
        mainPanel(width = 12,
          tags$p("Switch back to this tab anytime to upload a different file. The old one will be overwritten.")
        )
      )
    ),
    navbarMenu("More", icon = shiny::icon("cog"),
      tabPanel("Help", icon = shiny::icon("question"),
        tags$h2("Format for map data"),
        tags$li(
          tags$ul("First column must be five-digit zip codes, five-digit county codes (with leading zeros)or state names in lowercase.")
        ),
        tags$h2("Format for statistic table"),
        tags$li(
          tags$ul("First column must be five-digit zip codes, five-digit county codes (with leading zeros or state names in lowercase"),
          tags$ul("Columns must be values between 0 and 1 for the binomia-exact, chi-squared and two-proportion tests.")
        )
      ), 
      tabPanel("Documentation", icon = shiny::icon("info"),
        tags$h2("Data sources"),
        tags$a(href = "http://www.census.gov/support/USACdataDownloads.html", "US Census Counties Database")
      )
    )
  
, fluid = TRUE)
)
  

