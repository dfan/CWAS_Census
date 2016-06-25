library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("US Census 2010"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("variable", "Color Map By:",
                list("Median Income" = "Median Income", 
                     "Black Population %" = "Black Population %",
                     "White Population %" = "White Population %",
                     "County Population" = "County Population"))
    ),
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("map")
  )
))