library(shiny)
library(RMySQL)
library(choroplethrMaps)
library(ggplot2)

con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2010", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data <- dbReadTable(conn = con, name = "acs")
dbDisconnect(con)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  formulaText <- reactive({
    paste("USA Colored by ", input$variable)
  })
  output$caption <- renderText({
   formulaText()
  })
  output$map <- renderPlot({
    string <- c('')
    if (input$variable == 'County Population') {
      string = 'population'
    }
    if (input$variable == 'White Population %') {
      string = 'white'
    }
    if (input$variable == 'Black Population %') {
      string = 'black'
    }
    if (input$variable == 'Median Income') {
      string = 'medianincome'
    }
    df <- as.data.frame(cbind(data$county, data$noquote(string)))
    names(df) <- c("region", "value")
    # remove leading zeros from FIP codes
    # choropleth requires numeric column type in dataframe
    df$region <- as.numeric(sapply(df$region, function(y) sub('^0+([1-9])', '\\1', y)))
    df$value <- as.numeric(as.character(df$value))
    county_choropleth(df, title = "", legend = "", num_colors = 7, state_zoom = NULL, county_zoom = NULL, reference_map = FALSE) + ggtitle(formulaText())
  })
})