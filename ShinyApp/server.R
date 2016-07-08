library(shiny)
library(RMySQL)
library(choroplethr)
library(ggplot2)
source("helper.R")

con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2000", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data2000 <- dbReadTable(conn = con, name = "acs")
dbDisconnect(con)
con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2010", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data2010 <- dbReadTable(conn = con, name = "acs")
dbDisconnect(con)

options(shiny.maxRequestSize=150*1024^2)

# Define server logic required to output displays
shinyServer(function(input, output, session) {
  output$contents <- renderTable({
    # input$file1 is NULL initially. After user uploads a file, it will be a data frame with 'name', 'size', 'type', 
    # and 'datapath' columns. The 'datapath' column will contain the local filenames where the data can be found.
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
  
  # resize plots to fill whole page if browser window is resized
  observeEvent(input$showpanel,{
    session$sendCustomMessage(type = 'resize', message = 1)
  })
  
  formulaText <- reactive({
    paste("USA Colored by", input$variable2, "in", input$variable1)
  })
  
  output$caption <- renderText({
   formulaText()
  })
  
  # for choropleth maps
  getPlotString <- reactive({
    string <- c('')
    if (input$variable2 == 'County Population') {
      string = 'population'
    }
    if (input$variable2 == 'White Population %') {
      string = 'white'
    }
    if (input$variable2 == 'Black Population %') {
      string = 'black'
    }
    if (input$variable2 == 'Median Income') {
      string = 'medianincome'
    }
    if (input$variable2 == "% Below Poverty") {
      string = 'poverty'
    }
    if (input$variable2 == "Blue Vote %") {
      string = 'blue'
    }
    if (input$variable2 == "Red Vote %") {
      string = 'red'
    }
    string
  })
  
  # for table display (not the maps)
  getTableString <- reactive( {
    string <- c('')
    if (input$sortBy == 'County Population') {
      string = 'population'
    }
    if (input$sortBy == 'White Population %') {
      string = 'white'
    }
    if (input$sortBy == 'Black Population %') {
      string = 'black'
    }
    if (input$sortBy == 'Median Income') {
      string = 'medianincome'
    }
    if (input$sortBy == "% Below Poverty") {
      string = 'poverty'
    }
    if (input$sortBy == "Blue Vote %") {
      string = 'blue'
    }
    if (input$sortBy == "Red Vote %") {
      string = 'red'
    }
    # make table disappear when you click None
    if (input$useData == "None") {
      string = ''
    }
    string
  })
  
  # isolate -> dependency on go button
  # map 1
  plotObject1 <- eventReactive(input$action, {
    if (input$variable1 == '2000') {
      plotMap(getPlotString(), data2000, formulaText())
    } else if (input$variable1 == '2010') {
      plotMap(getPlotString(), data2010, formulaText())
    } else if (input$variable1 == 'Both 2000 and 2010') {
      plotMap(getPlotString(), data2000, paste("USA Colored by", input$variable2, 'in 2000'))
    }
  })
  
  # map 2 if both are selected
  plotObject2 <- eventReactive(input$action, {
    if (input$variable1 == 'Both 2000 and 2010') {
      plotMap(getPlotString(), data2010, paste("USA Colored by", input$variable2, 'in 2010'))
    }
  })
  
  # called separately because isolate doesn't work otherwise
  output$map1 <- renderPlot({
    plotObject1()
  })
  
  output$map2 <- renderPlot({
    plotObject2()
  })
  
  # for second panel
  output$table <- renderDataTable({
    string <- getTableString()
    data <- c('')
    if (input$useData == 'Sort table by census data' & input$stat != 'None' & getTableString() != '') {
      data <- as.data.frame(cbind(data2000$county, data2000[, string], data2010[, string]))
      names(data) <- c('County', paste(input$sortBy, '(2000)'), paste(input$sortBy, '(2010)'))
      # ensure data is in numeric format so division happens correctly below. as.character prevents numeric from removing decimals
      data[, paste(input$sortBy, '(2000)')] <- as.numeric(as.character(data[, paste(input$sortBy, '(2000)')]))
      data[, paste(input$sortBy, '(2010)')] <- as.numeric(as.character(data[, paste(input$sortBy, '(2010)')]))
      # If data didn't exist in 2000 but did in 2010, then set % change to 0. We don't want Inf values
      data <- addStatCol(input$stat, data, data2000$population, data2010$population)
    } else if (input$useData == 'Sort table by user data' & !is.null(input$file1) & !is.null(input$file2)) {
      table1 <- readTable1()
      table2 <- readTable2()
      # avoid error message when "None" is selected but files are uploaded
      if (input$sortBy != 'None') {
        data <- as.data.frame(cbind(table1[, 1], table1[, input$sortBy], table2[, input$sortBy]))
        names(data) <- c('County', 'Year1', 'Year2')
        # ensure data is in numeric format so division happens correctly below. as.character prevents numeric from removing decimals
        data[, 'Year1'] <- as.numeric(as.character(data[, 'Year1']))
        data[, 'Year2'] <- as.numeric(as.character(data[, 'Year2']))
        # If data didn't exist in 2000 but did in 2010, then set % change to 0. We don't want Inf values
        data <- addStatCol(input$stat, data, data2000$population, data2010$population)
      }
    } else {
      return(NULL)
    }
  }, options=list(processing = FALSE, paging = FALSE, scrollX = TRUE, scrollY = "100vh"))
  
  readTable1 <- reactive({
    inFile <- ''
    if (is.null(input$file1)) {
      return(NULL)
    } else {
      inFile <- input$file1
    }
    a <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    a
  })
  
  readTable2 <- reactive({
    inFile <- ''
    if (is.null(input$file2)) {
      return(NULL)
    } else {
      inFile <- input$file2
    }
    a <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    a
  })
  
  output$tableList <- renderUI({
    switch(input$useData,
           'None' = selectInput("sortBy", "Sort By:", list("")),
           'Sort table by census data' = selectInput("sortBy", "Sort By:",
                                                     list("None" = "None",
                                                          "Median Income" = "Median Income", 
                                                          "Black Population %" = "Black Population %",
                                                          "White Population %" = "White Population %",
                                                          "County Population" = "County Population",
                                                          "% Below Poverty" = "% Below Poverty",
                                                          "Blue Vote %" = "Blue Vote %",
                                                          "Red Vote %" = "Red Vote %")
           ),
           'Sort table by user data' = selectInput("sortBy", "Sort By:",
                                                   c("None", colnames(readTable1()))
           )
    )
  })
  
  # populate dropdown menu with column names of dataframe (table 1 and table 2 assumed to have same column names)
  output$icd9List <- renderUI({
    selectInput('sorticd9', NULL, c("None", colnames(readTable1())))
  })
  
  # Isolate output to give dependency on go button
  geticd9Table <- eventReactive(input$displayTable, {
    inFile <- ''
    data <- ''
    if (input$icd9 == 'None selected' | is.null(input$file1) & is.null(input$file2)) {
      return(NULL)
    } else if (input$icd9 == 'See ICD9 rates for table 1') {
      inFile <- input$file1
      data = data2000
    } else if (input$icd9 == 'See ICD9 rates for table 2') {
      inFile <- input$file2
      data = data2010
    }
    a <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    a <- as.data.frame(cbind(a[, 1], a[, input$sorticd9]))
    names(a) <- c('STCOU', input$sorticd9)
    a[, -1] <- format(round(a[, -1] / data$population[1:length(a[, 1])], 7), scientific = TRUE)
    a
  })
  
  output$icd9table <- renderDataTable({
    geticd9Table()
  }, options = list(scrollX = TRUE, scrollY = "100vh", paging = FALSE, processing = FALSE))
  
})