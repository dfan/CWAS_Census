library(shiny)
library(RMySQL)
library(choroplethr)
library(ggplot2)
library(grid)
source("helper.R")

con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2000", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data2000 <- dbReadTable(conn = con, name = "acs")
dbDisconnect(con)
con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2010", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data2010 <- dbReadTable(conn = con, name = "acs")
dataCombined <- cbind(data2000, data2010[, -1], data2010[, -1])
colnames(dataCombined) <- c('county', read.csv('../Data/censusColNames.csv', stringsAsFactors=FALSE)[, 1])
dbDisconnect(con)

options(shiny.maxRequestSize=150*1024^2)

# Define server logic required to output displays
shinyServer(function(input, output, session) {
  getTotal <- reactive({
    return (as.numeric(input$total))
  })

  getNumCols <- reactive({
    return (as.numeric(input$cols))
  })
  
  getNumRows <- reactive({
    return (as.numeric(input$rows))
  })
  
  getParam1a <- reactive({
    return (input$variable1a)
  })
  
  getParam1b <- reactive({
    return (input$variable1b)
  })
  
  getParam2a <- reactive({
    return (input$variable2a)
  })
  
  getParam2b <- reactive({
    return (input$variable2b)
  })
  
  getParam3a <- reactive({
    return (input$variable3a)
  })
  
  getParam3b <- reactive({
    return (input$variable3b)
  })
  
  getParam4a <- reactive({
    return (input$variable4a)
  })
  
  getParam4b <- reactive({
    return (input$variable4b)
  })
  
  getParam5a <- reactive({
    return (input$variable5a)
  })
  
  getParam5b <- reactive({
    return (input$variable5b)
  })
  
  getParam6a <- reactive({
    return (input$variable6a)
  })
  
  getParam6b <- reactive({
    return (input$variable6b)
  })
  
  getParam7a <- reactive({
    return (input$variable7a)
  })
  
  getParam7b <- reactive({
    return (input$variable7b)
  })
  
  getParam8a <- reactive({
    return (input$variable8a)
  })
  
  getParam8b <- reactive({
    return (input$variable8b)
  })
  
  getParam9a <- reactive({
    return (input$variable9a)
  })
  
  getParam9b <- reactive({
    return (input$variable9b)
  })
  
  
  # height parameter in plotOuput doesn't work when you do arithmetic.. even tho the number is rendered
  getHeight <- reactive(
    return (floor((session$clientData$output_map1_height * 0.1)))
    # return (session$clientData[[paste0('output_', 'map1', '_height')]])
  )
  
  # isolate -> dependency on go button
  plotObjects <- eventReactive(input$action, {
    values <- reactiveValues(i = 0)
    if (input$whichMapData == 'Plot by census data')
      data <- dataCombined
    if (input$whichMapData == 'Plot by user data')
      data <- readTable()
    data2 <- NULL 
    data3 <- NULL 
    if (getParam2a() != 'None')
      data2 <- data[, getParam2a()]
    if (getParam3a() != 'None')
      data3 <- data[, getParam3a()]
    legend <- ''
    if (is.null(data2) && is.null(data3))
      legend <- 'legendandmap'
    
    plotList <<- lapply(1:getTotal(), function(x) {
        if (input$difference)
          plotDiffMap(get(paste0('getParam', values$i, 'a'))(), get(paste0('getParam', values$i , 'b'))(), data, paste("USA Colored by Difference in", get(paste0('getParam', values$i, 'a'))(), 'and', get(paste0('getParam', values$i, 'a'))()))$render()
        if (!input$difference) {
          values$i <- values$i + 1
          plotMap(get(paste0('getParam', values$i, 'a'))(), data, paste("USA Colored by", get(paste0('getParam', values$i , 'a'))()), getBuckets(data[, get(paste0('getParam', values$i, 'a'))()], data2, data3), legend)$render()
        }
      })
      return(plotList)
  })
  
  plotLegend <- eventReactive(input$action, {
    if (input$whichMapData == 'Plot by census data')
      data <- dataCombined
    if (input$whichMapData == 'Plot by user data')
      data <- readTable() 
    param1a <- getParam1a()
    param1b <- getParam1b()
    param2a <- getParam2a()
    param2b <- getParam2b()
    param3a <- getParam3a()
    param3b <- getParam3b()
    data2 <- NULL 
    data3 <- NULL 
    if (param2a != 'None')
      data2 <- data[, param2a]
    if (param3a != 'None')
      data3 <- data[, param3a]
    if (input$difference)
      plotDiffMap(param1a, param1b, data, paste("USA Colored by Difference in", param1a, 'and', param1b))
    if (!input$difference)
      plotMap(param1a, data, paste("USA Colored by", param1a), getBuckets(data[, param1a], data2, data3), 'legendonly')
  })

  output$allmaps <- renderPlot({
    do.call("grid.arrange", c(plotObjects(), ncol=getNumCols(), nrow = ceiling(getTotal() / getNumCols())))
  })
  
  output$legend <- renderPlot({
    # call grid.draw here instead of helper so legend doesn't disappear when page resizes
    # legend is fixed size unfortunately in ggplot
    if (getTotal() > 1)
      grid.draw(plotLegend())
  })
  
  # Reactive scope reference: https://shinydata.wordpress.com/2015/02/02/a-few-things-i-learned-about-shiny-and-reactive-programming/
  output$maps <- renderUI({
    # isolate mapType value update so that reactive dependencies don't override the isolated go button
    input$action
    isolate(total <- getTotal())

    column(12, align = "center", 
        fluidRow(
          plotOutput("allmaps", width = '100%')
        ),
      fluidRow(
        column(12, align = "center", plotOutput("legend", width = '100%', height = 75))
      )
    )
  })
   
  ### for second panel ###
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
    } else if (input$useData == 'Sort table by user data' & !is.null(input$file1)) {
      table <- readTable()
      # avoid error message when "None" is selected but files are uploaded
      if (input$sortBy != 'None') {
        data <- as.data.frame(cbind(table[, 1], table[, 2], table[, 3]))
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
  
  readMap <- reactive({
    inFile <- ''
    if (input$whichMapData == 'Plot by census data') {
      a <- read.csv('../Data/censusColNames.csv', stringsAsFactors=FALSE)
    } else if (is.null(input$file1)) {
      return(NULL)
    } else {
      inFile <- input$file1
      a <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    }
    a
  })
  
  readTable <- reactive({
    inFile <- ''
    if (input$useData == 'Sort table by census data') {
      a <- read.csv('../Data/censusColNames.csv', stringsAsFactors=FALSE)
    } else if (is.null(input$file1)) {
      return(NULL)
    } else {
      inFile <- input$file1
      a <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    }
    a
  })
  
  output$map1Selection1 <- renderUI({
    selectInput("variable1a", "Map 1:", getMapCols())
  })
  
  output$map1Selection2 <- renderUI({
    selectInput("variable1b", "Map 1:", getMapCols())
  })
  
  output$map2Selection1 <- renderUI({
    selectInput("variable2a", "Map 2:", getMapCols())
  })
  
  output$map2Selection2 <- renderUI({
    selectInput("variable2b", "Map 2:", getMapCols())
  })
  
  output$map3Selection1 <- renderUI({
    selectInput("variable3a", "Map 3:", getMapCols())
  })
  
  output$map3Selection2 <- renderUI({
    selectInput("variable3b", "Map 3:", getMapCols())
  })
  
  output$map4Selection1 <- renderUI({
    selectInput("variable4a", "Map 4:", getMapCols())
  })
  
  output$map4Selection2 <- renderUI({
    selectInput("variable4b", "Map 4:", getMapCols())
  })
  
  output$map5Selection1 <- renderUI({
    selectInput("variable5a", "Map 5:", getMapCols())
  })
  
  output$map5Selection2 <- renderUI({
    selectInput("variable5b", "Map 5:", getMapCols())
  })
  
  output$map6Selection1 <- renderUI({
    selectInput("variable6a", "Map 6:", getMapCols())
  })
  
  output$map6Selection2 <- renderUI({
    selectInput("variable6b", "Map 6:", getMapCols())
  })
  
  output$map7Selection1 <- renderUI({
    selectInput("variable7a", "Map 7:", getMapCols())
  })
  
  output$map7Selection2 <- renderUI({
    selectInput("variable7b", "Map 7:", getMapCols())
  })
  
  output$map8Selection1 <- renderUI({
    selectInput("variable8a", "Map 8:", getMapCols())
  })
  
  output$map8Selection2 <- renderUI({
    selectInput("variable8b", "Map 8:", getMapCols())
  })
  
  output$map9Selection1 <- renderUI({
    selectInput("variable9a", "Map 9:", getMapCols())
  })
  
  output$map9Selection2 <- renderUI({
    selectInput("variable9b", "Map 9:", getMapCols())
  })
  
  output$tableList <- renderUI({
    selectInput("sortBy", "Sort By:", getTableCols())
  })
  
  getMapCols <- reactive({
    switch(input$whichMapData,
           'Plot by census data' = c('None', readMap()[, 1]),
           'Plot by user data' = c('None', colnames(readMap())[-1])
    )
  })
  
  getTableCols <- reactive({
    switch(input$useData,
           'Sort table by census data' = c('None', readTable()[, 1]),
           'Sort table by user data' = c('None', colnames(readTable())[-1])
    )
  })
  
  # populate dropdown menu with column names of dataframe (table 1 and table 2 assumed to have same column names)
  output$icd9List <- renderUI({
    selectInput('sorticd9', NULL, c("None", colnames(readTable())))
  })
  
  # Isolate output to give dependency on go button
  geticd9Table <- eventReactive(input$displayTable, {
    inFile <- ''
    data <- ''
    if (input$icd9 == 'None selected' | is.null(input$file1) & is.null(input$file2)) {
      return(NULL)
    } else if (input$icd9 == 'See ICD9 rates for user table') {
      inFile <- input$file1
      data = data2000
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