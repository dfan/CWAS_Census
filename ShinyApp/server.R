library(shiny)
library(RMySQL)
library(choroplethr)
library(ggplot2)
library(gtable)
library(gridExtra)
library(grid)
source("helper.R")

con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2000", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data2000 <- dbReadTable(conn = con, name = "acs")
dbDisconnect(con)
con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2010", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data2010 <- dbReadTable(conn = con, name = "acs")
dataCombined <- cbind(data2000, data2010[, -1], data2010[, -1])
setwd('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census/ShinyApp')
colnames(dataCombined) <- c('county', read.csv('../Data/censusColNames.csv', stringsAsFactors=FALSE)[, 1])
dbDisconnect(con)
# takes absurdly long... prevents menu from being loaded immediately
dataState <- aggregateToState(dataCombined)

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
  
  getDetail <- reactive({
    return (input$detailLevel)
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
  
  bucketData <- reactive({
    if (input$whichMapData == 'Plot by census data') {
      if (input$detailLevel == 'County') {
        data <- dataCombined
      } else if (input$detailLevel == 'State') {
        data <- dataState
      }
    }
    if (input$whichMapData == 'Plot by user data') {
      data <- readTable()
    }
    list <- sapply(1:getTotal(), function(i) {
      if (!input$difference) {
        data[, get(paste0('getParam', i, 'a'))()]
      } else if (input$difference) {
        abs(data[, get(paste0('getParam', i, 'a'))()] - data[, get(paste0('getParam', i, 'b'))()])
      }
    })
    return(list)
  })
  
  # isolate -> dependency on go button
  plotObjects <- eventReactive(input$action, {
    values <- reactiveValues(i = 0)
    # change from county to state data here
    if (input$whichMapData == 'Plot by census data') {
      if (input$detailLevel == 'County') {
        data <- dataCombined
        # get rid of leading zeros
        data[, 1] <- as.numeric(sapply(data[, 1], function(y) sub('^0+([1-9])', '\\1', y)))
      } else if (input$detailLevel == 'State') {
        data <- dataState
      }
    }
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
      if (input$difference) {
        values$i <- values$i + 1
        plotDiffMap(get(paste0('getParam', values$i, 'a'))(), get(paste0('getParam', values$i , 'b'))(), data, paste("USA Colored by Difference in", get(paste0('getParam', values$i, 'a'))(), 'and', get(paste0('getParam', values$i, 'b'))()), getBuckets(bucketData()), getDetail(), legend)$render()
      } else if (!input$difference) {
        # order matters; value line goes first
        values$i <- values$i + 1
        plotMap(get(paste0('getParam', values$i, 'a'))(), data, paste("USA Colored by", get(paste0('getParam', values$i , 'a'))()), getBuckets(bucketData()), getDetail(), legend)$render()
      }
    })
    return(plotList)
  })
  
  plotLegend <- eventReactive(input$action, {
   # values <- reactiveValues(i = 0)
    if (input$whichMapData == 'Plot by census data') {
      if (input$detailLevel == 'County') {
        data <- dataCombined
        # get rid of leading zeros
        data[, 1] <- as.numeric(sapply(data[, 1], function(y) sub('^0+([1-9])', '\\1', y)))
      } else if (input$detailLevel == 'State') {
        data <- dataState
      }
    }
    if (input$whichMapData == 'Plot by user data')
      data <- readTable()
    data2 <- NULL 
    data3 <- NULL 
    if (getParam2a() != 'None')
      data2 <- data[, getParam2a()]
    if (getParam3a() != 'None')
      data3 <- data[, getParam3a()]
    if (input$difference) {
      plotDiffMap(getParam1a(), getParam1b(), data, paste("USA Colored by Difference in", getParam1a(), 'and', getParam1b()), getBuckets(bucketData()), getDetail(), 'legendonly')
    } else if (!input$difference) {
      plotMap(getParam1a(), data, paste("USA Colored by", getParam1a()), getBuckets(bucketData()), getDetail(), 'legendonly')
    }
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
             # think about aspect ratios (width adjusts pretty well but height is weird)
             column(12, align = "center", plotOutput("allmaps", width = '100%'))
           ),
           fluidRow(
             column(12, align = "center", plotOutput("legend", width = '100%', height = 75))
           )
    )
  })
  
  ### for second panel ###
  output$table <- renderDataTable({
    if (input$useData == 'Sort table by census data') {
      data <- dataCombined
    } else if (input$useData == 'Sort table by user data') {
      data <- readTable()
    } else {
      # avoid error message when "None" is selected but files are uploaded
      return(NULL)
    }
    data <- as.data.frame(cbind(data$county, data[, input$sort1By], data[, input$sort2By]))
    names(data) <- c('county', input$sort1By, input$sort2By)
    # ensure data is in numeric format so division happens correctly below. as.character prevents numeric from removing decimals
    data[, input$sort1By] <- as.numeric(as.character(data[, input$sort1By]))
    data[, input$sort2By] <- as.numeric(as.character(data[, input$sort2By]))
    # If data didn't exist in 2000 but did in 2010, then set % change to 0. We don't want Inf values
    data <- addStatCol(input$stat, data, pop1 = dataCombined$population2000, pop2 = dataCombined$population2010)
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
  
  output$mapselection1 <- renderUI({
    lapply(1:(2 + getTotal()), function(i) {
      selectInput(paste0('variable', i, 'a'), paste0('Map ', i), getMapCols())
    })
  })
  
  output$mapselection2 <- renderUI({
    lapply(1:(2 + getTotal()), function(i) {
      selectInput(paste0('variable', i, 'b'), paste0('Map ', i), getMapCols())
    })
  })
  
  output$table1List <- renderUI({
    selectInput("sort1By", "Sort By:", getTableCols())
  })
  
  output$table2List <- renderUI({
    selectInput("sort2By", "Sort By:", getTableCols())
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
    selectInput('sorticd9', NULL, c("None", colnames(readTable())[-1]))
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