
# Define server logic required to output displays
shinyServer(function(input, output, session) {
  observeEvent(input$action,{
    session$sendCustomMessage(type = 'resize', message = paste0(100 * getTotal(), 'vh'))
  })
  
  # loading stuff
  withProgress(message = 'Loading...', value = 0.1, {
    # use install.packages("devtools") and install_github('arilamstein/choroplethrZip@v1.3.0', force = TRUE) for zip maps
    libraries <- c('shiny', 'RMySQL', 'choroplethr', 'ggplot2', 'gtable', 'gridExtra', 'grid', 'choroplethrZip', 'shinysky', 'DT')
    withProgress(message = 'Packages: ', value = 0.0, {
      for (i in 1:length(libraries)) {
        Sys.sleep(0.05)
        library(libraries[i],character.only=TRUE) # loading from string
        incProgress(1 / length(libraries), detail = libraries[i])
      }
    })
    data(state)
    source('helper.R')
    # for animation purposes
    sapply(seq(from=0, to=100, by=1), function(i) incProgress(0.01, detail = paste0(i, '%')))
    
    con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2000", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
    data2000 <- dbReadTable(conn = con, name = "acs")
    dbDisconnect(con)
    con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2010", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
    data2010 <- dbReadTable(conn = con, name = "acs")
    dataCombined <- cbind(data2000, data2010[, -1], data2010[, -1])
    setwd('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census/ShinyApp')
    colnames(dataCombined) <- c('county', read.csv('../Data/censusColNames.csv', stringsAsFactors=FALSE)[, 1])
    dbDisconnect(con)
    zipTable <- read.csv('../Data/zcta_county.csv', stringsAsFactors=FALSE, colClasses=c("ZCTA5"="character", "STATE" = "character", "COUNTY" = "character"))
    # already aggregated in another file to save time
    # read once to get column classes
    dataState <- read.csv('censusState.csv', stringsAsFactors=FALSE, check.names=FALSE)
    dataState <- read.csv('censusState.csv', stringsAsFactors=FALSE, check.names=FALSE, colClasses = c('character', sapply(names(dataState), function(x) class(dataState[,x]))[-1]))
    #dataState <- aggregateCensusToState(dataCombined, updateProgress)
    
    # retain leading zeros for ZCTA5 codes
    options(shiny.maxRequestSize=150*1024^2)
    # Increment the top-level progress indicator
  })
  
  getTotal <- reactive({
    return (as.numeric(input$total))
  })
  
  getNumCols <- reactive({
    return (as.numeric(input$cols))
  })
  
  getNumRows <- reactive({
    return (as.numeric(input$rows))
  })
  
  whichData <- reactive({
    return (input$whichMapData)
  })
  
  getDetail <- reactive({
    return (input$detailLevel)
  })
  
  # height parameter in plotOuput doesn't work when you do arithmetic.. even tho the number is rendered
  getWidth <- reactive({
    return (session$clientData$output_allmaps_width)
    # return (session$clientData[[paste0('output_', 'map1', '_height')]])
  })
  
  getHeight <- reactive({
    return (session$clientData$output_allmaps_width)
  })
  
  # for map display (suppressing error messages)
  isUploaded <- reactive({
    return(!is.null(input$file1))
  })
  
  # for conditional panel
  output$isUploaded <- reactive({
    return(!is.null(input$file1))
  })
  outputOptions(output, 'isUploaded', suspendWhenHidden=FALSE)
  
  bucketData <- reactive({
    input$action
    isolate(total <- getTotal())
    if (input$whichMapData == 'Plot by census data') {
      if (input$detailLevel == 'County') {
        data <- dataCombined
      } else if (input$detailLevel == 'State') {
        data <- dataState
      }
    }
    if (input$whichMapData == 'Plot by user data') {
      raw <- readTable()
      if (input$detailLevel == 'State') {
        data <- aggregateUserToState(raw)
      } else {
        data <- raw
      }
    }
    list <- sapply(1:total, function(i) {
      if (!input$difference) {
        data[, input[[paste0('variable', i, 'a')]]]
      } else if (input$difference) {
        data[, input[[paste0('variable', i, 'b')]]] - data[, input[[paste0('variable', i, 'a')]]]
      }
    })
    return(list)
  })
  
  colorList <- reactive({
    input$action
    isolate(total <- getTotal())
    list <- sapply(1:total, function(i) {
      input[[paste0('color', i)]]
    })
    return(list)
  })
  
  legendColor <- reactive({
    return(input$legendcolor)
  })
  
  # isolate -> dependency on go button
  plotObjects <- eventReactive(input$action, {
    # isolate mapType value update so that reactive dependencies don't override the isolated go button
    isolate(total <- getTotal())
    values <- reactiveValues(i = 0)
    # change from county to state data here
    if (input$whichMapData == 'Plot by census data') {
      type <- 'census'
      if (input$detailLevel == 'County') {
        data <- dataCombined
        # get rid of leading zeros
        data[, 1] <- as.numeric(sapply(data[, 1], function(y) sub('^0+([1-9])', '\\1', y)))
      } else if (input$detailLevel == 'State') {
        data <- dataState
      }
    }
    if (input$whichMapData == 'Plot by user data') {
      type <- 'user'
      raw <- readTable()
      if (input$detailLevel == 'County') {
        #data <- aggregateUsertoCounty(raw, zipTable)
        data <- raw
        data[, 1] <- as.numeric(sapply(data[, 1], function(y) sub('^0+([1-9])', '\\1', y)))
      } else if (input$detailLevel == 'State') {
        # leading 0s are removed in aggregate function
        data <- aggregateUserToState(raw)
      }
    }
    legend <- ''
    if (total == 1)
      legend <- 'legendandmap'
    colorList <- colorList()
    progress <- shiny::Progress$new()
    progress$set(message = "Plotting...", value = 0)
    on.exit(progress$close())
    plotList <- lapply(1:total, function(i) {
      if (input$difference) {
        # needs to be inside for some reason
        progress$inc(1/total, detail = paste("map", i))
        plotDiffMap(input[[paste0('variable', i, 'a')]], input[[paste0('variable', i, 'b')]], type, data, paste("USA Colored by Difference in", input[[paste0('variable', i, 'a')]], 'and', input[[paste0('variable', i, 'b')]]), colorList[i], getBuckets(bucketData()), getDetail(), legend)$render()
      } else if (!input$difference) {
        # order matters; value line goes first
        progress$inc(1/total, detail = paste("map", i))
        plotMap(input[[paste0('variable', i, 'a')]], type, data, paste("USA Colored by", input[[paste0('variable', i, 'a')]]), colorList[i], getBuckets(bucketData()), getDetail(), legend, NULL)$render()
      }
    })
    return(plotList)
  })
  
  plotLegend <- eventReactive(input$action, {
   # values <- reactiveValues(i = 0)
    if (input$whichMapData == 'Plot by census data') {
      type <- 'census'
      if (input$detailLevel == 'County') {
        data <- dataCombined
        # get rid of leading zeros
        data[, 1] <- as.numeric(sapply(data[, 1], function(y) sub('^0+([1-9])', '\\1', y)))
      } else if (input$detailLevel == 'State') {
        data <- dataState
      }
    }
    if (input$whichMapData == 'Plot by user data') {
      type <- 'user'
      raw <- readTable()
      if (input$detailLevel == 'County') {
        #data <- aggregateUsertoCounty(raw, zipTable)
        data <- raw
        # get rid of leading zeros
        data[, 1] <- as.numeric(sapply(data[, 1], function(y) sub('^0+([1-9])', '\\1', y)))
      } else if (input$detailLevel == 'State') {
        # leading 0s are removed in aggregate function
        data <- aggregateUserToState(raw)
      }
    }
    if (input$difference) {
      return(list(plotDiffMap(input[['variable1a']], input[['variable1b']], type, data, paste("USA Colored by Difference in", input[['variable1a']], 'and', input[['variable1b']]), legendColor(), getBuckets(bucketData()), getDetail(), 'legendonly')))
    } else if (!input$difference) {
      return(list(plotMap(input[['variable1a']], type, data, paste("USA Colored by", input[['variable1a']]), legendColor(), getBuckets(bucketData()), getDetail(), 'legendonly', NULL)))
    }
  })
  
  # http://stackoverflow.com/questions/33250075/get-screen-resolution-from-javascript-in-r-shiny
  output$allmaps <- renderPlot({
    input$action
    isolate(total <- getTotal())
    isolate(col <- getNumCols())
    isolate(plotlist <- plotObjects())
    do.call("grid.arrange", c(plotObjects(), nrow = ceiling(total / col), ncol = col))
    #g1 <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
    #g2 <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
    #grid.arrange(g1,g2, ncol = col)
    
    #grid.arrange(plotObjects()[[1]], plotObjects()[[2]], ncol = 1, widths = 1)
    #ggsave(filename = 'test.pdf', plot = do.call("grid.arrange", c(plotObjects(), nrow = ceiling(total / col), ncol = col)))
    #plot_grid(plotlist = plotObjects(), ncol = col, nrow = ceiling(total / col), labels = 'auto')
    #grid.draw(arrangeGrob(plotObjects(), ncol = col))
  })
  
  output$legend <- renderPlot({
    # call grid.draw here instead of helper so legend doesn't disappear when page resizes
    # legend is fixed size unfortunately in ggplot
    input$action
    isolate(total <- getTotal())
    if (total > 1) {
      do.call('grid.draw', plotLegend())
    }
  })
  
  # Reactive scope reference: https://shinydata.wordpress.com/2015/02/02/a-few-things-i-learned-about-shiny-and-reactive-programming/
  output$maps <- renderUI({
    # isolate mapType value update so that reactive dependencies don't override the isolated go button
    input$action
    isolate(whichMap <- whichData())
    isolate(whichDetail <- getDetail())
    isolate(uploaded <- isUploaded())
    if (whichMap != "None" && whichDetail != "None" && !(whichMap == "Plot by user data" && !uploaded)) {
      input$action
      isolate(total <- getTotal())
      isolate(col <- getNumCols())
      isolate(objects <- plotObjects())
      column(12, align = "center", 
             fluidRow(
               tagList(
                 column(width = 2, align = 'left', style='padding:0px;', downloadButton('png', 'Download as png')),
                 column(width = 2, align = 'left', style='padding:0px;', downloadButton('pdf', 'Download as pdf')),
                 # can't set width here and in UI or it resolves to 0
                 column(width = 12, align = 'center', plotOutput("allmaps")),
                 column(width = 12, align = 'center', plotOutput("legend", width = '100%', height = 75))
               )
             )
      )
      # don't display error at start of the app or when you've only updated one section
    }
    # splitLayout avoids columns
  })
  
  output$png <- downloadHandler(
    filename = 'plots.png',
    content = function(file) {
      if (getNumCols() == 1) {
        png(file = file, width = 11, height = 8.5, units = "in", res = 300)
        list <- plotObjects()
        final <- c(list)
        grid.arrange(grobs = final, ncol = getNumCols(), layout_matrix = matrix(1:getTotal(), byrow = TRUE, nrow = ceiling(getTotal() / getNumCols())))
        dev.off()
        if (getTotal() > 1) {
          png(file = file, width = 8.5, height = 11, units = "in", res = 300)
          leg <- plotLegend()
          final <- c(list, leg)
          grid.arrange(grobs = final, ncol = getNumCols(), layout_matrix = rbind(matrix(1:getTotal(), byrow = TRUE, nrow = ceiling(getTotal() / getNumCols())), rep(getTotal() + 1, getNumCols())))
          dev.off()
        }
      } else {
        png(file = file, width = 11, height = 8.5, units = "in", res = 300)
        list <- plotObjects()
        leg <- plotLegend()
        final <- c(list, leg)
        # arrangeGrob won't work
        #do.call('grid.arrange', c(final, layout_matrix = rbind(matrix(1:getTotal(), byrow = TRUE, nrow = ceiling(getTotal() / getNumCols())), rep(getTotal() + 1, getNumCols()))))
        grid.arrange(grobs = final, ncol = getNumCols(), layout_matrix = rbind(matrix(1:getTotal(), byrow = TRUE, nrow = ceiling(getTotal() / getNumCols())), rep(getTotal() + 1, getNumCols())))
        #grid.arrange(grid1, plotLegend(), nrow = 2)
        dev.off()
      }
    }
  )
  
  output$pdf <- downloadHandler(
    filename = 'plots.pdf',
    content = function(file) {
      if (getNumCols() == 1) {
        pdf(file = file, width = 11, height = 8.5)
        list <- plotObjects()
        final <- c(list)
        grid.arrange(grobs = final, ncol = getNumCols(), layout_matrix = matrix(1:getTotal(), byrow = TRUE, nrow = ceiling(getTotal() / getNumCols())))
        dev.off()
        if (getTotal() > 1) {
          pdf(file = file, width = 8.5, height = 11)
          leg <- plotLegend()
          final <- c(list, leg)
          grid.arrange(grobs = final, ncol = getNumCols(), layout_matrix = rbind(matrix(1:getTotal(), byrow = TRUE, nrow = ceiling(getTotal() / getNumCols())), rep(getTotal() + 1, getNumCols())))
          dev.off()
        }
      } else {
        pdf(file = file, width = 11, height = 8.5)
        list <- plotObjects()
        leg <- plotLegend()
        final <- c(list, leg)
        # arrangeGrob won't work
        #do.call('grid.arrange', c(final, layout_matrix = rbind(matrix(1:getTotal(), byrow = TRUE, nrow = ceiling(getTotal() / getNumCols())), rep(getTotal() + 1, getNumCols()))))
        grid.arrange(grobs = final, ncol = getNumCols(), layout_matrix = rbind(matrix(1:getTotal(), byrow = TRUE, nrow = ceiling(getTotal() / getNumCols())), rep(getTotal() + 1, getNumCols())))
        #grid.arrange(grid1, plotLegend(), nrow = 2)
        dev.off()
      }
    }
  )
  
  
  tableStat <- eventReactive(input$displayTable, {
    if (input$useData == 'Sort table by census data') {
      data <- dataCombined
    } 
    if (input$useData == 'Sort table by user data') {
      data <- readTable()
    } 
    if (input$stat == 'None') {
      showshinyalert(session, "noselection", "Please make a selection")
    } else  if (input$stat == 'Chi-squared' && (length(which(data[, input$sort1By] < 0)) > 0 || length(which(data[, input$sort2By] < 0)) > 0 || length(which(data[, input$sort1By] %% 1 != 0)) > 0 || length(which(data[, input$sort2By] %% 1 != 0)) > 0)) {
      showshinyalert(session, "incompatible", "Selected columns must be non-negative integers for this statistic.")
    } else if ((length(which(data[, input$sort1By] < 0)) > 0 || length(which(data[, input$sort2By] < 0)) > 0 || length(which(data[, input$sort2By] > 1)) > 0 || length(which(data[, input$sort1By] > 1)) > 0) && input$stat != 'Chi-squared' && input$stat != 'None'  && input$stat != 'Percent Difference') {
      showshinyalert(session, "incompatible", "Selected columns must be rates between 0 and 1 for this statistic.")
    } else {
      data <- as.data.frame(cbind(data[, 1], data[, input$sort1By], data[, input$sort2By]))
      names(data) <- c('county', input$sort1By, input$sort2By)
      # ensure data is in numeric format so division happens correctly below. as.character prevents numeric from removing decimals
      data[, input$sort1By] <- as.numeric(as.character(data[, input$sort1By]))
      data[, input$sort2By] <- as.numeric(as.character(data[, input$sort2By]))
      # If data didn't exist in 2000 but did in 2010, then set % change to 0. We don't want Inf values
      progress <- shiny::Progress$new()
      progress$set(message = "Computing table statistics...", value = 0)
      on.exit(progress$close())
      n <- length(data[, 1])
      updateProgress <- function(detail = NULL) {
        progress$inc(amount = 1/n, detail = detail)
      }
    
      data <- addStatCol(input$stat, data, pop1 = dataCombined$population2000, pop2 = dataCombined$population2010, updateProgress)
      data[, input$stat] <- sapply(data[, input$stat], function(x) {
        if (as.numeric(x) < 1E-8) {
          1E-8
        } else {
          format(x, scientific = TRUE)
        }
      })
      data
    }
  })
  
  ### for second panel ###
  output$table <- renderDataTable({
    datatable(tableStat(), options = list(dom = 'Bfrtip', buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'), paging = FALSE, scrollY = "90vh",
                                          list(targets = c(0), type = "num-fmt")), extensions = 'Buttons')
    # allow top search bar but not column filters
    # (not for datatable) processing = FALSE, paging = FALSE, scrollX = TRUE, scrollY = "100vh", columnDefs = list(list(targets = c(-(1:4)), searchable = FALSE)))
  })
  
  readMap <- reactive({
    inFile <- ''
    if (input$whichMapData == 'Plot by census data') {
      a <- read.csv('../Data/censusColNames.csv', stringsAsFactors=FALSE)
    } else if (is.null(input$file1)) {
      return(NULL)
    } else {
      inFile <- input$file1
      # first time to get column classes
      # check.names=FALSE prevents column names from being modified
      a <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, check.names=FALSE)
      a <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, check.names=FALSE, colClasses = c('character', sapply(names(a), function(x) class(a[,x]))[-1]))
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
      # first time to get column classes
      # check.names=FALSE prevents column names from being modified
      a <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, check.names=FALSE)
      a <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, check.names=FALSE, colClasses = c('character', sapply(names(a), function(x) class(a[,x]))[-1]))
    }
    a
  })
  
  output$mapselection1 <- renderUI({
    # dont' isolate or else the number of options won't render
    lapply(1:getTotal(), function(i) {
      selectInput(paste0('variable', i, 'a'), paste0('Map ', i), getMapCols())
    })
  })
  
  output$mapselection2 <- renderUI({
    lapply(1:getTotal(), function(i) {
      selectInput(paste0('variable', i, 'b'), paste0('Map ', i), getMapCols())
    })
  })
  
  output$colorselection <- renderUI({
    lapply(1:(getTotal() + 1), function(i) {
      if (i == (getTotal() + 1) && getTotal() > 1) {
        selectInput('legendcolor', 'Color of Legend', list('Red' = 'Reds', 'Blue' = 'Blues', 'Green' = 'Greens', 'Purple' = 'Purples', 'Orange' = 'Oranges'))
      } else if (i < (getTotal() + 1)){
        selectInput(paste0('color', i), paste0('Color ', i), list('Red' = 'Reds', 'Blue' = 'Blues', 'Green' = 'Greens', 'Purple' = 'Purples', 'Orange' = 'Oranges'))
      }
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
  geticd9Table <- eventReactive(input$displayICD9, {
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
