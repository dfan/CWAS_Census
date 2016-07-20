library(shiny)
library(RMySQL)
library(choroplethr)
library(ggplot2)
library(gtable)
library(gridExtra)
library(grid)
data(state)
# use install.packages("devtools") and install_github('arilamstein/choroplethrZip@v1.3.0', force = TRUE) for zip maps
library(choroplethrZip)

aggregateUsertoCounty <- function(data, zipTable) {
  countiesList <- sapply(data[, 1], function(x) {
    # five-digit FIPS code
    paste0(zipTable[which(zipTable$ZCTA5 == x), 'STATE'], zipTable[which(zipTable$ZCTA5 == x), 'COUNTY'])
  })
  popList <- sapply(data[, 1], function(x) {
    # five-digit FIPS code
    zipTable[which(zipTable$ZCTA5 == x), 'POPPT']
  })
  # can't do data[, x] or list doesn't add correctly
  data$county <- countiesList
  data$pop <- popList
  # reorder
  data <- data[c((length(unique(unlist(names(data)))) - 1):length(unique(unlist(names(data)))), 1:(length(unique(unlist(names(data)))) - 2))]
  uniqueCounties <- unique(unlist(countiesList))
  df <- as.data.frame(cbind(uniqueCounties, matrix(rep(0, (length(names(data)) - 3) * length(uniqueCounties)), ncol = length(names(data)) - 3)))
  names(df) <- names(data)[-(1:2)]
  df[, -1] <- sapply(uniqueCounties, function(x) {
    sapply(2:dim(df)[2], function(j) {
      county <- do.call('c', lapply(1:length(data[,  1]), function(y) {
        if (x %in% data[, 1][[y]]) {
          y
        }
      }))
      popList <- do.call('c', lapply(1:length(data[,  1]), function(y) {
        if (x %in% data[, 1][[y]]) {
          data$pop[[y]][which(data[, 1][[y]] == x)]
        }
      }))
      getWeighted(data[county, (j + 2)], popList)
    })
  })
  df[, 1] <- as.character(df[, 1])
  df[, -1] <- as.numeric(as.character(df[, -1]))
  return(df)
}

getMedianList <- function(data, pop) {
  do.call('c', lapply(1:length(data), function(i) {
    rep(data[i], pop[i])
  }))
}

getWeighted <- function(data, pop) {
  list <- sapply(1:length(data), function(i) {
    # prevent integer overflow
    as.numeric(data[i]) * pop[i] / sum(pop)
  })
  return(sum(list))
}

# external file because function is non-reactive
plotMap <- function(string, type, data, title, color, buckets, detail, legend) {
  df <- as.data.frame(cbind(data[, 1], data[, string]))
  names(df) <- c("region", "value")
  # remove leading zeros from FIP codes
  # choropleth requires numeric column type in dataframe
  df$value <- as.numeric(as.character(df$value))
  legendLabels <- getLabels(buckets)
  df$value <- factor(sapply(df$value, function(y) {
    if (y <= buckets[1])
      y <- legendLabels[1]
    else if (y <= buckets[2])
      y <- legendLabels[2]
    else if (y <= buckets[3])
      y <- legendLabels[3]
    else if (y <= buckets[4])
      y <- legendLabels[4]
    else if (y <= buckets[5])
      y <- legendLabels[5]
    else if (y <= buckets[6])
      y <- legendLabels[6]
    else
      y <- legendLabels[7]
  }))
  # additional customizations require creating a CountyChoropleth object, not using the county_choropleth() method
  if (type == 'census') {
    if (detail == 'County') {
      map <- CountyChoropleth$new(df)
    }
    if (detail == 'State') {
      df[, 1] <- as.character(df[, 1])
      map <- StateChoropleth$new(df)
    }
  }
  if (type == 'user') {
    if (detail == 'Zip') {
      map <- ZipChoropleth$new(df)
    } 
    if (detail == 'County') {
      map <- CountyChoropleth$new(df)
    }
  }
  
  map$title <- title
  map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette=color, drop=FALSE, guide = FALSE)
  if (legend == 'legendandmap' || legend == 'legendonly') {
    map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette=color, drop=FALSE)
  }
  renderMap(map, legend)
}

renderMap <- function(map, legend) {
  if (legend == "legendonly") {
    # make legend spread out
    tmp <- ggplot_gtable(ggplot_build(map$render() + guides(fill=guide_legend(nrow = 2))))
    # usually value of 8
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    # so legend doesn't disappear when page resizes
    return(legend)
  } else {
    return(map)
  }
}

getBuckets <- function(dataList) {
  dataTypes <- sapply(dataList, function(x) class(x))
  if (length(which(dataTypes == 'integer')) == length(dataTypes)) {
    return(floor(quantile(dataList, c(1/7.0, 2/7.0, 3/7.0, 4/7.0, 5/7.0, 6/7.0, 1.0))))
  }
  return(round(quantile(dataList, c(1/7.0, 2/7.0, 3/7.0, 4/7.0, 5/7.0, 6/7.0, 1.0)), 5))
}

getLabels <- function(buckets) {
  y <- c(
    paste0('0 to ', format(buckets[1], big.mark=","), sep=""),
    paste0(format(buckets[1],  big.mark=","), ' to ', format(buckets[2], big.mark = ",")),
    paste0(format(buckets[2], big.mark=","), ' to ', format(buckets[3], big.mark = ",")),
    paste0(format(buckets[3],  big.mark=","), ' to ', format(buckets[4], big.mark = ",")),
    paste0(format(buckets[4],  big.mark=","), ' to ', format(buckets[5], big.mark = ",")),
    paste0(format(buckets[5],  big.mark=","), ' to ', format(buckets[6], big.mark = ",")),
    paste0(format(buckets[6],  big.mark=","), ' to ', format(buckets[7], big.mark = ","))
  )
  
  #y <- factor(y, labels = y, ordered = TRUE)
  return(y)
}

plotDiffMap <- function(param1, param2, type, data, title, color, buckets, detail, legend) {
  df <- as.data.frame(cbind(data[,1], abs(data[, param2] - data[, param1])))
  names(df) <- c("region", "value")
  # remove leading zeros from FIP codes
  # choropleth requires numeric column type in dataframe
  df$value <- as.numeric(as.character(df$value))
  legendLabels <- getLabels(buckets)
  df$value <- factor(sapply(df$value, function(y) {
    if (y <= buckets[1])
      y <- legendLabels[1]
    else if (y <= buckets[2])
      y <- legendLabels[2]
    else if (y <= buckets[3])
      y <- legendLabels[3]
    else if (y <= buckets[4])
      y <- legendLabels[4]
    else if (y <= buckets[5])
      y <- legendLabels[5]
    else if (y <= buckets[6])
      y <- legendLabels[6]
    else
      y <- legendLabels[7]
  }))
  
  # additional customizations require creating a CountyChoropleth object, not using the county_choropleth() method
  if (type == 'census') {
    if (detail == 'County') {
      map <- CountyChoropleth$new(df)
    }
    if (detail == 'State') {
      df[, 1] <- as.character(df[, 1])
      map <- StateChoropleth$new(df)
    }
  }
  if (type == 'user') {
    if (detail == 'Zip') {
      map <- ZipChoropleth$new(df)
    } 
    if (detail == 'County') {
      map <- CountyChoropleth$new(df)
    }
  }
  map$title = title
  map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette=color, drop=FALSE, guide = FALSE)
  if (legend == 'legendandmap' || legend == 'legendonly') {
    map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette=color, drop=FALSE)
  }
  renderMap(map, legend)
}

addStatCol <- function(string, data, pop1, pop2) {
  if (string == 'Binomial Exact') {
    data[, string] <- sapply(1:length(data[, 1]), 
                             function(i) {
                               # ceiling to force whole number
                               if (data[i,2] != 0 & data[i,3] != 0) {
                                 format(round(binom.test(ceiling(data[i,2] * pop2[i]), pop2[i], data[i,3], alternative="two.sided"), 7), scientific = TRUE)$p.value
                               } else {
                                 return(1.0)
                               }
                             }
    )
    data <- data[order(data[, string], decreasing = FALSE), ]
  }
  if (string == 'Chi-squared') {
    data[, string] <- sapply(1:length(data[, 1]),
                             function(i) {
                               if (data[i,2] != 0 & data[i,3] != 0) {
                                 obs <- c(ceiling(data[i,3] * pop2[i]), ceiling((1 - data[i,3]) * pop2[i]))
                                 # ceiling to force whole number
                                 exp <- c(ceiling(data[i,2] * pop2[i]), ceiling((1 - data[i,2]) * pop2[i]))
                                 table <- as.data.frame(rbind(obs, exp))
                                 names(table) <- c('Has', 'Not Has')
                                 format(round(chisq.test(table)$p.value, 7), scientific = TRUE)
                               } else {
                                 return(1.0)
                               }
                             })
    data <- data[order(data[, string], decreasing = FALSE), ]
  }
  if (string == 'Two Proportions') {
    data[, string] <- sapply(1:length(data[, 1]),
                             function(i) {
                               if (data[i,2] != 0 & data[i,3] != 0 & pop1[i] != 0 & pop2[i] != 0) {
                                 # assume independence. disable Yates correction
                                 format(round(z.test(data[i,2], data[i,3], (data[i,2] * pop1[i] + data[i,3] * pop2[i]) / (pop1[i] + pop2[i]), pop1[i], pop2[i]), 7), scientific = TRUE)
                               } else {
                                 return(1.0)
                               }
                             })
    data <- data[order(data[, string], decreasing = FALSE), ]
  }
  if (string == 'Percent Difference') {
    data[, 'Percent Difference'] <- apply(data, 1, function(y) { 
      if (as.numeric(y[2]) == 0 | as.numeric(y[3]) == 0 | as.numeric(y[2]) == 0.0000 | as.numeric(y[3] == 0.0000)) 0.0
      else format(round(abs(as.numeric(y[3]) - as.numeric(y[2])) / as.numeric(y[2]), 7), scientific = TRUE)
      })
    data <- data[order(data[, string], decreasing = TRUE), ]
  }
  data
}

z.test <- function(p1, p2, p, n1, n2) {
  z <- -abs((p1 - p2)) / sqrt(p * (1 - p) * (1.0 / n1 + 1.0 / n2))
  return(pnorm(z) * 2)
}

### SERVER
# Define server logic required to output displays
shinyServer(function(input, output, session) {
  withProgress(message = 'Loading dependencies', value = 0.1, {
    # for animation purposes
    sapply(seq(from=0, to=35, by=1), function(i) incProgress(0.01, detail = paste0(i, '%')))
    con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2000", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
    data2000 <- dbReadTable(conn = con, name = "acs")
    dbDisconnect(con)
    con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2010", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
    data2010 <- dbReadTable(conn = con, name = "acs")
    dataCombined <- cbind(data2000, data2010[, -1], data2010[, -1])
    setwd('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census/ShinyApp')
    colnames(dataCombined) <- c('county', read.csv('../Data/censusColNames.csv', stringsAsFactors=FALSE)[, 1])
    dbDisconnect(con)
    # for animation purposes
    sapply(seq(from=35, to=70, by=1), function(i) incProgress(0.01, detail = paste0(i, '%')))
    # takes absurdly long... prevents menu from being loaded immediately
    dataState <- aggregateCensusToState(dataCombined)
    incProgress(0.3, detail = paste0(100, '%'))
    # retain leading zeros for ZCTA5 codes
    zipTable <- read.csv('../Data/zcta_county.csv', stringsAsFactors=FALSE, colClasses=c("ZCTA5"="character", "STATE" = "character", "COUNTY" = "character"))
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
  
  getDetail <- reactive({
    return (input$detailLevel)
  })
  
  # height parameter in plotOuput doesn't work when you do arithmetic.. even tho the number is rendered
  getHeight <- reactive(
    return (floor((session$clientData$output_map1_height * 0.1)))
    # return (session$clientData[[paste0('output_', 'map1', '_height')]])
  )
  
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
      data <- readTable()
    }
    list <- sapply(1:total, function(i) {
      if (!input$difference) {
        data[, input[[paste0('variable', i, 'a')]]]
      } else if (input$difference) {
        abs(data[, input[[paste0('variable', i, 'a')]]] - data[, input[[paste0('variable', i, 'b')]]])
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
    input$action
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
        data <- aggregateUsertoCounty(raw, zipTable)
        data[, 1] <- as.numeric(sapply(data[, 1], function(y) sub('^0+([1-9])', '\\1', y)))
      }
    }
    legend <- ''
    if (total == 1)
      legend <- 'legendandmap'
    colorList <- colorList()
    plotList <<- lapply(1:total, function(i) {
      if (input$difference) {
        plotDiffMap(input[[paste0('variable', i, 'a')]], input[[paste0('variable', i, 'b')]], type, data, paste("USA Colored by Difference in", input[[paste0('variable', i, 'a')]], 'and', input[[paste0('variable', i, 'b')]]), colorList[i], getBuckets(bucketData()), getDetail(), legend)$render()
      } else if (!input$difference) {
        # order matters; value line goes first
        plotMap(input[[paste0('variable', i, 'a')]], type, data, paste("USA Colored by", input[[paste0('variable', i, 'a')]]), colorList[i], getBuckets(bucketData()), getDetail(), legend)$render()
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
        data <- aggregateUsertoCounty(raw, zipTable)
      }
    }
    if (input$difference) {
      plotDiffMap(input[['variable1a']], input[['variable1b']], type, data, paste("USA Colored by Difference in", input[['variable1a']], 'and', input[['variable1b']]), legendColor(), getBuckets(bucketData()), getDetail(), 'legendonly')
    } else if (!input$difference) {
      plotMap(input[['variable1a']], type, data, paste("USA Colored by", input[['variable1a']]), legendColor(), getBuckets(bucketData()), getDetail(), 'legendonly')
    }
  })
  
  output$allmaps <- renderPlot({
    input$action
    isolate(total <- getTotal())
    isolate(col <- getNumCols())
    do.call("grid.arrange", c(plotObjects(), ncol=col, nrow = ceiling(total / col)))
  })
  
  output$legend <- renderPlot({
    # call grid.draw here instead of helper so legend doesn't disappear when page resizes
    # legend is fixed size unfortunately in ggplot
    input$action
    isolate(total <- getTotal())
    if (total > 1)
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