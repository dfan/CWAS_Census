library(RMySQL)
library(choroplethr)
# for the fips data
library(choroplethrMaps)
library(ggplot2)
library(gridExtra)
library(grid)
library(maps)
data(state)
data(state.regions)

aggregateCensusToState <- function(data, updateProgress = NULL) {
  stateCode <- unique(substr(data[, 1], 1, 2))
  # initialize another df of 51 rows and same # of cols
  stateNames <- tolower(append(state.name, 'District of Columbia', which(state.name == 'Delaware')))
  # df <- cbind(state.regions$region, data[, -1][1:length(stateCode), ])
  df <- cbind(stateNames, data[, -1][1:length(stateCode), ])
  names(df) <- c('state', names(data)[-1])
  # all counties in the same state start with the same two digits in their FIPS code
  # ranges from 01 to 56 but some numbers are skipped like 03.
  df[, -1] <- t(sapply(1:length(stateCode), function(i) {
    if (is.function(updateProgress)) {
      updateProgress(detail = NULL)
    }
    sapply(2:dim(df)[2], function(j) {
      # works vector-wise
      state <- which(substr(data[, 1], 1, 2) == stateCode[i])
      if (substr(names(df)[j], 1, 3) == 'pop') {
        sum(data[state, j]) 
      } else if (substr(names(df)[j], 1, 3) == 'med') {
        median(getMedianList(data[state, j], data[state, paste0('population', substr(names(df)[j], nchar(names(df)[j]) - 3, nchar(names(df)[j])))]))
      } else {
        getWeighted(data[state, j], data[state, paste0('population', substr(names(df)[j], nchar(names(df)[j]) - 3, nchar(names(df)[j])))])
      }
    })
  }))
  df[, 1] <- as.character(df[, 1])
  df[, 2] <- as.numeric(df[, 2])
  return(df)
}

aggregateCensusToRegion <- function(df) {
  new_england = c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont")
  middle_atlantic = c("new jersey", "new york", "pennsylvania")
  east_north_central = c("indiana", "illinois", "michigan", "ohio", "wisconsin")
  west_north_central = c("iowa", "kansas", "minnesota", "missouri", "nebraska", "north dakota", "south dakota")
  south_atlantic = c("delaware", "district of columbia", "florida", "georgia", "maryland", "north carolina", "south carolina", "virginia", "west virginia")
  east_south_central = c("alabama", "kentucky", "mississippi", "tennessee")
  west_south_central = c("arkansas", "louisiana", "oklahoma", "texas")
  mountain = c("arizona", "colorado", "idaho", "new mexico", "montana", "utah", "nevada", "wyoming")
  pacific = c("alaska", "california", "hawaii", "oregon", "washington")
  regions <- list(new_england, middle_atlantic, east_north_central, west_north_central, south_atlantic, east_south_central, west_south_central, mountain, pacific)
  df[, -1] <- sapply(2:dim(df)[2], function(i) {
    col <- df[, i]
    for (x in 1:length(regions)) {
      list <- sapply(1:length(regions[[x]]), function(j) {
        df[which(df[, 1] == regions[[x]][j]), i]
      })
      indexes <- sapply(1:length(regions[[x]]), function(j) {
        which(df[, 1] == regions[[x]][j])
      })
      for (k in 1:length(regions[[x]])) {
        if (substr(names(df)[i], 1, 3) == 'pop') {
          col[which(df[, 1] == regions[[x]][k])] <- sum(list)
        } else if (substr(names(df)[i], 1, 3) == 'med') {
          col[which(df[, 1] == regions[[x]][k])] <- median(getMedianList(list, df[indexes, paste0('population', substr(names(df)[i], nchar(names(df)[i]) - 3, nchar(names(df)[i])))]))
        } else {
          col[which(df[, 1] == regions[[x]][k])] <- getWeighted(list, df[indexes, paste0('population', substr(names(df)[i], nchar(names(df)[i]) - 3, nchar(names(df)[i])))])
        }
      }
    }
    col
  })
  df
}

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

aggregateUserToState <- function(data, updateProgress = NULL) {
  stateCode <- unique(substr(data[, 1], 1, 2))
  stateNames <- sapply(stateCode, function(x) state.regions[which(state.regions$fips.character == x), 'region'])
  df <- cbind(stateNames, data[, -1][1:length(stateNames), ])
  names(df) <- c('state', names(data)[-1])
  # all counties in the same state start with the same two digits in their FIPS code
  # ranges from 01 to 56 but some numbers are skipped like 03.
  df[, -1] <- t(sapply(1:length(stateCode), function(i) {
    if (is.function(updateProgress)) {
      updateProgress(detail = NULL)
    }
    state <- which(substr(data[, 1], 1, 2) == stateCode[i])
    if (is.function(updateProgress)) {
      updateProgress(detail = NULL)
    }
    sapply(2:dim(df)[2], function(j) {
      sum(data[state, j]) 
    })
  }))
  df[, 1] <- as.character(df[, 1])
  df[, 2] <- as.numeric(df[, 2])
  return(df)
}

# only handles absolute values right now
aggregateUserToRegion <- function(df) {
  new_england = c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont")
  middle_atlantic = c("new jersey", "new york", "pennsylvania")
  east_north_central = c("indiana", "illinois", "michigan", "ohio", "wisconsin")
  west_north_central = c("iowa", "kansas", "minnesota", "missouri", "nebraska", "north dakota", "south dakota")
  south_atlantic = c("delaware", "district of columbia", "florida", "georgia", "maryland", "north carolina", "south carolina", "virginia", "west virginia")
  east_south_central = c("alabama", "kentucky", "mississippi", "tennessee")
  west_south_central = c("arkansas", "louisiana", "oklahoma", "texas")
  mountain = c("arizona", "colorado", "idaho", "new mexico", "montana", "utah", "nevada", "wyoming")
  pacific = c("alaska", "california", "hawaii", "oregon", "washington")
  regions <- list(new_england, middle_atlantic, east_north_central, west_north_central, south_atlantic, east_south_central, west_south_central, mountain, pacific)
  df[, -1] <- sapply(2:dim(df)[2], function(i) {
    col <- df[, i]
    for (x in 1:length(regions)) {
      list <- sapply(1:length(regions[[x]]), function(j) {
        df[which(df[, 1] == regions[[x]][j]), i]
      })
      for (k in 1:length(regions[[x]])) {
        col[which(df[, 1] == regions[[x]][k])] <- sum(list)
      }
    }
    col
  })
  df
}


aggregateRegionOnly <- function(df) {
  new_england = c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont")
  middle_atlantic = c("new jersey", "new york", "pennsylvania")
  east_north_central = c("indiana", "illinois", "michigan", "ohio", "wisconsin")
  west_north_central = c("iowa", "kansas", "minnesota", "missouri", "nebraska", "north dakota", "south dakota")
  south_atlantic = c("delaware", "district of columbia", "florida", "georgia", "maryland", "north carolina", "south carolina", "virginia", "west virginia")
  east_south_central = c("alabama", "kentucky", "mississippi", "tennessee")
  west_south_central = c("arkansas", "louisiana", "oklahoma", "texas")
  mountain = c("arizona", "colorado", "idaho", "new mexico", "montana", "utah", "nevada", "wyoming")
  pacific = c("alaska", "california", "hawaii", "oregon", "washington")
  regions <- list(new_england, middle_atlantic, east_north_central, west_north_central, south_atlantic, east_south_central, west_south_central, mountain, pacific)
  names <- c('new_england', 'middle_atlantic', 'east_north_central', 'west_north_central', 'south_atlantic', 'east_south_central', 'west_south_central', 'mountain', 'pacific')
  data <- as.data.frame(cbind(names, matrix(rep(0, length(names) * (dim(df)[2] - 1)), nrow = length(names))), stringsAsFactors = FALSE)
  names(data) <- c('region', names(df)[-1])
  data[, -1] <- sapply(2:dim(df)[2], function(i) {
    col <- as.numeric(data[, i])
    for (x in 1:length(regions)) {
      list <- sapply(1:length(regions[[x]]), function(j) {
        df[which(df[, 1] == regions[[x]][j]), i]
      })
      col[x] <- sum(list)
    }
    col
  })
  data
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
plotMap <- function(string, type, data, title, color, buckets, detail, legend, percent, zoom) {
  df <- as.data.frame(cbind(data[, 1], data[, string]))
  names(df) <- c("region", "value")
  # remove leading zeros from FIP codes
  # choropleth requires numeric column type in dataframe
  df$value <- as.numeric(as.character(df$value))
  legendLabels <- getLabels(buckets, percent)
  # set levels to be sure and INSIDE not afterward or else the values will get changed
  df$value <- factor(as.character(sapply(df$value, function(y) {
    if (y <= buckets[1]) {
      legendLabels[1]
    } else if (y <= buckets[2]) {
      legendLabels[2]
    } else if (y <= buckets[3]) {
      legendLabels[3]
    } else if (y <= buckets[4]) {
      legendLabels[4]
    } else if (y <= buckets[5]) {
      legendLabels[5]
    } else if (y <= buckets[6]) {
      legendLabels[6]
    } else if (y <= buckets[7]) {
      legendLabels[7]
    } else if (y <= buckets[8]) {
      legendLabels[8]
    } else if (y <= buckets[9]) {
      legendLabels[9]
    } else {
      legendLabels[10]
    }
  })), levels = make.unique(legendLabels))

  #additional customizations require creating a CountyChoropleth object, not using the county_choropleth() method
  if (type == 'census') {
    if (detail == 'County') {
      map <- CountyChoropleth$new(df)
    }
    if (detail == 'State' || detail == 'Region') {
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
      if (!is.null(zoom)) {
        map <- CountyZoomChoropleth$new(df)
        map$set_zoom(zoom)
      }
    }
    if (detail == 'State' || detail == 'Region') {
      df[, 1] <- as.character(df[, 1])
      map <- StateChoropleth$new(df)
    }
  }
  map$title <- title
  # http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  if (color == "Reds") {
    cbPalette <- colorRampPalette(c(colorRampPalette(c("white", "orange1"))(20)[2], "tomato1", "red3", "#660000"))(10)
  } 
  if (color == "Blues") {
    cbPalette <- colorRampPalette(c(colorRampPalette(c("white", "#0000ff"))(20)[2], "royalblue2", "#000099"))(10)
  }
  if (color == "Greens") {
    cbPalette <- colorRampPalette(c(colorRampPalette(c("white", "palegreen3"))(18)[2], "#66a266", "#003200"))(10)
  }
  if (color == "Red-Green") {
    numRed <- length(which(buckets < 0))
    cbPalette <- c(rev(colorRampPalette(c("#ffe5e5", "red3"))(numRed + 1))[-1], colorRampPalette(c("#e5f2e5", "palegreen3", "#004000"))(10 - numRed))
  }
  # scale_fill_brewer(name=NULL, labels = legendLabels, palette=color, drop=FALSE, guide = FALSE)
  # drop = FALSE to prevent unused buckets from being omitted. name = NULL to prevent legend title
  map$ggplot_scale <- scale_fill_manual(name=NULL, values = cbPalette, drop = FALSE, labels = legendLabels, guide = FALSE)
  if (legend == 'legendandmap' || legend == 'legendonly') {
    map$ggplot_scale <- scale_fill_manual(name=NULL, values = cbPalette, drop = FALSE, labels = legendLabels)
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

getBuckets3 <- function(dataList, type) {
  if (type != 'Percent') {
    modifiedList <- dataList[which(dataList != 0)]
  } 
  if (type == 'Percent') {
    # Zeros were removed in server.R
    modifiedList <- dataList
  }
  tempBins <- quantile(modifiedList, c(0.75, sapply(1:8, function(x) 0.75 + x / 8 * 0.25)), na.rm = TRUE)
  if (length(which(dataList %% 1 == 0)) == length(dataList)) {
    return(floor(tempBins))
  }
  # 2 decimal places
  return(as.numeric(format(round(tempBins, 4), nsmall = 2)))
}

getBuckets <- function(dataList, type) {
  # deciles
  # get rid of 0 values (if years differed in counties listed)
  # Zeros were removed in server.R for percents; zeros don't matter for the rest
  modifiedList <- dataList[which(dataList != 0)]
  if (type == 'Difference' || type == 'Percent') {
    modifiedList <- dataList
  }
  tempBins <- quantile(modifiedList, sapply(1:10, function(x) x / 10.0), na.rm = TRUE)
  if (length(which(dataList %% 1 == 0)) == length(dataList)) {
    return(floor(tempBins))
  }
  # 2 decimal places
  return(as.numeric(format(round(tempBins, 4), nsmall = 2)))
}

# 1s + quartiles
getBucketsVersion2 <- function(dataList) {
  # always want 5 buckets
  # get rid of 0 values (if years differed in counties listed)
  modifiedList <- dataList[which(dataList != 0)]
  tempBins <- quantile(modifiedList, sapply(1:5, function(x) x / 5.0))
  # be careful with absolute difference since it can be negative so you don't want the first bin to be 1 or lower
  if (length(which(dataList == 1)) > 0 && length(which(dataList < 0)) == 0) {
    tempBins[1] <- 1
    modifiedList <- dataList[which(dataList != 1 & dataList != 0)]
    tempBins[-1] <- quantile(modifiedList, sapply(1:4, function(x) x / 4))
  }
  if (length(which(dataList %% 1 == 0)) == length(dataList) && length(unique(tempBins)) == length(tempBins)) {
    return(floor(tempBins))
  }
  # 2 decimal places
  return(as.numeric(format(round(tempBins, 4), nsmall = 2)))
}

getLabels <- function(buckets, percent) {
  y <- rep(0,10)
  # integers
  if (length(which(buckets %% 1 == 0)) == length(buckets)) {
    y[1] <- paste0(format(buckets[1], big.mark=","), ' or lower')
    for (i in 1:9) {
      if ((buckets[i] + 1) != buckets[i + 1] && buckets[i] != buckets[i + 1]) {
        y[i + 1] <- paste0(format(buckets[i] + 1,  big.mark=","), ' to ', format(buckets[i + 1], big.mark = ","))
      } else {
        y[i + 1] <- paste0(format(buckets[i] + 1,  big.mark=","))
      }
    }
  } else {
    if (percent) {
      y[1] <- paste0(format(buckets[1] * 100, big.mark=","), '% or lower')
      for (i in 2:10) {
        y[i] <- paste0(format(buckets[i - 1] * 100,  big.mark=","), '% to ', format(buckets[i] * 100, big.mark = ","), '%')
      }
    } else if (!percent) {
      y[1] <- paste0(format(buckets[1] , big.mark=","), ' or lower')
      for (i in 2:10) {
        y[i] <- paste0(format(buckets[i - 1],  big.mark=","), ' to ', format(buckets[i], big.mark = ","))
      } 
    }
  }
  #y <- factor(y, labels = y, ordered = TRUE)
  return(y)
}

plotDiffMap <- function(param1, param2, type, data, title, color, buckets, detail, legend, percent, zoom) {
  df <- as.data.frame(cbind(data[, 1], data[, param2] - data[, param1]))
  names(df) <- c("region", "value")
  # remove leading zeros from FIP codes
  # choropleth requires numeric column type in dataframe
  df$value <- as.numeric(as.character(df$value))
  legendLabels <- getLabels(buckets, percent)
  # set levels to be sure and INSIDE not afterward or else the values will get changed
  df$value <- factor(as.character(sapply(df$value, function(y) {
    if (y <= buckets[1]) {
      legendLabels[1]
    } else if (y <= buckets[2]) {
      legendLabels[2]
    } else if (y <= buckets[3]) {
      legendLabels[3]
    } else if (y <= buckets[4]) {
      legendLabels[4]
    } else if (y <= buckets[5]) {
      legendLabels[5]
    } else if (y <= buckets[6]) {
      legendLabels[6]
    } else if (y <= buckets[7]) {
      legendLabels[7]
    } else if (y <= buckets[8]) {
      legendLabels[8]
    } else if (y <= buckets[9]) {
      legendLabels[9]
    } else {
      legendLabels[10]
    }
  })), levels = make.unique(legendLabels))
  
  #additional customizations require creating a CountyChoropleth object, not using the county_choropleth() method
  if (type == 'census') {
    if (detail == 'County') {
      map <- CountyChoropleth$new(df)
    }
    if (detail == 'State' || detail == 'Region') {
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
      if (!is.null(zoom)) {
        map <- CountyZoomChoropleth$new(df)
        map$set_zoom(zoom)
      }
    }
    if (detail == 'State' || detail == 'Region') {
      df[, 1] <- as.character(df[, 1])
      map <- StateChoropleth$new(df)
    }
  }
  map$title <- title
  if (color == "Reds") {
    cbPalette <- colorRampPalette(c(colorRampPalette(c("white", "orange1"))(20)[2], "tomato1", "red3", "#660000"))(10)
  } 
  if (color == "Blues") {
    cbPalette <- colorRampPalette(c(colorRampPalette(c("white", "#0000ff"))(20)[2], "royalblue2", "#000099"))(10)
  }
  if (color == "Greens") {
    cbPalette <- colorRampPalette(c(colorRampPalette(c("white", "palegreen3"))(18)[2], "#66a266", "#003200"))(10)
  }
  if (color == "Red-Green") {
    numRed <- length(which(buckets < 0))
    cbPalette <- c(rev(colorRampPalette(c("#ffe5e5", "red3"))(numRed + 1))[-1], colorRampPalette(c("#e5f2e5", "palegreen3", "#004000"))(10 - numRed))
  }
  # scale_fill_brewer(name=NULL, labels = legendLabels, palette=color, drop=FALSE, guide = FALSE)
  map$ggplot_scale <- scale_fill_manual(values = cbPalette, drop = FALSE, labels = legendLabels, guide = FALSE)
  if (legend == 'legendandmap' || legend == 'legendonly') {
    map$ggplot_scale <- scale_fill_manual(values=cbPalette, drop = FALSE, labels = legendLabels)
  }
  renderMap(map, legend)
}

plotPercentDiffMap <- function(param1, param2, type, data, title, color, buckets, detail, legend, percent, zoom) {
  df <- as.data.frame(cbind(data[,1], (data[, param2] - data[, param1]) / data[, param1]))
  names(df) <- c("region", "value")
  # remove leading zeros from FIP codes
  # choropleth requires numeric column type in dataframe
  df$value <- as.numeric(as.character(df$value))
  legendLabels <- getLabels(buckets, percent)
  # set levels to be sure and INSIDE not afterward or else the values will get changed
  df$value <- factor(as.character(sapply(df$value, function(y) {
    if (y <= buckets[1]) {
      legendLabels[1]
    } else if (y <= buckets[2]) {
      legendLabels[2]
    } else if (y <= buckets[3]) {
      legendLabels[3]
    } else if (y <= buckets[4]) {
      legendLabels[4]
    } else if (y <= buckets[5]) {
      legendLabels[5]
    } else if (y <= buckets[6]) {
      legendLabels[6]
    } else if (y <= buckets[7]) {
      legendLabels[7]
    } else if (y <= buckets[8]) {
      legendLabels[8]
    } else if (y <= buckets[9]) {
      legendLabels[9]
    } else {
      legendLabels[10]
    }
  })), levels = make.unique(legendLabels))
  
  #additional customizations require creating a CountyChoropleth object, not using the county_choropleth() method
  if (type == 'census') {
    if (detail == 'County') {
      map <- CountyChoropleth$new(df)
    }
    if (detail == 'State' || detail == 'Region') {
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
      if (!is.null(zoom)) {
        map <- CountyZoomChoropleth$new(df)
        map$set_zoom(zoom)
      }
    }
    if (detail == 'State' || detail == 'Region') {
      df[, 1] <- as.character(df[, 1])
      map <- StateChoropleth$new(df)
    }
  }
  map$title <- title
  if (color == "Reds") {
    cbPalette <- colorRampPalette(c(colorRampPalette(c("white", "orange1"))(20)[2], "tomato1", "red3", "#660000"))(10)
  } 
  if (color == "Blues") {
    cbPalette <- colorRampPalette(c(colorRampPalette(c("white", "#0000ff"))(20)[2], "royalblue2", "#000099"))(10)
  }
  if (color == "Greens") {
    cbPalette <- colorRampPalette(c(colorRampPalette(c("white", "palegreen3"))(18)[2], "#66a266", "#003200"))(10)
  }
  if (color == "Red-Green") {
    numRed <- length(which(buckets < 0))
    cbPalette <- c(rev(colorRampPalette(c("#ffe5e5", "red3"))(numRed + 1))[-1], colorRampPalette(c("#e5f2e5", "palegreen3", "#004000"))(10 - numRed))
  }
  # scale_fill_brewer(name=NULL, labels = legendLabels, palette=color, drop=FALSE, guide = FALSE)
  map$ggplot_scale <- scale_fill_manual(values = cbPalette, drop = FALSE, labels = legendLabels, guide = FALSE)
  if (legend == 'legendandmap' || legend == 'legendonly') {
    map$ggplot_scale <- scale_fill_manual(values=cbPalette, drop = FALSE, labels = legendLabels)
  }
  renderMap(map, legend)
}

addStatCol <- function(string, data, pop1, pop2, updateProgress = NULL) {
  pop1 <- as.numeric(pop1)
  pop2 <- as.numeric(pop2)
  if (string == 'Binomial Exact') {
    data[, string] <- sapply(1:length(data[, 1]),
                             function(i) {
                               if (is.function(updateProgress)) {
                                 updateProgress(detail = NULL)
                               }
                               # ceiling to force whole number
                               if (pop1[i] != 0 && pop2[i] != 0) {
                                 round(binom.test(ceiling(data[i,2] * pop2[i]), pop2[i], data[i,3], alternative="two.sided")$p.value, 7)
                               } else {
                                 1.0
                               }
                             }
    )
    data <- data[order(data[, string], decreasing = FALSE), ]
  }
  if (string == 'Chi-squared') {
    data[, string] <- sapply(1:length(data[, 1]),
                             function(i) {
                               if (is.function(updateProgress)) {
                                 updateProgress(detail = NULL)
                               }
                               if ((ceiling(data[i,3] * pop2[i]) != 0) || ceiling(data[i,2] * pop2[i]) != 0) {
                                 # table takes non-negative integers. But assumes the data is in rates
                                 obs <- c(ceiling(data[i,3] * pop2[i]), ceiling((1 - data[i,3]) * pop2[i]))
                                 # ceiling to force whole number
                                 exp <- c(ceiling(data[i,2] * pop2[i]), ceiling((1 - data[i,2]) * pop2[i]))
                                 table <- as.data.frame(rbind(obs, exp))
                                 names(table) <- c('Has', 'Not Has')
                                 round(chisq.test(table)$p.value, 7)
                               } else {
                                 return(1.0)
                               }
                             })
    data <- data[order(data[, string], decreasing = FALSE), ]
  }
  if (string == 'Two Proportions') {
    data[, string] <- sapply(1:length(data[, 1]),
                             function(i) {
                               if (is.function(updateProgress)) {
                                 updateProgress(detail = NULL)
                               }
                               p1 <- data[i,2]
                               p2 <- data[i,3]
                               p <- (data[i,2] * pop1[i] + data[i,3] * pop2[i]) / (pop1[i] + pop2[i])
                               n1 <- pop1[i]
                               n2 <- pop2[i]
                               if (p != 0 && n1 != 0 && n2 != 0) {
                                 # assume independence. disable Yates correction
                                 round(z.test(p1, p2, p, n1, n2), 7)
                               } else {
                                 return(1.0)
                               }
                             })
    data <- data[order(data[, string], decreasing = FALSE), ]
  }
  if (string == 'Percent Difference') {
    data[, 'Percent Difference'] <- apply(data, 1, function(y) { 
      if (is.function(updateProgress)) {
        updateProgress(detail = NULL)
      }
      if (as.numeric(y[2]) == 0 | as.numeric(y[3]) == 0 | as.numeric(y[2]) == 0.0000 | as.numeric(y[3] == 0.0000)) 0.0
      else round((as.numeric(y[3]) - as.numeric(y[2])) / as.numeric(y[2]), 7)
    })
    data <- data[order(data[, string], decreasing = TRUE), ]
  }
  data
}

z.test <- function(p1, p2, p, n1, n2) {
  if (n1 == 0 || n2 == 0) {
    z <- 0.5
  } else {
    z <- -abs((p1 - p2)) / sqrt(p * (1 - p) * (1.0 / n1 + 1.0 / n2))
  }
  return(pnorm(z) * 2)
}