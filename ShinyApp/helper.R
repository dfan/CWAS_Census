library(RMySQL)
library(choroplethr)
library(ggplot2)
library(gridExtra)
data(state)

aggregateToState <- function(data) {
  stateCode <- unique(substr(data[, 1], 1, 2))
  # initialize another df of 51 rows and same # of cols
  stateNames <- tolower(append(state.name, 'District of Columbia', which(state.name == 'Delaware')))
  df <- cbind(tolower(stateNames), data[, -1][1:length(stateCode), ])
  names(df) <- c('state', names(data)[-1])
  # all counties in the same state start with the same two digits in their FIPS code
  # ranges from 01 to 56 but some numbers are skipped like 03.
  df[, -1] <- t(sapply(1:length(stateCode), function(i) {
    sapply(2:dim(df)[2], function(j) {
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

getMedianList <- function(data, pop) {
  do.call('c', lapply(1:length(data), function(i) {
    rep(data[i], pop[i])
  }))
}

getWeighted <- function(data, pop) {
  list <- sapply(1:length(data), function(i) {
    data[i] * pop[i]
  })
  return(sum(list) / sum(pop))
}

# external file because function is non-reactive
plotMap <- function(string, data, title, buckets, detail, legend) {
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
  if (detail == 'County') {
    map <- CountyChoropleth$new(df)
  }
  if (detail == 'State') {
    df[, 1] <- as.character(df[, 1])
    map <- StateChoropleth$new(df)
  }
  map$title <- title
  
  if (string == 'red2000' || string == 'red2008' || string == 'red2010') {
    map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Reds", drop=FALSE, guide = FALSE)
    if (legend == 'legendandmap' || legend == 'legendonly') {
      map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Reds", drop=FALSE)
    }
  } else if (string == 'blue2000' || string == 'blue2008' || string == 'blue2010') {
    map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Blues", drop=FALSE, guide = FALSE)
    if (legend == 'legendandmap' || legend == 'legendonly') {
      map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Blues", drop=FALSE)
    }
  } else {
    map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Greens", drop=FALSE, guide = FALSE)
    if (legend == 'legendandmap' || legend == 'legendonly') {
      map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Greens", drop=FALSE)
    }
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

getBuckets <- function(data1, data2, data3) {
  if (!is.null(data2) & !is.null(data3)) {
    if (class(data1) == 'integer' && class(data2) == 'integer' && class(data3) == 'integer')
      return(floor(quantile(cbind(data1,data2,data3), c(1/7.0, 2/7.0, 3/7.0, 4/7.0, 5/7.0, 6/7.0, 1.0))))
    return(round(quantile(cbind(data1,data2,data3), c(1/7.0, 2/7.0, 3/7.0, 4/7.0, 5/7.0, 6/7.0, 1.0)), 5))
  }
  if (!is.null(data2) & is.null(data3)) {
    if (class(data1) == 'integer' && class(data2) == 'integer')
      return(floor(quantile(cbind(data1,data2), c(1/7.0, 2/7.0, 3/7.0, 4/7.0, 5/7.0, 6/7.0, 1.0))))
    return(round(quantile(cbind(data1,data2), c(1/7.0, 2/7.0, 3/7.0, 4/7.0, 5/7.0, 6/7.0, 1.0)), 5))
  }
  if (class(data1) != 'integer')
    return(round(quantile(data1, c(1/7.0, 2/7.0, 3/7.0, 4/7.0, 5/7.0, 6/7.0, 1.0)), 5))
  return(floor(quantile(data1, c(1/7.0, 2/7.0, 3/7.0, 4/7.0, 5/7.0, 6/7.0, 1.0))))
}

getLabels <- function(buckets) {
  y <- c(
         paste0('[0 to ', format(buckets[1], big.mark=","), ')', sep=""),
         paste0('[', format(buckets[1],  big.mark=","), ' to ', format(buckets[2], big.mark = ","), ')'),
         paste0('[', format(buckets[2], big.mark=","), ' to ', format(buckets[3], big.mark = ","), ')'),
         paste0('[', format(buckets[3],  big.mark=","), ' to ', format(buckets[4], big.mark = ","), ')'),
         paste0('[', format(buckets[4],  big.mark=","), ' to ', format(buckets[5], big.mark = ","), ')'),
         paste0('[', format(buckets[5],  big.mark=","), ' to ', format(buckets[6], big.mark = ","), ')'),
         paste0('[', format(buckets[6],  big.mark=","), ' to ', format(buckets[7], big.mark = ","), ']')
        )

  #y <- factor(y, labels = y, ordered = TRUE)
  return(y)
}

plotDiffMap <- function(param1, param2, data, title, buckets, detail, legend) {
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
  if (detail == 'County') {
    map <- CountyChoropleth$new(df)
  }
  if (detail == 'State') {
    df[, 1] <- as.character(df[, 1])
    map <- StateChoropleth$new(df)
  }
  map$title = title
  if (param1 == 'red2000' || param1 == 'red2008' || param1 == 'red2010') {
    map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Reds", drop=FALSE, guide = FALSE)
    if (legend == 'legendandmap' || legend == 'legendonly') {
      map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Reds", drop=FALSE)
    }
  } else if (param1 == 'blue2000' || param1 == 'blue2008' || param1 == 'blue2010') {
    map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Blues", drop=FALSE, guide = FALSE)
    if (legend == 'legendandmap' || legend == 'legendonly') {
      map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Blues", drop=FALSE)
    }
  } else {
    map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Greens", drop=FALSE, guide = FALSE)
    if (legend == 'legendandmap' || legend == 'legendonly') {
      map$ggplot_scale <- scale_fill_brewer(name=NULL, labels = legendLabels, palette="Greens", drop=FALSE)
    }
  }
  renderMap(map, legend)
}


addStatCol <- function(string, data, pop1, pop2) {
  if (string == 'Binomial Exact') {
    data[, string] <- sapply(1:length(data[, 1]), 
                             function(i) {
                                            # ceiling to force whole number
                                            if (data[i,2] != 0 & data[i,3] != 0) {
                                              binom.test(ceiling(data[i,2] * pop2[i]), pop2[i], data[i,3], alternative="two.sided")$p.value
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
                                              chisq.test(table)$p.value
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
                                   z.test(data[i,2], data[i,3], (data[i,2] * pop1[i] + data[i,3] * pop2[i]) / (pop1[i] + pop2[i]), pop1[i], pop2[i])
                               } else {
                                 return(1.0)
                               }
                             })
    data <- data[order(data[, string], decreasing = FALSE), ]
  }
  if (string == 'Percent Difference') {
    data[, 'Percent Difference'] <- apply(data, 1, function(y) { if (as.numeric(y[2]) == 0 | as.numeric(y[3]) == 0 | as.numeric(y[2]) == 0.0000 | as.numeric(y[3] == 0.0000)) 0.0
      else abs(as.numeric(y[3]) - as.numeric(y[2])) / as.numeric(y[2])})
    data <- data[order(data[, string], decreasing = TRUE), ]
  }
  data
}

z.test <- function(p1, p2, p, n1, n2) {
  z <- -abs((p1 - p2)) / sqrt(p * (1 - p) * (1.0 / n1 + 1.0 / n2))
  return(pnorm(z) * 2)
}
