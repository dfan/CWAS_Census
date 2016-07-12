library(RMySQL)
library(choroplethr)
library(ggplot2)

# external file because function is non-reactive
plotMap <- function(string, data, title) {
  df <- as.data.frame(cbind(data$county, data[, string]))
  names(df) <- c("region", "value")
  # remove leading zeros from FIP codes
  # choropleth requires numeric column type in dataframe
  df$region <- as.numeric(sapply(df$region, function(y) sub('^0+([1-9])', '\\1', y)))
  df$value <- as.numeric(as.character(df$value))
  county_choropleth(df, title = "", legend = "", num_colors = 9, state_zoom = NULL, county_zoom = NULL, reference_map = FALSE) + ggtitle(title)
}

plotDiffMap <- function(string, data1, data2, title) {
  data <- abs(data2[, string] - data1[, string])
  df <- as.data.frame(cbind(data1$county, data))
  names(df) <- c("region", "value")
  # remove leading zeros from FIP codes
  # choropleth requires numeric column type in dataframe
  df$region <- as.numeric(sapply(df$region, function(y) sub('^0+([1-9])', '\\1', y)))
  df$value <- as.numeric(as.character(df$value))
  county_choropleth(df, title = "", legend = "", num_colors = 9, state_zoom = NULL, county_zoom = NULL, reference_map = FALSE) + ggtitle(title)
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
