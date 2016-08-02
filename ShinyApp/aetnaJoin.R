library(grid)
library(gridExtra)
library(ggplot2)
library(RMySQL)
# check.names=FALSE prevents column names from being changed from slash to period
aetna1 <- read.csv('/Users/dfan/Desktop/AETNA_Data/jolie_05_14_2012_to_05_14_2013.txt', colClasses = c('County' = 'character'), sep = '\t', check.names=FALSE)
aetna2 <- read.csv('/Users/dfan/Desktop/AETNA_Data/jolie_05_14_2013_to_05_14_2014.txt', colClasses = c('County' = 'character'), sep = '\t', check.names=FALSE)
aetna3 <- read.csv('/Users/dfan/Desktop/AETNA_Data/jolie_05_14_2014_to_05_14_2015.txt', colClasses = c('County' = 'character'), sep = '\t', check.names=FALSE)
aetna4 <- read.csv('/Users/dfan/Desktop/AETNA_Data/jolie_total_BRCA.txt', colClasses = c('County' = 'character'), sep = '\t')
allCounties <- unique(c(aetna1[,1], aetna2[,1], aetna3[,1], aetna4[,1]))
combined <- as.data.frame(cbind(allCounties, matrix(rep(0, length(allCounties) * 4), nrow = length(allCounties))))
combined[, -1] <- t(sapply(1:length(allCounties), function(i) {
  sapply(1:4, function(j) {
    if(length(which(get(paste0('aetna', j))[, 1] == allCounties[i])) > 0) {
      get(paste0('aetna', j))[, 2][which(get(paste0('aetna', j))[, 1] == allCounties[i])]
    } else {
      0
    }
  })
}))
# we don't want Puerto Rico
combined <- combined[-which(combined[,1] == '72031'),]
names(combined) <- c('County', '2013', '2014', '2015', 'Total')
write.table(combined[, -5], file = 'aetnaCombinedCounty.txt', row.names = FALSE, sep = '\t')
stateLevel <- aggregateUserToState(combined)
names(stateLevel) <- c('State', '2013', '2014', '2015', 'Total')
write.table(stateLevel[, -5], file = 'aetnaCombinedState.txt', row.names = FALSE, sep = '\t')
regionLevel <- aggregateRegionOnly(stateLevel)
names(regionLevel) <- c('Region', '2013', '2014', '2015', 'Total')
write.table(regionLevel[, -5], file = 'aetnaCombinedRegion.txt', row.names = FALSE, sep = '\t')

setwd('/Users/dfan/Desktop/AETNA_Data')
write.table(combined, file = 'aetnaCombined.txt', row.names = FALSE, sep = '\t')

###
### Making city zoom plots
###
# Houston: Harris (48201), Fort Bend (48157), Montgomery (48339), Brazoria (48039), Galveston (48167), Liberty (48291), Waller (48473), Chambers (48071)
# LA: Los Angeles (06037), Orange (06059), Ventura (06111), San Bernardino (06071), Riverside (06065)
# NYC: New York (36061), Kings (36047), Bronx (36005), Richmond (36085), Queens (36081)
# Boston: Norfolk (25021), Suffolk (25025), Plymouth (25023)
houston <- c(48201, 48157, 48339, 48039, 48167, 48291, 48473, 48071)
la <- c(06037, 06059, 06111, 06071, 06065)
nyc <- c(36061, 36047, 36005, 36085, 36081)
boston <- c(25021, 25025, 25023)
combined[, 1] <- as.numeric(sapply(combined[, 1], function(y) sub('^0+([1-9])', '\\1', y)))
names <- names(combined)

# Houston 3x1
list <- lapply(2:4, function(i) {
  plotMap(names[i], 'user', combined, paste0('Houston Colored by ', names[i]), 'Reds', getBuckets(combined[, names[i]], ''), 'County', '', FALSE, houston)$render()
})
legend <- list(plotMap('Total', 'user', combined, paste0('Houston Colored by Total'), 'Reds', getBuckets(c(combined[, 2], combined[, 3], combined[, 4]), ''), 'County', 'legendonly', FALSE, houston))
final <- c(list, legend)
plot <- grid.arrange(grobs = final, ncol = 1, layout_matrix = rbind(matrix(1:3, byrow = TRUE, nrow = 3), rep(4, 3)))
ggsave('brca3x1_houston_decile.pdf', plot, width = 8.5, height = 11, dpi = 350)

# Houston Total
list <- lapply(5:5, function(i) {
  plotMap(names[i], 'user', combined, paste0('Houston Colored by ', names[i]), 'Reds', getBuckets(combined[, names[i]], ''), 'County', 'legendandmap', FALSE, houston)$render()
})
final <- c(list)
plot <- grid.arrange(grobs = final, ncol = 1)
ggsave('brcatotal_houston_decile.pdf', plot, width = 11, height = 8.5, dpi = 350)

# LA 3x1
list <- lapply(2:4, function(i) {
  plotMap(names[i], 'user', combined, paste0('LA Colored by ', names[i]), 'Reds', getBuckets(combined[, names[i]], ''), 'County', '', FALSE, la)$render()
})
legend <- list(plotMap('Total', 'user', combined, paste0('LA Colored by Total'), 'Reds', getBuckets(c(combined[, 2], combined[, 3], combined[, 4]), ''), 'County', 'legendonly', FALSE, la))
final <- c(list, legend)
plot <- grid.arrange(grobs = final, ncol = 1, layout_matrix = rbind(matrix(1:3, byrow = TRUE, nrow = 3), rep(4, 3)))
ggsave('brca3x1_la_decile.pdf', plot, width = 8.5, height = 11, dpi = 350)

# LA Total
list <- lapply(5:5, function(i) {
  plotMap(names[i], 'user', combined, paste0('LA Colored by ', names[i]), 'Reds', getBuckets(combined[, names[i]], ''), 'County', 'legendandmap', FALSE, la)$render()
})
final <- c(list)
plot <- grid.arrange(grobs = final, ncol = 1)
ggsave('brcatotal_la_decile.pdf', plot, width = 11, height = 8.5, dpi = 350)

# NYC 3x1
names <- names(combined)
list <- lapply(2:4, function(i) {
  plotMap(names[i], 'user', combined, paste0('NYC Colored by ', names[i]), 'Reds', getBuckets(combined[, names[i]], ''), 'County', '', FALSE, nyc)$render()
})
legend <- list(plotMap('Total', 'user', combined, paste0('NYC Colored by Total'), 'Reds', getBuckets(c(combined[, 2], combined[, 3], combined[, 4]), ''), 'County', 'legendonly', FALSE, nyc))
final <- c(list, legend)
plot <- grid.arrange(grobs = final, ncol = 1, layout_matrix = rbind(matrix(1:3, byrow = TRUE, nrow = 3), rep(4, 3)))
ggsave('brca3x1_nyc_decile.pdf', plot, width = 8.5, height = 11, dpi = 350)

# NYC Total
list <- lapply(5:5, function(i) {
  plotMap(names[i], 'user', combined, paste0('NYC Colored by ', names[i]), 'Reds', getBuckets(combined[, names[i]], ''), 'County', 'legendandmap', FALSE, nyc)$render()
})
final <- c(list)
plot <- grid.arrange(grobs = final, ncol = 1)
ggsave('brcatotal_nyc_decile.pdf', plot, width = 11, height = 8.5, dpi = 350)

# Boston 3x1
names <- names(combined)
list <- lapply(2:4, function(i) {
  plotMap(names[i], 'user', combined, paste0('Boston Colored by ', names[i]), 'Reds', getBuckets(combined[, names[i]], ''), 'County', '', FALSE, boston)$render()
})
legend <- list(plotMap('Total', 'user', combined, paste0('Boston Colored by Total'), 'Reds', getBuckets(c(combined[, 2], combined[, 3], combined[, 4]), ''), 'County', 'legendonly', FALSE, boston))
final <- c(list, legend)
plot <- grid.arrange(grobs = final, ncol = 1, layout_matrix = rbind(matrix(1:3, byrow = TRUE, nrow = 3), rep(4, 3)))
ggsave('brca3x1_boston_decile.pdf', plot, width = 8.5, height = 11, dpi = 350)

# Boston Total
list <- lapply(5:5, function(i) {
  plotMap(names[i], 'user', combined, paste0('Boston Colored by ', names[i]), 'Reds', getBuckets(combined[, names[i]], ''), 'County', 'legendandmap', FALSE, boston)$render()
})
final <- c(list)
plot <- grid.arrange(grobs = final, ncol = 1)
ggsave('brcatotal_boston_decile.pdf', plot, width = 11, height = 8.5, dpi = 350)

# Regions 3x1
# don't remove 0s from combined
region <- aggregateUserToRegion(aggregateUserToState(combined))
list <- lapply(2:4, function(i) {
  plotMap(names[i], 'user', region, paste0('USA Regions Colored by ', names[i]), 'Reds', getBuckets(region[, names[i]], ''), 'State', '', FALSE, NULL)$render()
})
legend <- list(plotMap('Total', 'user', region, paste0('USA Regions Colored by Total'), 'Reds', getBuckets(c(region[, 2], region[, 3], region[, 4]), ''), 'State', 'legendonly', FALSE, NULL))
final <- c(list, legend)
plot <- grid.arrange(grobs = final, ncol = 1, layout_matrix = rbind(matrix(1:3, byrow = TRUE, nrow = 3), rep(4, 3)))
ggsave('brca3x1_regions_decile.pdf', plot, width = 8.5, height = 11, dpi = 350)

# Regions Total
# don't remove 0s from combined
region <- aggregateUserToRegion(aggregateUserToState(combined))
list <- lapply(5:5, function(i) {
  plotMap(names[i], 'user', region, paste0('USA Regions Colored by ', names[i]), 'Reds', getBuckets(region[, names[i]], ''), 'State', 'legendandmap', FALSE, NULL)$render()
})
final <- c(list)
plot <- grid.arrange(grobs = final, ncol = 1)
ggsave('brcatotal_regions_decile.pdf', plot, width = 11, height = 8.5, dpi = 350)

# bar chart of states ranked by descending value of [count from may 2013 to may 2014] / [count from may 2012 to may 2013]
combined <- as.data.frame(cbind(allCounties, matrix(rep(0, length(allCounties) * 4), nrow = length(allCounties))))
combined[, -1] <- t(sapply(1:length(allCounties), function(i) {
  sapply(1:4, function(j) {
    if(length(which(get(paste0('aetna', j))[, 1] == allCounties[i])) > 0) {
      get(paste0('aetna', j))[, 2][which(get(paste0('aetna', j))[, 1] == allCounties[i])]
    } else {
      0
    }
  })
}))
combined <- combined[-which(combined[,1] == '72031'),]
names(combined) <- c('County', 'BRCA 5/14/2012 to 5/14/2013', 'BRCA 5/14/2013 to 5/14/2014', 'BRCA 5/14/2014 to 5/14/2015', 'Total')

stateData <- aggregateUserToState(combined)
data <- as.data.frame(cbind(stateData[,1], lapply(1:length(stateData[,1]), function(i) {
  stateData[i,3] / stateData[i,2]
})))
data[, 2] <- as.numeric(data[, 2])
data <- data[order(data[,2], decreasing = TRUE),]
data <- data[-which(data[,2] == Inf),]
data[, 1] <- factor(data[, 1], levels = data[,1])
# Hawaii wasn't in the data in 2012-2013
names(data) <- c('State', 'Value') 
plot <- ggplot(data = data, aes(x = State, y = Value, fill = State)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position="none") + 
  scale_y_continuous("Ratio of 2013-2014 over 2012-2013", breaks = seq(0, 3, by = 0.25), labels = (seq(0, 3, by = 0.25))) + 
  ggtitle('States by BRCA Ratio 2014 to 2013') + theme(plot.margin=grid::unit(c(15,15,15,15), "mm"))
ggsave('brcaratio_barplot_state.pdf', plot, width = 11, height = 8.5, dpi = 350)
# by region instead of state
stateData <- aggregateRegionOnly(aggregateUserToState(combined))
data <- as.data.frame(cbind(stateData[,1], lapply(1:length(stateData[,1]), function(i) {
  stateData[i,3] / stateData[i,2]
})))
data[, 2] <- as.numeric(data[, 2])
data <- data[order(data[,2], decreasing = TRUE),]
data[, 1] <- factor(data[, 1], levels = data[,1])
# Hawaii wasn't in the data in 2012-2013
names(data) <- c('Region', 'Value') 
plot <- ggplot(data = data, aes(x = Region, y = Value, fill = Region)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position="none") + 
  scale_y_continuous("Ratio of 2013-2014 over 2012-2013", breaks = seq(0, 3, by = 0.25), labels = (seq(0, 3, by = 0.25))) + 
  ggtitle('Regions by BRCA Ratio 2014 to 2013') + theme(plot.margin=grid::unit(c(15,15,15,15), "mm"))
ggsave('brcaratio_barplot_region.pdf', plot, width = 11, height = 8.5, dpi = 350)
# filter out top quartile of states with the most BRCA testing (not quartile of ratios)
stateData <- aggregateUserToState(combined)
data <- as.data.frame(cbind(stateData[,1], lapply(1:length(stateData[,1]), function(i) {
  stateData[i,3] / stateData[i,2]
})))
data[, 2] <- as.numeric(data[, 2])
data <- data[which(stateData[, 5] > quantile(stateData[, 5], c(0.75))),]
data <- data[order(data[,2], decreasing = TRUE),]
data[, 1] <- factor(data[, 1], levels = data[,1])
names(data) <- c('State', 'Value')
plot <- ggplot(data = data, aes(x = State, y = Value, fill = State)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position="none") + 
  scale_y_continuous("Ratio of 2013-2014 over 2012-2013", breaks = seq(0, 3, by = 0.25), labels = (seq(0, 3, by = 0.25))) + ggtitle('Top 25% of States by Total BRCA Testing - BRCA Ratio 2014 to 2013')
ggsave('brcaratio_barplot_topquartile_state.pdf', plot, width = 11, height = 8.5, dpi = 350)

# Plot of US Region by ratio
region <- aggregateUserToRegion(aggregateUserToState(combined))
data <- as.data.frame(cbind(stateData[,1], as.list(region[,3] / region[,2])))
# no N/A values since as a region it's aggregated
data[, 2] <- as.numeric(data[, 2])
data[, 1] <- as.character(data[, 1])
# Hawaii wasn't in the data in 2012-2013
names(data) <- c('State', 'Value') 
list <- list(plotMap('Value', 'user', data, paste0('USA Regions Colored by Ratio Between 2013-2014 and 2012-2013'), 'Reds', getBuckets(data[, 'Value'], ''), 'State', 'legendandmap', TRUE, NULL)$render() + theme(plot.margin=grid::unit(c(15,15,15,15), "mm")))
final <- c(list)
plot <- grid.arrange(grobs = final, ncol = 1)
ggsave('brcaratio_region.pdf', plot, width = 11, height = 8.5, dpi = 350)

# Plot of States by difference between 2013 and 2014 as a % without states with total count < 100
stateData <- aggregateUserToState(combined)
stateData <- stateData[which(stateData[,2] != 0), ]
stateData <- stateData[which(stateData$Total >= 100), ]
data <- as.data.frame(cbind(stateData[,1], as.list((stateData[,3] - stateData[,2]) / stateData[,2])))
data[, 2] <- as.numeric(data[, 2])
data[, 1] <- as.character(data[, 1])
names(data) <- c('State', 'Value')
list <- list(plotMap('Value', 'user', data, paste0('USA Colored by % Difference in ', names[2], ' and ', names[3]), 'Red-Green', getBuckets(data[, 'Value'], 'Percent'), 'State', 'legendandmap', TRUE, NULL)$render())
final <- c(list)
plot <- grid.arrange(grobs = final, ncol = 1)
ggsave('brcadifferenceratio_state_filtered_decile.pdf', plot, width = 11, height = 8.5, dpi = 350)

### Ratio scatter plots
source('helper.R')
con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2000", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data2000 <- dbReadTable(conn = con, name = "acs")
dbDisconnect(con)
con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2010", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data2010 <- dbReadTable(conn = con, name = "acs")
censusCounty <- cbind(data2000, data2010[, -1], data2010[, -1])
censusState <- read.csv('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census/ShinyApp/censusState.csv', stringsAsFactors=FALSE, check.names=FALSE)
censusState <- read.csv('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census/ShinyApp/censusState.csv', stringsAsFactors=FALSE, check.names=FALSE, colClasses = c('character', sapply(names(censusState), function(x) class(censusState[,x]))[-1]))
censusRegion <- read.csv('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census/ShinyApp/censusRegionOnly.csv', stringsAsFactors=FALSE, check.names=FALSE)
censusRegion <-  read.csv('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census/ShinyApp/censusRegionOnly.csv', stringsAsFactors=FALSE, check.names=FALSE, colClasses = c('character', sapply(names(censusState), function(x) class(censusState[,x]))[-1]))
colnames(censusCounty) <- c('county', read.csv('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census//Data/censusColNames.csv', stringsAsFactors=FALSE)[, 1])
dbDisconnect(con)
names <- names(censusCounty)

countyIndexes <- sapply(combined[,1], function(x) which(censusCounty[,1] == x))
stateIndexes <- sapply(stateLevel[,1], function(x) which(censusState[,1] == x))
regionIndexes <- sapply(regionLevel[,1], function(x) which(censusRegion[,1] == x))
setwd('/Users/dfan/Desktop/AETNA_Data')
for (i in 2:length(names)) {
  png(paste0('jolie_', names[i], '.png'), width = 1600, height = 800, units = "px", res = 150)
  par(mfrow=c(1,3))
  plot1 <- plot(censusCounty[countyIndexes, i], log(combined[,3] / combined[,2]), main=paste0("Log Jolie Effect by ", names[i], ' in County'), xlab=names[i], ylab="Log 2014/2013", ylim=c(-2.0, 2.0), pch=1)
  plot2 <- plot(censusState[stateIndexes, i], log(stateLevel[,3] / stateLevel[,2]), main=paste0("Log Jolie Effect by ", names[i], ' in State'), xlab=names[i], ylab="Log 2014/2013", ylim=c(-2.0, 2.0), pch=1)
  plot3 <- plot(censusRegion[regionIndexes, i], log(regionLevel[,3] / regionLevel[,2]), main=paste0("Log Jolie Effect by ", names[i], ' in Region'), xlab=names[i], ylab="Log 2014/2013", ylim=c(-2.0, 2.0), pch=1)
  dev.off()
}

