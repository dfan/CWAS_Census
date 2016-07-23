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
names(combined) <- c('County', 'BRCA 5/14/2012 to 5/14/2013', 'BRCA 5/14/2013 to 5/14/2014', 'BRCA 5/14/2014 to 5/14/2015', 'Total')
setwd('/Users/dfan/Desktop/AETNA_Data')
write.table(combined, file = 'aetnaCombined.txt', row.names = FALSE, sep = '\t')