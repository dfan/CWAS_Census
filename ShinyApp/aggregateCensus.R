library(RMySQL)
library(maps)
library(choroplethrMaps)
data(state)
data(county.regions)
source('helper.R')
con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2000", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data2000 <- dbReadTable(conn = con, name = "acs")
dbDisconnect(con)
con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2010", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data2010 <- dbReadTable(conn = con, name = "acs")
vote <- read.csv('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census/Data/County/countyvoting_2012.csv', stringsAsFactors=FALSE, check.names=FALSE)
vote[, 'FIPS'] <- sapply(vote[, 'FIPS'], function(x) {
  if (nchar(as.character(x)) == 4) {
    paste0('0',x)
  } else {
    x
  }
})
alaska <- as.data.frame(cbind(rep('AK', 29), county.regions$county.fips.character[which(substr(county.regions$county.fips.character, 1, 2) == '02')], county.regions$county.name[which(substr(county.regions$county.fips.character, 1, 2) == '02')], matrix(as.numeric(rep(0, 29 * 3)), ncol = 3)), stringsAsFactors = FALSE)
names(alaska) <- names(vote)
vote <- rbind(vote, alaska)
# election 2012 data from Huffington post was missing Kalawao, Hawaii and all of Alaska. Alaska had no county data nor did Kalawao in the real election
vote <- rbind(vote, c('HI', '15005', 'kalawao', 0, 0, 0))
vote$County <- tolower(vote$County)
names(vote)[c(dim(vote)[2] - 1, dim(vote)[2])] <- c('blue2012', 'red2012')
list <- sapply(county.regions$county.fips.character, function(x) {
  # don't match by county name; the huffington post source sometimes uses county capital names
  as.numeric(which(vote$FIPS == x[1]))
})
countyVote <- as.data.frame(cbind(county.regions$county.fips.character,
                                  sapply(1:length(list), function(x) { 
                                    sum(as.numeric(vote[list[[x]], 'Total']))
                                  }),
                                  sapply(1:length(list), function(x) { 
                                    sum(as.numeric(vote[list[[x]], 'blue2012']))
                                  }),
                                  sapply(1:length(list), function(x) { 
                                    sum(as.numeric(vote[list[[x]], 'red2012']))
                                  })), stringsAsFactors = FALSE)
names(countyVote) <- c('FIPS', 'Total', 'blue2012', 'red2012')

countyVote[,3] <- sapply(1:length(countyVote[,2]), function(x) {
  if (as.numeric(countyVote[x, 'Total']) != 0) {
    as.numeric(countyVote[x,3]) / as.numeric(countyVote[x, 'Total'])
  } else {
    0
  }
})
countyVote[,4] <- sapply(1:length(countyVote[,2]), function(x) {
  if (countyVote[x, 'Total'] != 0) {
    as.numeric(countyVote[x,4]) / as.numeric(countyVote[x, 'Total'])
  } else {
    0
  }
})
stateVote <- vote[which(vote$FIPS == '0'),][, c('County', 'blue2012', 'red2012')]
stateVote[,2] <- as.numeric(stateVote[,2]) / as.numeric(vote[which(vote$FIPS == '0'),]$Total)
stateVote[,3] <- as.numeric(stateVote[,3]) / as.numeric(vote[which(vote$FIPS == '0'),]$Total)
names(stateVote) <- c('State', 'blue2012', 'red2012')
stateVote <- stateVote[order(stateVote[,1]),]
dataCombined <- cbind(data2000, data2010[, -1], data2010[, -1], countyVote[,3:4])
setwd('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census/ShinyApp')
colnames(dataCombined) <- c('county', read.csv('../Data/censusColNames.csv', stringsAsFactors=FALSE)[, 1])
write.table(dataCombined, file = 'censusCombined.csv', row.names = FALSE, sep = ',')
dbDisconnect(con)
zipTable <- read.csv('../Data/zcta_county.csv', stringsAsFactors=FALSE, colClasses=c("ZCTA5"="character", "STATE" = "character", "COUNTY" = "character"))

# don't aggregate state vote because you don't have 2012 population
dataState <- cbind(aggregateCensusToState(dataCombined[,1:22]), stateVote[,-1])
write.table(dataState, file = 'censusState.csv', row.names = FALSE, sep = ',')
# calculating voting in regions 2012
pop2012 <- read.csv('statepop2012_estimates.csv', stringsAsFactors=FALSE, check.names=FALSE)
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
names(regions) <- c('new_england', 'middle_atlantic', 'east_north_central', 'west_north_central', 'south_atlantic', 'east_south_central', 'west_south_central', 'mountain', 'pacific')
blueCol <- dataState$blue2012
for (x in 1:length(regions)) {
  blue <- sapply(regions[[x]], function(y) {
    dataState[which(dataState[,1] == y), 'blue2012'] * pop2012[which(dataState[,1] == y), 2]
  })
  indexes <- sapply(regions[[x]], function(y) {
    which(dataState[,1] == y)
  })
  blueCol[indexes] <- sum(blue) / sum(pop2012[indexes, 2])
}
redCol <- dataState$red2012
for (x in 1:length(regions)) {
  red <- sapply(regions[[x]], function(y) {
    dataState[which(dataState[,1] == y), 'red2012'] * pop2012[which(dataState[,1] == y), 2]
  })
  indexes <- sapply(regions[[x]], function(y) {
    which(dataState[,1] == y)
  })
  redCol[indexes] <- sum(red) / sum(pop2012[indexes, 2])
}
stateasRegionVote <- as.data.frame(cbind(blue2012 = blueCol, red2012 = redCol))

dataRegion <- cbind(aggregateCensusToRegion(dataState[1:22]), stateasRegionVote)
write.table(dataRegion, file = 'censusRegion.csv', row.names = FALSE, sep = ',')
regionVote <- as.data.frame(cbind(
sapply(1:length(regions), function(x) {
  blue <- sapply(regions[[x]], function(y) {
    dataState[which(dataState[,1] == y), 'blue2012'] * pop2012[which(dataState[,1] == y), 2]
  })
  indexes <- sapply(regions[[x]], function(y) {
    which(dataState[,1] == y)
  })
  sum(blue) / sum(pop2012[indexes, 2])
}),
sapply(1:length(regions), function(x) {
  red <- sapply(regions[[x]], function(y) {
    dataState[which(dataState[,1] == y), 'red2012'] * pop2012[which(dataState[,1] == y), 2]
  })
  indexes <- sapply(regions[[x]], function(y) {
    which(dataState[,1] == y)
  })
  redCol[indexes] <- sum(red) / sum(pop2012[indexes, 2])
})
))
names(regionVote) <- c('blue2012', 'red2012')
dataRegionOnly <- cbind(aggregateCensusRegionOnly(dataState[1:22]), regionVote)
write.table(dataRegionOnly, file = 'censusRegionOnly.csv', row.names = FALSE, sep = ',')

### some scripts
stateNames <- tolower(append(state.name, 'District of Columbia', which(state.name == 'Delaware')))
stateCodes <- state.regions[order(state.regions$region),]$fips.character
stateCol <- sapply(1:length(dataCombined[,1]), function(x) {
  return(stateNames[which(stateCodes == substr(dataCombined[x, 1], 1, 2))])
})
regionCol <- sapply(1:length(stateCol), function(x) {
  for (i in 1:length(regions)) {
    if (stateCol[x] %in% regions[[i]]) {
      return(names(regions)[i])
    }
  }
})
dataAggregated <- cbind(dataCombined[,1], stateCol, regionCol, dataCombined[,-1])
names(dataAggregated)[1:3] <- c('county', 'state', 'region')
write.table(dataAggregated, file = 'censusExtraCols.txt', row.names = FALSE, sep = '\t')