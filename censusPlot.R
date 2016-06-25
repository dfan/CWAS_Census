library(RMySQL)
library(choroplethr)
library(ggplot2)

con <- dbConnect(MySQL(), user = "root", password = "root", dbname = "census2010", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
data <- dbReadTable(conn = con, name = "acs")
dbDisconnect(con)

# data frame must have one column called "region" and another called "value"
df <- as.data.frame(cbind(data$county, data$medianincome))
names(df) <- c("region", "value")
# remove leading zeros from FIP codes. Choropleth requires numeric column type in dataframe
df$region <- as.numeric(sapply(df$region, function(y) sub('^0+([1-9])', '\\1', y)))

# set to directory you want the images to appear in
setwd('/Users/dfan/Dropbox/Research\ Lab\ Projects/Undergraduate/Harvard-MIT\ 2016/Code/CWAS_Census')

df$value <- as.numeric(as.character(data$medianincome))
png('medianIncome.png', units="px", width=1600, height=1000, res=150)
county_choropleth(df, title = "", legend = "", num_colors = 7, state_zoom = NULL, county_zoom = NULL, reference_map = FALSE) + ggtitle("Counties by Median Income")
dev.off()

df$value <- as.numeric(as.character(data$population))
png('population.png', units="px", width=1600, height=1000, res=150)
county_choropleth(df, title = "", legend = "", num_colors = 7, state_zoom = NULL, county_zoom = NULL, reference_map = FALSE) + ggtitle("Counties by Total Population")
dev.off()

df$value <- as.numeric(as.character(data$black))
png('blackFrac.png', units="px", width=1600, height=1000, res=150)
county_choropleth(df, title = "", legend = "", num_colors = 7, state_zoom = NULL, county_zoom = NULL, reference_map = FALSE) + ggtitle("Counties by Black Fraction")
dev.off()

df$value <- as.numeric(as.character(data$white))
png('whiteFrac.png', units="px", width=1600, height=1000, res=150)
county_choropleth(df, title = "", legend = "", num_colors = 7, state_zoom = NULL, county_zoom = NULL, reference_map = FALSE) + ggtitle("Counties by White Fraction")
dev.off()