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