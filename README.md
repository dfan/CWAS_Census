# CWAS_Census
A R/Shiny app that plots choropleth maps of the USA (by county) colored in density by median income, fraction of black/white demographics and total population. Uses choroplethR package in R.
Raw files were downloaded from the [2010 Census](https://www.census.gov/support/USACdataDownloads.html#RHI) and selected columns were extracted into `/Data/Raw_CompiledData.xlsx`. Column titles were changed before exporting to `/Data/ACS_2010.csv`. `cleanData.py` removes entries in the form xx000 (cumulate data for each state/unincorporated territory) and null values that arise for counties with population of 0 (30113, 51560, 51780). This data was staged in a local MySQL database.

`censusPlot.R` demonstrates how choropleth plots may be generated. The code in `censusPlot.R` was copied over to the server-side script in the Shiny App. To run the Shiny App, click "Run App" after opening `/ShinyApp/server.R` in RStudio. Below are plots of the USA colored by median income, black population, white population and total population by county.

[](medianIncome.png)

[](blackFrac.png)

[](whiteFrac.png)

[](population.png)
