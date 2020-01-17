require(ggplot2)
require(grid)
require(dplyr)
require(reshape2)
require(tidyverse)
require(RColorBrewer)
require(AdvancedVisualization)

code <- c("FB", "AAPL", "MSFT", "T")
start_date <- "2016-12-31"
end_date <- "2018-12-31"
collapse <- "daily"
api_key <- "Xsgzx1TsTQY6YFLRf8at"

data <- get.market.data(code, start_date, end_date, collapse, api_key)

# Show names
names(data)

# Show df
data$FB

# Show names(df)
names(data$FB)

# Open price for all
plot_timeseries(data, column_names = c("Open"), collapse = "2 months")

# Close price for all
plot_timeseries(data, column_names = c("Close"), collapse = "1 month")

# Close and Open
plot_timeseries(data, column_names = c("High", "Low"), collapse = "2 months")

# Close and Open
plot_timeseries(data, column_names = c("High", "Low"), codes=c("FB", "AAPL"), collapse = "2 months")

# Add function
plot_timeseries(data, column_names = c("Open"), codes=c("FB", "AAPL"), collapse = "2 months", func = y~sqrt(x))

# New dataset
code <- c("GOOG", "INTC", "KO", "A")
data <- get.market.data(code, start_date, end_date, collapse, api_key)

plot_timeseries(data, column_names = c("Open"), collapse = "2 months")

# Plot all
plot_timeseries_all(data, column_names = c("Open"), by="codes", func = y~sqrt(x))
plot_timeseries_all(data, column_names = c("Volume"), by="codes")

plot_timeseries_all(data, column_names = c("Close", "Open", "High", "Low"), by="column_names", codes = "GOOG")


code <- c("C", "JPM", "BAC", "WFC")
data <- get.market.data(code, start_date, end_date, collapse, api_key)

plot_barchart(data, column_names=c("Open", "Open", "High", "Low"))

plot_piechart(data, w=c(0.2, 0.2,0.2, 0.2))
