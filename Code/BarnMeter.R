rm(list=ls())
generate = F
generate_incomplete = TRUE

sterling <- scales::dollar_format(prefix = "£")    # Define formatter for Sterling

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(zoo)

source("funcs/interpolation.R")
source("funcs/graph_and_predict.R")
source("funcs/data_generation.R")
source("funcs/calc_change_cols.R")

if (generate) {
    readings <- generate_data('2019/03/09', '2021/08/08', 27505, 30)
    print(readings)
} else {
    # Import CSV
    readings <- read.csv('../Data/readings.csv')
    readings$date <- as.Date(readings$date, format = "%d/%m/%Y")
}

readings <- interpolate(readings)

usage_statistics <- function(df, ppu = 0.14){
    # Take a df of readings along with the price per unit and return vital statistics
    total_usage <- max(df$measurement) - min(df$measurement)
    total_usage_cost <- sterling(total_usage * ppu)
    median_daily_usage <- median(df$change[which(!is.na(df$change))])
    median_daily_usage_cost <- sterling(median_daily_usage * ppu)
    statistic <- c("Total Usage:", "Total Usage Cost:", "Median Daily Usage:", "Median Daily Cost")
    value <- c(total_usage, total_usage_cost, median_daily_usage, median_daily_usage_cost)
    outdf <- data.frame(statistic, value)
    names(outdf) <- c("", "")
    print.data.frame(outdf, row.names = FALSE)
}

readings$change <- calc_change(readings$measurement)
readings$change_residuals <- calc_change_residiuals(readings$change)

print(interpolate(readings))
usage_statistics(readings, 0.14)

plot_usage(readings, 100)
plot_usage(readings, 100, as.cost = TRUE, ppu = 0.14)
plot_residual_change(readings)

readings_cal_heatmap <- generate_cal_heatmap_df(readings)
plot_cal_heatmap(readings_cal_heatmap)

# testdf <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/yahoo.csv")

