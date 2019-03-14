generate = FALSE
generate_incomplete = TRUE

sterling <- scales::dollar_format(prefix = "£")    # Define formatter for Sterling

library(dplyr)
library(tidyr)
library(ggplot2)

source("funcs/interpolation.R")
source("funcs/graph_and_predict.R")

if (generate) {
    date <- seq.Date(as.Date('2019/03/09'), as.Date('2019/08/08'), 'day')
    date_length <- length(date)
    starting_reading <- 27505
    increment <- 30
    measurement_seq <- seq(0, date_length - 1)
    measurement <- measurement_seq * increment
    measurement <- measurement + starting_reading
    flucs <- as.integer(rnorm(date_length, 0, 5))
    measurement <- measurement + flucs
    measurement[1] <- starting_reading
    interpolated <- as.logical(rep(NA, date_length))
    if (generate_incomplete) {
        # measurement[unique(as.integer(runif(as.integer(date_length/10), 1, date_length)))] <- NA
        measurement[sample(1:date_length, as.integer(date_length/10))] <- NA
    }
    readings <- data.frame(date, measurement, interpolated)
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

calc_change <- function(v){
    # Take a vector of measurements and calculate change between time t-1 and time t
    start_dates <- v[1:length(v)-1]
    end_dates <- v[2:length(v)]
    change <- end_dates - start_dates
    change <- c(NA, change)
    return(change)
}

calc_change_residiuals <- function(v){
    # Take a vector of change and calculate median residuals
    median_change <- median(v[which(!is.na(v))])
    change_resids <- v - median_change
    return(change_resids)
}

plot_usage <- function(df, predict=0, as.cost = FALSE, ppu = 0.14){
    # Take a df of readings and plot usage by day
    if (as.cost) {
        df$measurement <- (df$measurement - min(df$measurement)) * ppu
        df$change <- df$change * ppu
    }
    
    if (predict > 0) {
        date <- seq.Date(max(df$date), length.out = predict + 1, by = 'day')
        max_prediction <- predict_max_usage(df, predict)
        median_prediction <- predict_median_usage(df, predict)
        min_prediction <- predict_min_usage(df, predict)
        median_7day_prediction <- predict_median_7day_usage(df, predict)
        
        forecast <- data.frame(date, max_prediction, median_prediction, median_7day_prediction, min_prediction)
        names(forecast) <- c("date", "Max", "Median", "Median (7d)", "Min")
        forecast <- gather(forecast, "prediction", "measurement", 2:5)
    }
    
    p <- ggplot(df, aes(x = date, y = measurement)) +
        geom_line() + geom_line(data=forecast, aes(x = date, y = measurement, colour = prediction))
    if (as.cost) {
        p <- p + ylab("Cost") + scale_y_continuous(labels = sterling)
    }
    print(p)
}

plot_residual_change <- function(df){
    # Take a df of readings and plot residual change
    df <- df[which(!is.na(df$change_residuals)),]
    df$residual_type <- ifelse(df$change_residuals > 0, "worse", "better")
    df$residual_type[which(df$change_residuals == 0)] <- "zero"
    p <- ggplot(df, aes(x = date, y = change_residuals)) +
        scale_colour_manual(values = c("better"="#00ba38", "worse"="#f8766d", "zero"="#6db4f8")) +
        geom_segment(aes(y = 0, x = date, yend = change_residuals, xend = date, col = residual_type), alpha = 0.5) +
        geom_point(stat = "identity", aes(col = residual_type), size = 2) +
        scale_y_continuous(limits=c(-max(abs(df$change_residuals)), max(abs(df$change_residuals))))
        # theme_bw()
    print(p)
}

readings$change <- calc_change(readings$measurement)
readings$change_residuals <- calc_change_residiuals(readings$change)

print(interpolate(readings))
usage_statistics(readings, 0.14)

plot_usage(readings, 5)
plot_usage(readings, 149, as.cost = TRUE, ppu = 0.14)
plot_residual_change(readings)
