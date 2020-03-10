library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

predict_max_usage <- function(df, predict){
    date <- seq.Date(max(df$date), length.out = predict + 1, by = 'day')
    base_vec <- rep(df$measurement[nrow(df)], predict + 1)
    max_prediction <- seq(0, (predict)) * max(df$change[which(!is.na(df$change))]) + base_vec
    return(max_prediction)
}

predict_median_usage <- function(df, predict){
    date <- seq.Date(max(df$date), length.out = predict + 1, by = 'day')
    base_vec <- rep(df$measurement[nrow(df)], predict + 1)
    median_prediction <- seq(0, (predict)) * median(df$change[which(!is.na(df$change))]) + base_vec
    return(median_prediction)
}

predict_min_usage <- function(df, predict){
    date <- seq.Date(max(df$date), length.out = predict + 1, by = 'day')
    base_vec <- rep(df$measurement[nrow(df)], predict + 1)
    min_prediction <- seq(0, (predict)) * min(df$change[which(!is.na(df$change))]) + base_vec
    return(min_prediction)
}

predict_median_7day_usage <- function(df, predict){
    date <- seq.Date(max(df$date), length.out = predict + 1, by = 'day')
    base_vec <- rep(df$measurement[nrow(df)], predict + 1)
    days_to_measure <- 7
    if (length(df$change[which(!is.na(df$change))]) < 7){
        days_to_measure <- length(df$change[which(!is.na(df$change))])
    }
    median_change_vec <- df$change[which(!is.na(df$change))]
    median_7day_prediction <- seq(0, (predict)) * median(median_change_vec[(length(median_change_vec)-(days_to_measure-1)):length(median_change_vec)]) + base_vec
    return(median_7day_prediction)
}

plot_usage <- function(df, predict=0, as.cost = FALSE, ppu = 0.14){
    # Take a df of readings and plot usage by day
    if (as.cost) {
        df$measurement <- (df$measurement - min(df$measurement)) * ppu
        df$change <- df$change * ppu
    }
    
    p <- ggplot(df, aes(x = date, y = measurement)) +
        geom_line() + labs(x = "Date", y = "Usage (kWh)")
    
    if (predict > 0) {
        date <- seq.Date(max(df$date), length.out = predict + 1, by = 'day')
        max_prediction <- predict_max_usage(df, predict)
        median_prediction <- predict_median_usage(df, predict)
        min_prediction <- predict_min_usage(df, predict)
        median_7day_prediction <- predict_median_7day_usage(df, predict)
        
        forecast <- data.frame(date, max_prediction, median_prediction, median_7day_prediction, min_prediction)
        names(forecast) <- c("date", "Max", "Median", "Median (7d)", "Min")
        forecast <- gather(forecast, "prediction", "measurement", 2:5)
        p <- p + geom_line(data=forecast, aes(x = date, y = measurement, colour = prediction))
        p <- p + labs(colour = "Rate")
        }
    if (as.cost) {
        sterling <- scales::dollar_format(prefix = "£")
        if (predict > 0) {
            p <- p + ylab("Cost") + scale_y_continuous(labels = sterling, limits = c(0, max(max(df$measurement), max(forecast$measurement))))
        } else {
            p <- p + ylab("Cost") + scale_y_continuous(labels = sterling, limits = c(0, max(max(df$measurement), max(df$measurement))))
        }
    }
    # p <- p + scale_x_date(date_breaks = "14 days", date_labels = "%d-%m-%y") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    # p <- p + scale_x_date(breaks = pretty_dates(df$date, n = 10), date_labels = "%d/%m/%y")
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
        scale_y_continuous(limits=c(-max(abs(df$change_residuals)), max(abs(df$change_residuals)))) + 
        theme(legend.position="none") +
        labs(x = "Date", y = "Usage Residuals (from median)")
    # theme_bw()
    print(p)
}

generate_cal_heatmap_df <- function(df){
    # Take a df including change and return a df with the necessary columns to plot a calendar heatmap
    df <- df[which(!is.na(df$change)),]
    df$yearmonth <- as.yearmon(df$date)
    df$year <- format(df$date, "%Y")
    df$ordermonth <- paste0(format(df$date, "%m"), "_", format(df$date, "%b"))
    df$weekday <- as.integer(format(df$date, "%w"))
    df$weekday[which(df$weekday == 0)] <- 7
    df$week <- as.integer(format(df$date, "%W"))
    df$yearmonthf <- factor(df$yearmonth)
    
    # Generate monthweek column
    df <- df %>% group_by(yearmonthf) %>% mutate(monthweek=1+week-min(week))
    df <- df[, c("year", "ordermonth", "monthweek", "weekday", "change")]
    return(df)
}

plot_cal_heatmap <- function(df){
    # Plot a calendar heatmap of usage from a df generated by generate_cal_heatmap_df()
    
    month_convert <- function(string){
        # Labeller function to deal with the janky way we've had to format ordered dates.
        splitted <- strsplit(string, "_")
        out <- unlist(sapply(splitted, "[", 2))
        return(out)
    }
    
    p <- ggplot(df, aes(monthweek, weekday, fill = change)) + 
        geom_tile(colour = "white") + 
        facet_grid(year~ordermonth, labeller = labeller(ordermonth = month_convert)) + 
        scale_fill_gradient(low="green", high="red") +
        labs(x="Week of Month",
             y="",
             title = "Time-Series Calendar Heatmap", 
             subtitle="Meter Usage", 
             fill="Usage (kWh)") +
        # Reverse y axis for easier interpretation and label back appropriately
        scale_y_reverse(breaks = c(1:7), labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
    print(p)
}

