library(dplyr)
library(tidyr)
library(ggplot2)

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