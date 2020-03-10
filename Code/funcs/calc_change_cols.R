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
